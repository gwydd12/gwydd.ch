{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Text.Pandoc.Options
-- Pygments integration
import           Control.Concurrent.MVar
import           Control.Monad          (replicateM)
import           Data.Maybe             (listToMaybe)
import qualified Data.Text as T
import           System.IO
import           System.IO.Unsafe       (unsafePerformIO)
import           System.Process
import           Text.Pandoc.Definition (Block (CodeBlock, RawBlock), Pandoc)
import           Text.Pandoc.SideNoteHTML (usingSideNotesHTML)
import           Text.Pandoc.Walk       (walkM)

root :: String
root = "https://gwydd.ch"

pandocMathCompiler =
    let mathExtensions    = extensionsFromList [Ext_tex_math_dollars, Ext_tex_math_double_backslash, Ext_latex_macros]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions     = defaultExtensions <> mathExtensions
        writerOptions     = defaultHakyllWriterOptions {
                              writerExtensions = newExtensions,
                              writerHTMLMathMethod = MathJax ""
                            }
    in pandocCompilerWithTransformM
           defaultHakyllReaderOptions
           writerOptions
           (pygmentsHighlight . usingSideNotesHTML writerOptions)

--------------------------------------------------------------------------------
-- A persistent `pygments-server.py` process (see scripts/pygments-server.py),
-- started lazily on first use and reused for the rest of the build. This
-- replaces spawning a fresh `pygmentize` process per code block, mirroring
-- how the KaTeX integration (`hlKaTeX`) keeps one long-lived `node` process
-- around instead of re-launching it for every formula.
pygmentsHandles :: MVar (Handle, Handle)
pygmentsHandles = unsafePerformIO $ do
    (Just hin, Just hout, _, _) <-
        createProcess (proc "python3" ["scripts/pygments-server.py"])
            { std_in  = CreatePipe
            , std_out = CreatePipe
            }
    mapM_ (`hSetEncoding` utf8) [hin, hout]
    mapM_ (`hSetBuffering` NoBuffering) [hin, hout]
    newMVar (hin, hout)
{-# NOINLINE pygmentsHandles #-}

pygmentsHighlight :: Pandoc -> Compiler Pandoc
pygmentsHighlight = walkM $ \case
    CodeBlock (_, (T.unpack -> lang) : _, _) (T.unpack -> body) ->
      RawBlock "html" . T.pack <$> unsafeCompiler (callPygs lang body)
    block -> pure block
  where
    callPygs :: String -> String -> IO String
    callPygs lang body = withMVar pygmentsHandles $ \(hin, hout) -> do
        hPutStrLn hin lang
        hPutStrLn hin (show (length body))
        hPutStr   hin body
        hFlush    hin
        n <- read <$> hGetLine hout
        replicateM n (hGetChar hout)
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith defaultConfiguration {destinationDirectory = "docs"} $ do

    match "templates/*" $ compile templateBodyCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- Self-hosted webfonts (EB Garamond is pulled from Google Fonts via CSS
    -- @import instead, so it doesn't need a route; Discipuli Britannica has
    -- no CDN, so its .ttf files live here).
    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "robots.txt" $ do
        route idRoute
        compile copyFileCompiler

    match "articles/*" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/article.html"    articleCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" articleCtx
            >>= relativizeUrls

    match "notes/*" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/note.html"  noteCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" noteCtx
            >>= relativizeUrls

    create ["articles.html"] $ do
        route idRoute
        compile $ do
            articles <- recentFirst =<< loadAll "articles/*"
            let articlesCtx =
                    listField "articles" articleCtx (return articles) `mappend`
                    constField "title" "Articles"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/articles.html" articlesCtx
                >>= loadAndApplyTemplate "templates/default.html" articlesCtx
                >>= relativizeUrls

    create ["notes.html"] $ do
        route idRoute
        compile $ do
            notes <- recentFirst =<< loadAll "notes/*"
            let notesCtx =
                    listField "notes" noteCtx (return notes) `mappend`
                    constField "title" "Notes"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/notes.html" notesCtx
                >>= loadAndApplyTemplate "templates/default.html" notesCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            articles <- recentFirst =<< loadAll "articles/*"
            let indexCtx =
                    listField "articles" articleCtx (return articles) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            articles <- recentFirst =<< loadAllSnapshots "articles/*" "content"
            let feedCtx = articleCtx `mappend` bodyField "description"
            renderAtom feedConfig feedCtx articles


--------------------------------------------------------------------------------
feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = "Gwydd's Blog"
    , feedDescription = "Latest articles from Gwydd's blog"
    , feedAuthorName  = "Gwydd"
    , feedAuthorEmail = "me@gwydd.ch"
    , feedRoot        = root
    }
--------------------------------------------------------------------------------
articleCtx :: Context String
articleCtx =
    constField "root" root      <>
    dateField "date" "%b %d"    <>
    defaultContext
--------------------------------------------------------------------------------
noteCtx :: Context String
noteCtx =
    constField "root" root      <>
    dateField "date" "%b %d"    <>
    defaultContext
--------------------------------------------------------------------------------
