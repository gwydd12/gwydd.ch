#!/usr/bin/env python3
"""
Persistent pygments highlighting server.

Protocol (all lengths are counts of *characters*, not bytes, so they line up
with Haskell's `length :: String -> Int` on the other end):

  Request  (from Haskell):
      <lang>\n
      <N>\n
      <N characters of source code, no trailing newline required>

  Response (to Haskell):
      <M>\n
      <M characters of highlighted HTML>

Kept alive for the whole Hakyll build, so we pay pygments' lexer/formatter
setup cost once instead of once per code block.
"""
import sys
from pygments import highlight
from pygments.lexers import get_lexer_by_name
from pygments.formatters import HtmlFormatter

def main() -> None:
    while True:
        lang_line = sys.stdin.readline()
        if not lang_line:
            break  # stdin closed, Haskell side is done with us

        lang = lang_line.rstrip("\n")
        length_line = sys.stdin.readline()
        if not length_line:
            break
        n = int(length_line.rstrip("\n"))
        code = sys.stdin.read(n)

        try:
            lexer = get_lexer_by_name(lang)
        except Exception:
            # Unknown language: fall back to plain text rather than crashing
            # the whole build.
            from pygments.lexers.special import TextLexer
            lexer = TextLexer()

        html = highlight(
            code,
            lexer,
            HtmlFormatter(cssclass=f"highlight-{lang}", cssstyles="padding-left: 1em;"),
        )

        sys.stdout.write(f"{len(html)}\n")
        sys.stdout.write(html)
        sys.stdout.flush()

if __name__ == "__main__":
    main()
