#!/usr/bin/env sh
# Make graphviz SVGs adapt to light/dark mode
for f in "$@"; do
  grep -q 'prefers-color-scheme' "$f" && continue
  sed -i \
    -e '0,/<polygon fill="white" stroke="none"/s//<polygon fill="none" stroke="none"/' \
    -e '0,/<g id="graph0"/s//<style>\
@media (prefers-color-scheme: dark) {\
  [stroke="black"] { stroke: #e6edf3; }\
  [fill="black"], text:not([fill]) { fill: #e6edf3; }\
  [stroke="red"] { stroke: #ff7b72; }\
  [fill="red"] { fill: #ff7b72; }\
  [fill="#888888"] { fill: #9198a1; }\
  [fill="#e6ffe6"] { fill: #12361f; }\
  [fill="#ffe0e0"] { fill: #4b1d1f; }\
  [fill="#f2f2f2"] { fill: #262c36; }\
}\
<\/style>\
<g id="graph0"/' "$f"
done
