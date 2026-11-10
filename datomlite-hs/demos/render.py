#!/usr/bin/env python3
"""
Render a README screenshot of a datomlite REPL session.

    render.py demos/Repl.hs    ../pix/datomlite-hs-repl.png   (GHCi, via cabal repl)
    render.py demos/Repl.scala pix/datomlite-repl.png         (Scala 3 REPL)

The session file is a list of REPL entries separated by blank lines. Each
entry is sent to the REPL as one input and what it prints back is drawn under
it, so the picture is always a real run. A first entry starting with
`-- setup` (or `// setup`) is run but not drawn.

GHCi gets each entry as a :{ ... :} block, and LANGUAGE pragmas in the setup
become `:set -X...`. The Scala REPL only shows the result of an entry's last
expression, as a comment.

Needs rsvg-convert and the JetBrains Mono NL font, plus cabal or mill and
scala-cli.
"""
import argparse
import json
import os
import re
import subprocess
import sys
import tempfile
import unicodedata
from html import escape

FONT = 'JetBrains Mono NL'
FONT_SIZE = 14
CELL_W = FONT_SIZE * 0.6
LINE_H = round(FONT_SIZE * 1.3, 2)
PAD_X, PAD_Y = 14, 12
CLOCK = '12:21 08-Nov-26'

# Catppuccin Mocha
BASE = '#1e1e2e'
MANTLE = '#181825'
SURFACE1 = '#45475a'
OVERLAY1 = '#7f849c'
OVERLAY2 = '#9399b2'
TEXT = '#cdd6f4'
BLUE = '#89b4fa'
GREEN = '#a6e3a1'
MAUVE = '#cba6f7'
PEACH = '#fab387'
YELLOW = '#f9e2af'
SKY = '#89dceb'
MAROON = '#eba0ac'
RED = '#f38ba8'

PKG = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SEP = '@@datomlite-repl-sep@@'


# --- reading the session -------------------------------------------------

def entries(src, setup_marker):
    blocks, cur = [], []
    for line in src.splitlines() + ['']:
        if line.strip():
            cur.append(line.rstrip())
        elif cur:
            blocks.append(cur)
            cur = []
    setup = []
    if blocks and blocks[0][0].strip() == setup_marker:
        setup = blocks.pop(0)[1:]
    return setup, blocks


def split_outputs(stdout, setup, blocks, error):
    chunks = stdout.split(SEP + '\n')
    if len(chunks) != len(blocks) + 2:
        sys.exit(stdout)
    for chunk, block in zip(chunks, [setup] + blocks):
        if error.search(chunk):
            sys.exit('REPL error in:\n  ' + '\n  '.join(block) + '\n\n' + chunk)
    return [c.rstrip('\n').splitlines() for c in chunks[1:-1]]


# --- running a GHCi session ----------------------------------------------

def ghci_input(setup, blocks):
    script = [':set prompt ""', ':set prompt-cont ""', ':set -w']
    for line in setup:
        pragma = re.match(r'\{-#\s*LANGUAGE\s+(.*?)\s*#-\}', line)
        if pragma:
            script += [':set -X' + ext.strip() for ext in pragma.group(1).split(',')]
        else:
            script.append(line)
    script.append(f'putStrLn "{SEP}"')
    for block in blocks:
        script += [':{'] + block + [':}', f'putStrLn "{SEP}"']
    return '\n'.join(script) + '\n'


def run_ghci(session, setup, blocks):
    run = subprocess.run(
        ['cabal', 'repl', '-v0', 'datomlite', '--repl-options=-ignore-dot-ghci'],
        cwd=PKG, input=ghci_input(setup, blocks), text=True,
        stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    return split_outputs(run.stdout, setup, blocks, re.compile(r'\berror\b'))


# --- running a Scala 3 REPL session --------------------------------------

# Runs each entry, prints its last resN through __show (or the REPL's own
# output on an error), then SEP.
SCALA_RUNNER = '''\
//> using scala @VERSION@
//> using dep org.scala-lang::scala3-compiler:@VERSION@
import dotty.tools.repl.{ReplDriver, State}
import java.io.{ByteArrayOutputStream, PrintStream}

@main def replRunner(cp: String, sep: String): Unit =
  val buf = ByteArrayOutputStream()
  val out = PrintStream(buf, true, "UTF-8")
  val driver = ReplDriver(Array("-classpath", cp, "-color:never", "-nowarn"), out)
  var state: State = driver.initialState
  def eval(code: String): String =
    buf.reset()
    Console.withOut(out) { state = driver.run(code)(using state) }
    buf.toString("UTF-8")
  val Res = """val (res\\d+): .*""".r
  val input = scala.io.Source.stdin.mkString.stripSuffix(sep + "\\n")
  for entry <- input.split(sep + "\\n", -1) do
    val raw = if entry.trim.isEmpty then "" else eval(entry)
    val failed = raw.linesIterator.exists(l => l.startsWith("-- ") && l.contains("Error"))
      || raw.contains("Exception")
    if failed then print(raw)
    else raw.linesIterator.toSeq.lastOption.collect { case Res(name) => name }
      .foreach(name => print(eval(s"println(__show($name))")))
    println(sep)
'''

# Like the REPL's printer, but with strings quoted.
SCALA_SHOW = r'''def __show(x: Any): String =
  def args(xs: IterableOnce[Any]) = xs.iterator.map(__show).mkString("(", ", ", ")")
  x match
    case s: String => "\"" + s + "\""
    case m: collection.Map[?, ?] =>
      m.toString.takeWhile(_ != '(') + m.map((k, v) => s"${__show(k)} -> ${__show(v)}").mkString("(", ", ", ")")
    case it: Iterable[?] => it.toString.takeWhile(_ != '(') + args(it)
    case p: Product if p.productPrefix.startsWith("Tuple") => args(p.productIterator)
    case p: Product if p.productArity > 0 => p.productPrefix + args(p.productIterator)
    case o => String.valueOf(o)
'''


def mill_root(session):
    d = os.path.dirname(os.path.abspath(session))
    while not os.path.exists(os.path.join(d, 'build.mill')):
        if d == os.path.dirname(d):
            sys.exit(f'no build.mill above {session}')
        d = os.path.dirname(d)
    return d


def run_scala(session, setup, blocks):
    root = mill_root(session)
    with open(os.path.join(root, 'build.mill')) as f:
        version = re.search(r'scalaVersions\s*=\s*Seq\("([^"]+)"', f.read()).group(1)
    shown = subprocess.run(['./mill', '--ticker', 'false', 'show', f'datomlite.jvm[{version}].runClasspath'],
                           cwd=root, text=True, stdout=subprocess.PIPE, check=True)
    cp = ':'.join(re.sub(r'^q?ref:v\d+:[0-9a-f]+:', '', p) for p in json.loads(shown.stdout))

    setup = SCALA_SHOW.splitlines() + setup
    stdin = ''.join('\n'.join(e) + '\n' + SEP + '\n' for e in [setup] + blocks)
    with tempfile.TemporaryDirectory() as tmp:
        runner = os.path.join(tmp, 'ReplRunner.scala')
        with open(runner, 'w') as f:
            f.write(SCALA_RUNNER.replace('@VERSION@', version))
        run = subprocess.run(['scala-cli', 'run', '--server=false', '-q', runner, '--', cp, SEP],
                             cwd=tmp, input=stdin, text=True,
                             stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if run.returncode:
        sys.exit(run.stderr + run.stdout)
    return split_outputs(run.stdout, setup, blocks, re.compile(r'^-- .*Error|Exception\b', re.M))


# --- languages -----------------------------------------------------------

HASKELL = dict(
    window='ghci', ps1='λ> ', ps2=' | ', setup='-- setup', run=run_ghci,
    keywords={'import', 'let', 'in', 'if', 'then', 'else', 'where', 'do', 'case', 'of',
              'data', 'instance', 'type', 'deriving'},
    consts={'Just', 'Nothing', 'Left', 'Right', 'True', 'False'},
    params={'b', 'f', 'l', 'w'},
    token=re.compile(
        r'(?P<com>--.*|\{-.*?-\})'
        r'|(?P<str>"[^"]*")'
        r'|(?P<num>\b\d+(\.\d+)?\b)'
        r"|(?P<id>[A-Za-z_][\w']*)"
        r'|(?P<op><\$>|<\|>|<-|->|=>|==|/=|=<<|\$|[=+*\-<>|\\@])'
        r'|(?P<punc>[()\[\]{},.;:\'])'
        r'|(?P<plain>.)'),
    # a name applied to an argument: `f x`, `f "x"`, `f (x)`
    applied=re.compile(r' +["(\[A-Za-z0-9$@]'),
    not_applied=re.compile(r' +(in|then|else|of)\b'),
    dot_is_field=True,
    bind=re.compile(r"^(?:Right\s+)?([a-z][\w']*)\s*=[^=]"),
    result_comment=None,
)

SCALA = dict(
    window='scala', ps1='scala> ', ps2='     | ', setup='// setup', run=run_scala,
    keywords={'import', 'val', 'var', 'def', 'case', 'class', 'object', 'trait', 'derives',
              'extends', 'given', 'using', 'for', 'if', 'then', 'else', 'yield', 'do',
              'match', 'new', 'type', 'lazy'},
    consts={'Some', 'None', 'Nil', 'Left', 'Right', 'true', 'false'},
    params={'b', 'w', 'l', 'm', '_'},
    token=re.compile(
        r'(?P<com>//.*|/\*.*?\*/)'
        r'|(?P<str>"[^"]*")'
        r'|(?P<num>\b\d+(\.\d+)?L?\b)'
        r'|(?P<op>@\w+|<-|=>|===|==|!=|->|[=+*\-<>|&!])'
        r'|(?P<id>[A-Za-z_]\w*)'
        r'|(?P<punc>[()\[\]{},.;:])'
        r'|(?P<plain>.)'),
    # a method call: `f(x)`, `f[A]`, `f { x => ... }`
    applied=re.compile(r'\(|\[| \{'),
    not_applied=None,
    dot_is_field=False,
    bind=re.compile(r'^val\s+(\w+)\s*='),
    result_comment='// ',
)

LANGS = {'.hs': HASKELL, '.scala': SCALA}


# --- drawing -------------------------------------------------------------

SGR = re.compile(r'\x1b\[([0-9;]*)m')
RESET = '\x1b[0m'


def char_width(ch):
    if unicodedata.combining(ch):
        return 0
    if unicodedata.east_asian_width(ch) in 'WF' or ord(ch) >= 0x1F300:
        return 2
    return 1


def width(s):
    return sum(char_width(c) for c in SGR.sub('', s))


def esc(fg=None, bg=None, bold=False, italic=False):
    def rgb(c):
        return ';'.join(str(int(c[i:i + 2], 16)) for i in (1, 3, 5))

    codes = ['0']
    if bold:
        codes.append('1')
    if italic:
        codes.append('3')
    if fg:
        codes.append('38;2;' + rgb(fg))
    if bg:
        codes.append('48;2;' + rgb(bg))
    return f'\x1b[{";".join(codes)}m'


def pad(s, n):
    return s + ' ' * (n - width(s)) + RESET


STYLES = {
    'kw': (MAUVE, False), 'str': (GREEN, False), 'com': (OVERLAY1, True),
    'num': (PEACH, False), 'const': (PEACH, False), 'fn': (BLUE, False),
    'typ': (YELLOW, False), 'op': (SKY, False), 'punc': (OVERLAY2, False),
    'param': (MAROON, False), 'plain': (TEXT, False),
}


def classify(lang, line, m, values):
    s = m.group(0)
    if s in lang['keywords']:
        return 'kw'
    if s in lang['consts']:
        return 'const'
    if s[0].isupper():
        return 'typ'
    if s in lang['params']:
        return 'param'
    if s in values:
        return 'plain'  # a value bound earlier in the session, not a function
    if lang['dot_is_field'] and m.start() > 0 and line[m.start() - 1] == '.':
        return 'plain'  # a record field, p.name
    rest = line[m.end():]
    if lang['applied'].match(rest) and not (lang['not_applied'] and lang['not_applied'].match(rest)):
        return 'fn'
    return 'plain'


def bound_values(lang, blocks):
    """Names the session binds without arguments: `db = ...`, `val db = ...`"""
    return {m.group(1) for b in blocks for l in b for m in [lang['bind'].match(l)] if m}


def highlight(lang, line, values=()):
    out = []
    for m in lang['token'].finditer(line):
        kind = m.lastgroup
        if kind == 'id':
            kind = classify(lang, line, m, values)
        color, italic = STYLES[kind]
        out.append(esc(color, italic=italic) + m.group(0))
    return ''.join(out) + RESET


def highlight_output(lang, line):
    if lang['result_comment']:
        indent = ' ' * len(lang['ps1'])
        return indent + esc(OVERLAY1, italic=True) + lang['result_comment'] + highlight(lang, line)
    return highlight(lang, line)


def tmux_bar(lang, cols):
    session, windows, clock = ' datomlite ', f' 1:{lang["window"]}* ', f' {CLOCK} '
    gap = cols - len(session) - len(windows) - len(clock)
    return (esc(BASE, GREEN, True) + session + esc(TEXT, MANTLE) + windows
            + esc(bg=MANTLE) + ' ' * gap + esc(BASE, GREEN) + clock + RESET)


def repl_screen(lang, blocks, outputs):
    ps1 = esc(BLUE, bold=True) + lang['ps1'] + RESET
    ps2 = esc(SURFACE1) + lang['ps2'] + RESET
    values = bound_values(lang, blocks)
    lines = ['']
    for block, output in zip(blocks, outputs):
        lines += ['  ' + (ps2 if i else ps1) + highlight(lang, l, values) for i, l in enumerate(block)]
        lines += ['  ' + highlight_output(lang, l) for l in output]
        lines.append('')
    lines += ['  ' + ps1 + esc(BASE, TEXT) + ' ' + RESET, '']
    cols = max(width(l) for l in lines) + 4
    return [pad(l, cols) for l in lines] + [tmux_bar(lang, cols)]


def params(match):
    return [int(p) if p else 0 for p in match.group(1).split(';')] if match.group(1) else [0]


def cells(line):
    st = dict(fg=None, bg=None, bold=False, italic=False)
    out, pos = [], 0
    for m in list(SGR.finditer(line)) + [None]:
        end = m.start() if m else len(line)
        for ch in line[pos:end]:
            out.append((ch, char_width(ch), st['fg'], st['bg'], st['bold'], st['italic']))
        if not m:
            break
        pos = m.end()
        ps = params(m)
        i = 0
        while i < len(ps):
            p = ps[i]
            if p == 0:
                st.update(fg=None, bg=None, bold=False, italic=False)
            elif p == 1:
                st['bold'] = True
            elif p == 3:
                st['italic'] = True
            elif p in (38, 48) and ps[i + 1] == 2:
                st['fg' if p == 38 else 'bg'] = '#%02x%02x%02x' % tuple(ps[i + 2:i + 5])
                i += 4
            i += 1
    return out


def svg(screen):
    rows = [cells(l) for l in screen]
    cols = max(sum(c[1] for c in row) for row in rows)
    w, h = PAD_X * 2 + cols * CELL_W, PAD_Y * 2 + len(rows) * LINE_H
    out = []
    for n, row in enumerate(rows):
        y = PAD_Y + n * LINE_H
        runs, col = [], 0
        for ch, cw, fg, bg, bold, italic in row:
            if cw == 0:
                continue
            style = (fg, bg, bold, italic)
            if runs and runs[-1][0] == style:
                runs[-1][2].append((ch, col))
            else:
                runs.append([style, col, [(ch, col)]])
            col += cw

        for (fg, bg, bold, italic), start, chars in runs:
            if bg:
                last, last_col = chars[-1]
                span = last_col + char_width(last) - start
                out.append(f'<rect x="{PAD_X + start * CELL_W:.2f}" y="{y:.2f}" '
                           f'width="{span * CELL_W + 0.5:.2f}" height="{LINE_H + 0.5:.2f}" fill="{bg}"/>')
            text = [(c, k) for c, k in chars if c != ' ']
            if not text:
                continue
            attrs = f'fill="{fg or TEXT}"'
            if bold:
                attrs += ' font-weight="bold"'
            if italic:
                attrs += ' font-style="italic"'
            ty = f'{y + LINE_H * 0.76:.2f}'
            glyphs = ''.join(f'<text x="{PAD_X + k * CELL_W:.2f}" y="{ty}">{escape(c)}</text>' for c, k in text)
            out.append(f'<g {attrs}>{glyphs}</g>')

    body = '\n'.join(out)
    return (f'<svg xmlns="http://www.w3.org/2000/svg" width="{w:.0f}" height="{h:.0f}" '
            f'viewBox="0 0 {w:.2f} {h:.2f}">\n'
            f'<rect width="{w:.2f}" height="{h:.2f}" fill="{BASE}"/>\n'
            f'<g font-family="{FONT}" font-size="{FONT_SIZE}" xml:space="preserve">\n{body}\n</g></svg>\n')


def main():
    ap = argparse.ArgumentParser(description='Render a datomlite REPL session to PNG.')
    ap.add_argument('session', help='a .hs (GHCi) or .scala (Scala 3 REPL) session file')
    ap.add_argument('png')
    args = ap.parse_args()

    lang = LANGS.get(os.path.splitext(args.session)[1])
    if not lang:
        sys.exit(f'unknown session type: {args.session} (want .hs or .scala)')
    with open(args.session) as f:
        setup, blocks = entries(f.read(), lang['setup'])
    outputs = lang['run'](args.session, setup, blocks)
    subprocess.run(['rsvg-convert', '-z', '2', '-o', os.path.abspath(args.png)],
                   input=svg(repl_screen(lang, blocks, outputs)).encode(), check=True)
    print(f'wrote {args.png}')


if __name__ == '__main__':
    main()
