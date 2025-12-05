# Markdown to Micron Converter

A simple Markdown to Micron markup converter for Nomadnet applications.

## Features

Converts common Markdown syntax to Micron:

- ✅ **Headings** (`#` to `>`) - up to 6 levels
- ✅ **Bold** (`**text**` or `__text__`)
- ✅ **Italic** (`*text*` or `_text_`)
- ✅ **Code/Literal** (`` `text` ``)
- ✅ **Links** (`[label](url)`)
- ✅ **Lists** (unordered and numbered)
- ✅ **Blockquotes** (`> text`)
- ✅ **Horizontal rules** (`---`, `***`, `___`)

## Usage

```scheme
(load "src/markdown.scm")

;; Convert a string
(markdown->micron "# Hello **World**")
; => "> Hello `!World`!\n"

;; Convert a file
(markdown-file->micron "docs/example.md")

;; Shortcuts
(md->micron "Some **markdown**")
(md-file->micron "path/to/file.md")
```

## Examples

### From Command Line

```bash
# Convert and display
csi -e '(load "src/markdown.scm") (display (md-file->micron "docs/example-simple.md"))'

# Convert and save
csi -e '(load "src/markdown.scm") (call-with-output-file "output.mu" (lambda (p) (display (md-file->micron "input.md") p)))'
```

### Run Tests

```bash
csi -s docs/test-markdown-converter.scm
```

## Known Limitations

These are intentional design choices for simplicity:

1. **Code blocks with markdown syntax**: If you use markdown syntax inside code blocks, it may be processed
   - Example: `` `**bold**` `` might become `` `=`!bold`!`= ``
   - Workaround: Avoid markdown syntax in code blocks

2. **Complex nested formatting**: Deep nesting may not work perfectly
   - Example: `**bold with *italic* inside**` may have quirks

3. **No block-level code**: Only inline code (backticks) is supported
   - No triple-backtick code blocks

4. **Lists are simple**: No nested lists or complex list formatting

5. **Blockquotes are converted to italic**: Not true blockquote rendering

## Conversion Reference

| Markdown | Micron | Display |
|----------|--------|---------|
| `**bold**` | `` `!bold`! `` | **bold** |
| `*italic*` | `` `*italic`* `` | *italic* |
| `` `code` `` | `` `=code`= `` | `code` |
| `[text](url)` | `` `[text`url] `` | clickable link |
| `# Heading` | `> Heading` | First-level heading |
| `## Heading` | `>> Heading` | Second-level heading |
| `---` | `-` | Horizontal line |
| `- item` | `  • item` | Bullet point |
| `1. item` | `  item` | Numbered item (bullets removed) |
| `> quote` | `` `*quote`* `` | Italic text |

## Philosophy

This converter prioritizes:
- **Simplicity** over completeness
- **Common cases** over edge cases
- **Learning** over production use

It's designed to quickly convert existing markdown content for Nomadnet, not to be a perfect markdown parser.

## See Also

- `src/markdown.scm` - Converter implementation
- `src/micron-dsl.scm` - Micron DSL functions used by converter
- `docs/MICRON_DSL_REFERENCE.md` - Complete micron DSL reference
- `docs/test-markdown-converter.scm` - Test suite
