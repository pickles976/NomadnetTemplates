# Micron DSL Reference

Complete reference for the micron-dsl.scm functions.

## Text Formatting

```scheme
(bold "text")              ; `!text`!
(italics "text")           ; `*text`*
(underline "text")         ; `_text`_
(bold-italic "text")       ; `!`*text`*`!
(bold-underline "text")    ; `!`_text`_`!
```

## Colors

### Basic Colors

```scheme
(fg "f00" "red text")             ; `Ff00red text
(bg "0f0" "green background")     ; `B0f0green background
(colored "fff" "000" "white on black")  ; `Ffff`B000white on black
(reset-colors)                     ; `f`b
```

### Color Presets

Available as constants:
- `color-red`, `color-green`, `color-blue`
- `color-yellow`, `color-cyan`, `color-magenta`
- `color-white`, `color-black`, `color-gray`
- `color-dark-gray`, `color-light-gray`
- `color-success`, `color-warning`, `color-error`, `color-info`

```scheme
(fg color-red "error")
(bg color-dark-gray "subtle")
```

### Semantic Helpers

```scheme
(success-text "Saved!")     ; Green text
(error-text "Failed!")      ; Red text
(warning-text "Warning!")   ; Orange text
(info-text "Note:")         ; Blue text
(muted-text "secondary")    ; Gray text
```

## Alignment

```scheme
(left "text")          ; `atext
(center "text")        ; `ctext
(right "text")         ; `rtext
(align-default)        ; `a - reset to default
```

## Sections & Structure

```scheme
(section "Title")           ; > Title
(subsection "Subtitle")     ; >> Subtitle
(subsubsection "Sub-sub")   ; >>> Sub-sub
(section-depth 4 "Deep")    ; >>>> Deep

(divider)                   ; -
```

## Links

```scheme
(link "url")                    ; `[url`url]
(link "url" "label")            ; `[label`url]
(link-button "url" "Click me")  ; Styled link button
```

## Literal/Code Blocks

```scheme
(literal "raw text")   ; `=raw text`=
(code "(+ 1 2)")       ; `=(+ 1 2)`= - alias for literal
```

## Comments

```scheme
(comment "not displayed")  ; # not displayed
```

## Form Inputs

### Text Inputs

```scheme
(input-field "name")              ; `<name`>
(input-field-fixed "email" 32)    ; `<32|email`>
(input-password "pwd" 20)         ; `<20|pwd|*`>
```

### Checkboxes & Radio Buttons

```scheme
(checkbox "agree")                    ; `{agree`} agree
(checkbox "terms" "I agree")          ; `{terms`} I agree

(radio "size" "small")                ; `(size=small`) small
(radio "size" "large" "Large size")   ; `(size=large`) Large size
```

### Submit Button

```scheme
(submit-field "Submit" "/action.scm" "page-id" "field1" "field2")
; `[Submit`:/action.scm`field1|field2|post_id=page-id]
```

## Style Combinations

```scheme
(style '(fg "33f" bg "000" align center) "styled text")
; Valid keys: fg, bg, align (left/center/right/default)
```

## Reset Functions

```scheme
(reset-style)    ; ``
(reset-colors)   ; `f`b
(reset-all)      ; ```f`b`a - reset everything
```

## Utilities

```scheme
(spacer 3)                  ; Insert 3 blank lines
(indent 2 "indented")       ; Indent by 2 levels (4 spaces)
(heading "Big Title")       ; Centered, bold heading
```

## Constants

```scheme
squiggle     ; "-∿"
newline      ; "  \n"
nl           ; Same as newline
hr           ; Same as divider
```

## Usage Examples

### Simple Page

```scheme
(load "src/micron-dsl.scm")

(print
  squiggle nl
  (heading "My Page") nl
  squiggle nl nl

  (section "Welcome")
  (left)
  "This is my page content." nl
  nl

  (success-text "Page loaded successfully!"))
```

### Form Page

```scheme
(section "Contact Form")
(left)

(input-field-fixed "name" 32) " Your Name" nl
(input-field-fixed "email" 40) " Email" nl
nl

(checkbox "newsletter" "Subscribe to newsletter") nl
nl

(submit-field "Send" "/contact" "contact-page" "name" "email" "newsletter")
```

### Styled Content

```scheme
(style '(fg "fff" bg "300" align center)
  (bold "Important Notice"))
nl

(colored color-warning color-black "Warning: ")
"Please read carefully."
```

## Tips

1. **Variadic functions**: Most functions accept multiple content arguments
   ```scheme
   (bold "Hello " "World")  ; `!Hello World`!
   ```

2. **Composability**: Nest functions for complex formatting
   ```scheme
   (center (bold (fg color-red "ERROR")))
   ```

3. **Constants**: Use color constants for consistency
   ```scheme
   (fg color-error "...")  ; Better than (fg "f33" "...")
   ```

4. **Reset often**: Reset styles between sections to avoid carryover
   ```scheme
   (section "New Section")
   (reset-all)
   ```

5. **Use style for multiple attributes**: More concise than separate calls
   ```scheme
   (style '(fg "0af" bg "333" align center) "text")
   ; vs
   (center (bg "333" (fg "0af" "text")))
   ```

## Complete Demo

See `docs/micron-dsl-demo.scm` for a comprehensive example showing all features.

Run it with:
```bash
csi -s docs/micron-dsl-demo.scm > demo.mu
```

Then view `demo.mu` in Nomadnet!
