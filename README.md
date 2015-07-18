glyphs
======

A little experiment in reducing verbosity in Common Lisp, inspired by
BODOL (https://github.com/bodil/BODOL - no affiliation).

To try it out in your REPL you can use (ql:quickload :glyphs)
if you have added to your ASDF load path in local projects.

Update: Now that it's in quicklisp, simply:
```lisp
(ql:quickload :glyphs)
```

## Examples

### Factorial example with glyphs function macro:

glyphs:
```lisp
(ƒ factorial
   0 → 1
   α → (* α (factorial (1- α))))
```
vs.:
```lisp
(defun factorial (x)
  (cond ((equal x 0) 1)
        (x (* x (factorial (1- x))))))
```
result:
```lisp
(factorial 8)
40320
```

### Type safety with factorial
```lisp
(ƒ factorial
  0 → 1
  (and (numberp α) (> α 0)) → (* α (factorial (1- α)))
  α → (error "Non-numbers (or negatives) don't work real well with factorials!"))
```
vs.:
```lisp
(defun factorial (x)
  (cond ((equal x 0) 1)
        ((and (numberp x) (> x 0)) (* x (factorial (1- x))))
        (t (error "Non-numbers (or negatives) don't work real well with factorials!"))))
```

### Built in type safety with factorial
```lisp
(ƒ→ factorial (integer → integer)
  0 → 1
  α → (* α (factorial (1- α))))
```

### Reverse a list
glyphs:
```lisp
(ƒ reverse*
  (cdr α) → (append (reverse* (cdr α)) `(,(car α)))
  α → α)
```

### Map with glyphs lambda macro to compare strings and do a side effect
glyphs:

```lisp
(mapcar (λ "cat" → (print "Cats rock")
           "dog" → (print "Dogs do too!")) '("cat" "dog" "mouse"))
```
vs.:
```lisp
(mapcar (lambda (x)
          (cond ((equal x "cat") (print "Cats rock"))
                ((equal x "dog") (print "Dogs do too!")) '("cat" "dog" "mouse"))))
```
result:
```lisp
"Cats rock"
"Dogs do too!"
NIL
```

## Comparison based on passed in conditionals
glyphs:
```lisp
(ƒ double-odds-half-evens
  (oddp α) → (* x 2)
  (evenp α) → (/ x 2))
```
vs.:
```lisp
(defun double-odds-half-evens (x)
  (cond ((oddp x) (* x 2))
        ((evenp x) (/ x 2))))
```
result:
```lisp
(double-odds-half-evens 4)
2
(double-odds-half-evens 3)
6
```

### Using multiple values by including special variables in the statement

You can use multiple values by prefixing a variable in the statement
portion of the code with either a 'α' or an '?'.

glyphs:
```lisp
(mapcar (λ α → (+ α αb αc)) '(1 2 3) '(4 5 6) '(7 8 9))
```
vs.:
```lisp
(mapcar (lambda (a b c) (+ a b c)) '(1 2 3) '(4 5 6) '(7 8 9))
```

result:
```lisp
(12 15 18)
```

## Fast matching based on regex strings

Make sure to use the readtable that comes with it first:
```lisp
(in-readtable glyphs:syntax)
```
Or these little readtable shortcuts will not work.

glyphs:
```lisp
(ƒ any-cats?
  ~"cat"~ → (print "yes!"))
```
vs.:
```lisp
(defun any-cats? (x)
  (when (cl-ppcre:scan "cat" x)
    (print "yes!")))
```
result:
```lisp
(any-cats? "I see some cats")
"yes!"
```

## Easy regex replaces on matching strings
glyphs:
```lisp
(ƒ no-cats
  ~"(were|cat)"~ → |"dog"|)
```
vs.:
```lisp
(defun no-cats (x)
  (let ((regex "(were|cat)"))
       (when (cl-ppcre:scan regex x)
         (cl-ppcre:regex-replace-all regex x "dog"))))
```
result:
```lisp
(no-cats "there were dogs and there were cats")
"there dog dogs and there dog dogs"
```

# Currently used glyphs and bindings for them (more to come)

## Emacs bindings

Add to `.emacs'

```lisp
;; Keybindings for glyphs
(global-set-key (kbd "M-l") (lambda () (interactive) (insert "\u03bb"))) ; λ lambda
(global-set-key (kbd "M-f") (lambda () (interactive) (insert "\u0192"))) ; ƒ function
(global-set-key (kbd "M--") (lambda () (interactive) (insert "\u2192"))) ; → right arrow
(global-set-key (kbd "M-a") (lambda () (interactive) (insert "\u03b1"))) ; α alpha
(global-set-key (kbd "M-y") (lambda () (interactive) (insert "\u03c8"))) ; ψ psi
```

## Vim bindings

Add to `.vimrc'

```vim
" Keybindings for glyphs
:inoremap <A-l> <C-v>u3bb<Space>   ; λ lambda
:inoremap <A-f> <C-v>u192<Space>   ; ƒ function
:inoremap <A--> <C-v>u2192<Space>  ; → right arrow
:inoremap <A-a> <C-v>u03b1<Space>  ; α alpha
:inoremap <A-y> <C-v>u03c8<Space>  ; ψ psi
```

## Mac OS X keybindings

Add to `~/Library/KeyBindings/DefaultKeyBinding.dict'

```
{
"~l" = ("insertText:", "\U03BB"); /* alt + l ~> λ lambda */
"~f" = ("insertText:", "\U0192"); /* alt + f ~> ƒ function */
"~-" = ("insertText:", "\U2192"); /* alt + - ~> → right arrow */
"~a" = ("insertText:", "\U03b1"); /* alt + a ~> α alpha */
"~y" = ("insertText:", "\U03c8"); /* alt + y ~> ψ psi */
}
```

## StumpWM keybindings
```lisp
(defmacro defkeys-top (&rest keys)
  (let ((ks (mapcar #'(lambda (k) (cons 'defkey-top k)) keys)))
    `(progn ,@ks)))

(defcommand xdo-lambda () ()
    (run-shell-command "xdotool type λ"))
(defcommand xdo-fn () ()
    (run-shell-command "xdotool type ƒ"))
(defcommand xdo-alpha () ()
    (run-shell-command "xdotool type α"))
(defcommand xdo-arrow () ()
    (run-shell-command "xdotool type →"))

(defkeys-top
    ("s-l" "xdo-lambda")
    ("s-f" "xdo-fn")
    ("s--" "xdo-arrow")
    ("s-a" "xdo-alpha"))
```
If you're absolutely opposed to non-ascii characters, you can use:
```lisp
λ == /.
ƒ == f
→ == ->
```


# License

See LICENSE.md
