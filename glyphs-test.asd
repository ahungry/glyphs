;; Glyphs - Reducing verbosity in Common Lisp
;; Copyright (C) 2013 Matthew Carter
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; glyphs.asd

(asdf:defsystem #:glyphs-test
  :serial t
  :description "Tests for Glyphs to reduce Common Lisp verbosity"
  :author "Matthew Carter <m@ahungry.com>"
  :license "GPLv3"
  :depends-on (#:glyphs
               #:stefil)
  :components ((:file "tests/package")
               (:file "tests/tests")))
