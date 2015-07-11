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

;;;; glyphs.lisp

(in-package #:glyphs)

;;; "glyphs" goes here. Hacks and glory await!

(defparameter *ψ* "")

(defun gscan (stream char)
  (declare (ignore char)) ;; Needed if manually setting end char
  (let ((replace-list (read-delimited-list #\~ stream t)))
    `(progn
       (setf *ψ* ,(car replace-list))
       (cl-ppcre:scan *ψ* α))))

(defun greplace (stream char)
  (declare (ignore char))
  (let ((replace-list (read-delimited-list #\| stream t)))
    `(progn
       (cl-ppcre:regex-replace-all *ψ* α ,(car replace-list)))))

(defreadtable glyphs:syntax
  (:fuze :standard)
  (:macro-char #\~ #'gscan)
  (:macro-char #\| #'greplace))

(defun flatten (structure)
  (cond ((null structure) nil)
        ((atom structure) (list structure))
        (t (mapcan #'flatten structure))))

(defmacro arg-walker (&rest args)
  `(setf ,@(loop for arg in args
              for i from 0
              collect arg
              collect `(nth ,i list-match))))

(defmacro pm (name args &rest rest)
  `(defun ,name ,args
     (let ((list-match ,(car args)))
       ,@rest)))

(defmacro defn (name types args &rest rest)
  "Type safe defun"
  (let ((types (remove-if
                (lambda (x) (or (equal '-> x) (equal '→ x))) types)))
    `(progn (defun ,name ,args
              ,@(loop for arg in (cdr args) for type in types
                     collect `(check-type ,arg ,type))
              ,@rest)
            (declaim (ftype (function ,(butlast types) ,@(last types)) ,name)))))

(defmacro ƒ→ (name types &rest rest)
  "Similar to defun, requires using α as the default case.
Includes the type safety of defn."
  `(defn ,name ,types (&optional glyphs:α
                   ,@(remove-duplicates
                      (loop for arg in (flatten rest)
                         when (and (symbolp arg)
                                   (or (string= arg '? :end1 1)
                                       (string= arg 'α :end1 1))
                                   (> (length (string arg)) 1))
                         collect arg)))
     (let ((glyphs:α (or glyphs:α t)))
       (cond
	 ,@(loop for arg in rest
	      for iter from 0
	      when (and (symbolp arg)
                        (or (string= '→ arg)
                            (string= '-> arg)))
	      collect `(,(if (consp (nth (1- iter) rest))
			     `,(nth (1- iter) rest)
                             `(equal glyphs:α ,(nth (1- iter) rest)))
                         ,(nth (1+ iter) rest)))))))

(defmacro ƒ (name &rest rest)
  "Similar to defun, requires using x as the default case"
  `(defun ,name (&optional glyphs:α
                   ,@(remove-duplicates
                      (loop for arg in (flatten rest)
                         when (and (symbolp arg)
                                   (or (string= arg '? :end1 1)
                                       (string= arg 'α :end1 1))
                                   (> (length (string arg)) 1))
                         collect arg)))
     (let ((glyphs:α (or glyphs:α t)))
       (cond
	 ,@(loop for arg in rest
	      for iter from 0
	      when (and (symbolp arg)
                        (or (string= '→ arg)
                            (string= '-> arg)))
	      collect `(,(if (consp (nth (1- iter) rest))
			     `,(nth (1- iter) rest)
                             `(equal glyphs:α ,(nth (1- iter) rest)))
                         ,(nth (1+ iter) rest)))))))

(defmacro f (&rest rest)
  "Good old ascii alternate for the ƒ macro"
  `(ƒ ,@rest))

(parenscript:defmacro+ps ƒƒ (name &rest rest)
  "PS - Similar to defun, requires using x as the default case"
  `(defun ,name (&optional glyphs:α
                   ,@(remove-duplicates
                      (loop for arg in (flatten rest)
                         when (and (symbolp arg)
                                   (or (string= arg '? :end1 1)
                                       (string= arg 'α :end1 1))
                                   (> (length (string arg)) 1))
                         collect arg)))
     (cond
       ,@(loop for arg in rest
	    for iter from 0
	    when (and (symbolp arg)
                      (or (string= '→ arg)
                          (string= '-> arg)))
	    collect `(,(if (consp (nth (1- iter) rest))
			   `,(nth (1- iter) rest)
			   `(equal glyphs:α ,(nth (1- iter) rest)))
		       ,(nth (1+ iter) rest))))))

(parenscript:defmacro+ps ff (&rest rest)
  `(ƒƒ ,@rest))

(defmacro λ (&rest rest)
  "Similar to lambda, requires using x as the default case"
  `(lambda (&optional glyphs:α
              ,@(remove-duplicates
                 (loop for arg in (flatten rest)
                    when (and (symbolp arg)
                              (or (string= arg '? :end1 1)
                                  (string= arg 'α :end1 1))
                              (> (length (string arg)) 1))
                    collect arg)))
     (cond
       ,@(loop for arg in rest
	    for iter from 0
	    when (and (symbolp arg)
                      (or (string= '→ arg)
                          (string= '-> arg)))
	    collect `(,(if (consp (nth (1- iter) rest))
			   `,(nth (1- iter) rest)
			   `(equal glyphs:α ,(nth (1- iter) rest)))
		       ,(nth (1+ iter) rest))))))

(defmacro /. (&rest rest)
  `(λ ,@rest))

(parenscript:defmacro+ps λλ (&rest rest)
  "PS - Similar to lambda, requires using x as the default case"
  `(lambda (&optional glyphs:α
              ,@(remove-duplicates
                 (loop for arg in (flatten rest)
                    when (and (symbolp arg)
                              (or (string= arg '? :end1 1)
                                  (string= arg 'α :end1 1))
                              (> (length (string arg)) 1))
                    collect arg)))
     (cond
       ,@(loop for arg in rest
            for iter from 0
            when (and (symbolp arg)
                      (or (string= '→ arg)
                          (string= '-> arg)))
            collect `(,(if (consp (nth (1- iter) rest))
                           `,(nth (1- iter) rest)
                           `(equal glyphs:α ,(nth (1- iter) rest)))
                       ,(nth (1+ iter) rest))))))

(parenscript:defmacro+ps /./. (&rest rest)
  `(λλ ,@rest))

(defmacro ψ (data &rest rest)
  "Shortcut for a mapcar with the short lambda stuff"
  `(mapcar (λ ,@rest) ,data))

(defmacro ± (a b)
  `(loop for x from ,(- a b) to ,(+ a b) collect x))
