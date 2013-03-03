;;; epack.el --- cl-like packages for emacs

;; Copyright (C) 2013  Yann Hodique

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Keywords: lisp

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This code is experimental, and is not meant to be used in any significant
;; piece of software for now.

;;; Code:

(eval-when-compile
  (require 'cl))

(defstruct (epack-package (:type list) :named)
  used
  symbols
  export)

(defvar epack-packages nil)

(defun epack-string-id (name)
  (or (and (stringp name) name)
      (and (symbolp name) (symbol-name name))))

(defmacro epack-defpackage (name &rest specs)
  (declare (indent 1))
  (let ((name (epack-string-id name)))
    `(epack--defpackage ,name ',specs)))

(defun epack--defpackage (name specs)
  (let* ((package (make-epack-package))
         (used (cdr (assoc :use specs)))
         (export (cdr (assoc :export specs))))
    (setf (epack-package-used package)
          (mapcar 'epack-string-id used))
    (setf (epack-package-export package)
          (mapcar 'epack-string-id export))
    (setf (epack-package-symbols package)
          (make-vector 17 nil))
    (let ((pack (assoc name epack-packages)))
      (if pack
          (setcdr pack package)
        (push (cons name package) epack-packages))
      package)))

(defun epack-find-symbol--deps (name deps)
  (and deps
       (or
        (let* ((dep (car deps))
               (pack (cdr (assoc dep epack-packages))))
          (and pack
               (member name (epack-package-export pack))
               (intern-soft name (epack-package-symbols pack))))
        (epack-find-symbol--deps name (cdr deps)))))

(defun epack-find-symbol (name pack)
  (and pack
       (or (epack-find-symbol--deps name (epack-package-used pack))
           (intern-soft name (epack-package-symbols pack)))))

(defun epack-intern (name pack)
  (or (epack-find-symbol name pack)
      (intern name (epack-package-symbols pack))))

(defun epack-resolve-symbol (sym default-package)
  (let ((name (symbol-name sym)))
    (cond ((string-match-p "^:.*" name)
           sym)
          ((and (string-match "^\\(.*\\):\\(.*\\)" name)
                (assoc (match-string 1 name) epack-packages)
                (epack-find-symbol (match-string 2 name)
                                   (cdr (assoc (match-string 1 name)
                                               epack-packages)))))
          (t (epack-intern name default-package)))))

(defun epack--in-package (packname forms)
  (let* ((name (epack-string-id packname))
         (pack (cdr (assoc name epack-packages))))
    (mapcar (lambda (form)
              (cond ((null form) nil)
                    ((symbolp form)
                     (epack-resolve-symbol form pack))
                    ((listp form)
                     (epack--in-package name form))
                    (t form)))
            forms)))

(defmacro epack-in-package (packname &rest body)
  (declare (indent 1))
  (cons 'progn
        (epack--in-package packname `,body)))

(provide 'epack)

;;; initialize the "emacs" package, with all builtins
(let ((emacs (epack-defpackage emacs)))
  (setf (epack-package-symbols emacs) obarray)
  (setf (epack-package-export emacs)
        (let ((subrs nil))
          (mapatoms (lambda (s)
                      (when (and (fboundp s) (subrp (symbol-function s)))
                        (setq subrs (cons (symbol-name s) subrs)))))
          subrs)))

;; (epack-defpackage test
;;   (:use emacs)
;;   (:export "plop"))

;; (epack-in-package test
;;   (defun plop () 42))

;; (epack-defpackage test2)

;; (epack-in-package test2
;;   (emacs:defun plop () 0))

;; (epack-in-package test
;;   (plop))

;; (epack-in-package test2
;;   (plop))

;; (epack-defpackage test3
;;   (:use test))

;; (epack-in-package test3
;;   (plop))

;;; epack.el ends here
