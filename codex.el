;;; codex.el --- cl-like namespaces for emacs

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

(defstruct (codex-struct :named)
  name
  used
  symbols
  export)

(defvar codex-alist nil)

(defun codex-string-id (name)
  (or (and (stringp name) name)
      (and (symbolp name) (symbol-name name))))

(defun codex-by-name (name)
  (let ((name (codex-string-id name)))
    (cdr (assoc name codex-alist))))

(defun codex-define (name specs &optional builtin)
  (let* ((codex (make-codex-struct))
         (used (cdr (assoc :use specs)))
         (export (cdr (assoc :export specs))))
    (setf (codex-struct-name codex)
          name)
    (setf (codex-struct-used codex)
          (mapcar 'codex-string-id used))
    (setf (codex-struct-export codex)
          (mapcar 'codex-string-id export))
    (setf (codex-struct-symbols codex)
          (or (and builtin obarray) (make-vector 17 nil)))
    (let ((cod (assoc name codex-alist)))
      (if cod
          (setcdr cod codex)
        (push (cons name codex) codex-alist))
      codex)))

(defun codex-find-symbol--deps (name deps)
  (and deps
       (or
        (let* ((dep (car deps))
               (cod (cdr (assoc dep codex-alist))))
          (and cod
               (member name (codex-struct-export cod))
               (intern-soft name (codex-struct-symbols cod))))
        (codex-find-symbol--deps name (cdr deps)))))

(defun codex-find-symbol (name cod)
  (and cod
       (or (codex-find-symbol--deps name (codex-struct-used cod))
           (intern-soft name (codex-struct-symbols cod)))))

(defun codex-intern (name cod)
  (or (codex-find-symbol name cod)
      (let ((sym (intern name (codex-struct-symbols cod))))
        (put sym :codex (codex-struct-name cod))
        sym)))

(defun codex-resolve-symbol (sym default-codex)
  (let ((name (symbol-name sym)))
    (cond ((string-match-p "^:.*" name)
           sym)
          ((and (string-match "^\\(.*\\):\\(.*\\)" name)
                (assoc (match-string 1 name) codex-alist)
                (codex-find-symbol (match-string 2 name)
                                   (cdr (assoc (match-string 1 name)
                                               codex-alist)))))
          (t (codex-intern name default-codex)))))

(defun codex-in-codex (codname forms)
  (let* ((cod (codex-by-name codname)))
    (mapcar (lambda (form)
              (cond ((null form) nil)
                    ((symbolp form)
                     (codex-resolve-symbol form cod))
                    ((listp form)
                     (codex-in-codex codname form))
                    (t form)))
            forms)))

(defun codex--dump-codex-debug (codname forms)
  (mapcar (lambda (form)
            (cond ((null form) nil)
                  ((symbolp form)
                   (make-symbol (concat
                                 (or (get form :codex) "*global*")
                                 ":"
                                 (symbol-name form))))
                  ((listp form)
                   (codex--dump-codex-debug codname form))
                  (t form)))
          (codex-in-codex codname forms)))

(defmacro in-codex--debug (codname &rest body)
  (declare (indent 1))
  (list 'quote
        (cons 'progn
              (codex--dump-codex-debug codname body))))

;;;###autoload
(defmacro defcodex (name &rest specs)
  (declare (indent 1))
  (let ((name (codex-string-id name)))
    `(codex-define ,name ',specs)))

;;;###autoload
(defmacro in-codex (codname &rest body)
  (declare (indent 1))
  `(let ((obarray (codex-struct-symbols ,(codex-by-name codname))))
     ,@(codex-in-codex codname body)))

(defun codex-initialize ()

  ;; initialize the "codex" codex
  (codex-define "codex" '((:export "defcodex" "in-codex")) t)

  ;; initialize the "emacs" codex, with all builtins
  (let ((emacs (codex-define "emacs" nil t)))
    (setf (codex-struct-export emacs)
          (let ((subrs nil))
            (mapatoms (lambda (s)
                        (when (and (fboundp s) (subrp (symbol-function s)))
                          (setq subrs (cons (symbol-name s) subrs)))))
            subrs)))

  t)

(provide 'codex)

(eval-after-load 'codex
  '(codex-initialize))

;; (defcodex test
;;   (:use emacs)
;;   (:export "plop"))

;; (in-codex test
;;   (defun plop () 42))

;; (defcodex test2)

;; (in-codex test2
;;   (emacs:defun plop () 0))

;; (in-codex test
;;   (plop))

;; (in-codex test2
;;   (plop))

;; (defcodex test3
;;   (:use test))

;; (in-codex test3
;;   (plop))

;;; codex.el ends here
