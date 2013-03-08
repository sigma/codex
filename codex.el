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


(let ((codex-obarray (make-vector 19 nil)))

  (let ((Smake-struct (intern "make-struct" codex-obarray))
        (Sstruct-name (intern "struct-name" codex-obarray))
        (Sstruct-used (intern "struct-used" codex-obarray))
        (Sstruct-symbols (intern "struct-symbols" codex-obarray))
        (Sstruct-export (intern "struct-export" codex-obarray))
        (Sstructs (intern "structs" codex-obarray))
        (Sstring-id (intern "string-id" codex-obarray))
        (Sby-name (intern "by-name" codex-obarray))
        (Sdefine (intern "define" codex-obarray))
        (Sfind-symbol-deps (intern "find-symbol-deps" codex-obarray))
        (Sfind-symbol (intern "find-symbol" codex-obarray))
        (Sintern (intern "intern" codex-obarray))
        (Sresolve-symbol (intern "resolve-symbol" codex-obarray))
        (Sin-codex-func (intern "in-codex-func" codex-obarray))
        (Sdefcodex (intern "defcodex" codex-obarray))
        (Sin-codex (intern "in-codex" codex-obarray)))

    (fset Smake-struct
          (lambda (name used symbols export)
            (let ((vec (make-vector 4 nil)))
              (aset vec 0 name)
              (aset vec 1 used)
              (aset vec 2 symbols)
              (aset vec 3 export)
              vec)))

    (fset Sstruct-name
          (lambda (s) (aref s 0)))

    (fset Sstruct-used
          (lambda (s) (aref s 1)))

    (fset Sstruct-symbols
          (lambda (s) (aref s 2)))

    (fset Sstruct-export
          (lambda (s) (aref s 3)))

    ;; variable to hold the set of defined codices
    (set Sstructs nil)

    (fset Sstring-id
          (lambda (name)
            (or (and (stringp name) name)
                (and (symbolp name) (symbol-name name)))))

    (fset Sby-name
          `(lambda (name)
             (let ((name (,Sstring-id name)))
               (cdr (assoc name (symbol-value ',Sstructs))))))

    (fset Sdefine
          `(lambda (name specs &optional ob)
             (let* ((used (cdr (assoc :use specs)))
                    (export (cdr (assoc :export specs)))
                    (codex (,Smake-struct
                            name
                            (mapcar ',Sstring-id used)
                            (or (and (eq ob t) obarray)
                                ob
                                (make-vector 17 nil))
                            (mapcar ',Sstring-id export))))
               (let ((cod (assoc name (symbol-value ',Sstructs))))
                 (if cod
                     (setcdr cod codex)
                   (push (cons name codex) ,Sstructs))
                 codex))))

    (fset Sfind-symbol-deps
          `(lambda (name deps)
             (and deps
                  (or
                   (let* ((dep (car deps))
                          (cod (cdr (assoc dep (symbol-value ',Sstructs)))))
                     (and cod
                          (member name
                                  (,Sstruct-export cod))
                          (intern-soft name
                                       (,Sstruct-symbols cod))))
                   (,Sfind-symbol-deps name (cdr deps))))))

    (fset Sfind-symbol
          `(lambda (name cod)
             (and cod
                  (or (,Sfind-symbol-deps name
                                          (,Sstruct-used cod))
                      (intern-soft name (,Sstruct-symbols cod))))))

    (fset Sintern
          `(lambda (name cod)
             (or (,Sfind-symbol name cod)
                 (let ((sym (intern name (,Sstruct-symbols cod))))
                   (put sym :codex (,Sstruct-name cod))
                   sym))))

    (fset Sresolve-symbol
          `(lambda (sym default-codex)
             (let ((name (symbol-name sym)))
               (cond ((string-match-p "^:.*" name)
                      sym)
                     ((and (string-match "^\\(.*\\):\\(.*\\)" name)
                           (assoc (match-string 1 name) (symbol-value ',Sstructs))
                           (,Sfind-symbol
                            (match-string 2 name)
                            (cdr (assoc (match-string 1 name)
                                        (symbol-value ',Sstructs))))))
                     (t (,Sintern name default-codex))))))

    (fset Sin-codex-func
          `(lambda (codname forms)
             (let* ((cod (,Sby-name codname)))
               (mapcar (lambda (form)
                         (cond ((null form) nil)
                               ((symbolp form)
                                (,Sresolve-symbol form cod))
                               ((listp form)
                                (,Sin-codex-func codname form))
                               (t form)))
                       forms))))

    ;; create "codex" codex
    (funcall Sdefine "codex" '((:export "defcodex" "in-codex"))
             codex-obarray)

    ;; create "emacs" codex
    (funcall Sdefine "emacs"
             (let ((subrs nil))
               (mapatoms (lambda (s)
                           (when (and (fboundp s) (subrp (symbol-function s)))
                             (setq subrs (cons (symbol-name s) subrs)))))
               subrs)
             t)

    (defmacro defcodex (name &rest specs)
      (declare (indent 1))
      (let ((name (funcall (intern "string-id" (get 'codex :obarray)) name)))
        `(funcall (intern "define" (get 'codex :obarray)) ,name ',specs)))

    (fset Sdefcodex (symbol-function 'defcodex))
    (fset 'defcodex Sdefcodex)

    (defmacro in-codex (codname &rest body)
      (declare (indent 1))
      `(let ((obarray (funcall (intern "struct-symbols" (get 'codex :obarray))
                               ,(funcall (intern "by-name" (get 'codex :obarray))
                                         codname))))
         ,@(funcall (intern "in-codex-func" (get 'codex :obarray)) codname body)))

    (fset Sin-codex (symbol-function 'in-codex))
    (fset 'in-codex Sin-codex))

  (put 'codex :obarray codex-obarray))

;; (defun codex--dump-codex-debug (codname forms)
;;   (mapcar (lambda (form)
;;             (cond ((null form) nil)
;;                   ((symbolp form)
;;                    (make-symbol (concat
;;                                  (or (get form :codex) "*global*")
;;                                  ":"
;;                                  (symbol-name form))))
;;                   ((listp form)
;;                    (codex--dump-codex-debug codname form))
;;                   (t form)))
;;           (codex-in-codex codname forms)))

;; (defmacro in-codex--debug (codname &rest body)
;;   (declare (indent 1))
;;   (list 'quote
;;         (cons 'progn
;;               (codex--dump-codex-debug codname body))))

(provide 'codex)

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
