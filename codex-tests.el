;;; codex-tests.el --- tests for codex.el

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

;; Tests for codex.el

;;; Code:

(require 'codex)
(require 'ert)

(defcodex codex-tests (:use ert codex))

(in-codex codex-tests

  (deftest codex-codex-exists ()
    (should
     (emacs:assoc "codex" (in-codex codex codex-alist))))

  (deftest codex-emacs-exists ()
    (should
     (emacs:assoc "emacs" (in-codex codex codex-alist))))

  (deftest codex-ert-exists ()
    (should
     (emacs:assoc "ert" (in-codex codex codex-alist))))

  (deftest codex-codex-exists-in-codex ()
    (should
     (in-codex codex
       (codexp "codex"))))

  (deftest codex-ert-codex ()
    (should
     (codexp "codex"))))

(provide 'codex-tests)
;;; codex-tests.el ends here
