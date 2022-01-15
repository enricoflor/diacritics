;;; diacritics.el --- Quickly insert special characters -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Enrico Flor

;; Author: Enrico Flor <enrico@eflor.net>
;; Maintainer: Enrico Flor <enrico@eflor.net>
;; URL: https://github.com/enricoflor/diacritics
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides one command to quickly insert special
;; characters.  In some cases, having a fast way to insert a modified
;; character is more convenient than using special input methods.
;; Specify what set of modified characters to associate to which
;; character in 'diacritics-alist'.  Then, when you call the command
;; 'diacritics-insert', and try to self insert a character that is a
;; key in 'diacritics-alist', you will see in the echo area pairs of
;; digits and characters: the formers are the ones that are associated
;; in 'diacritics-alist' with the key you inserted.  Choose which one
;; to insert via the digit.  If you choose press any other key, it
;; will just self-insert.  This package was inspired by a similar
;; functionality provided in MacOS.

;;; Code:

(require 'cl-lib)

(defvar diacritics-alist '(("a" . ("à" "á" "ä"))
                           ("e" . ("è" "é")))
  "Alist associating base characters with list of modifications.
Modify this alist according to your needs.  For any list of
modification, all the elements after the first nine are going to
be ignored.")

(defun diacritics-insert (in &optional arg)
  "Insert the modification of the base character IN.
The prefix argument ARG behaves exactly like the prefix argument
in 'self-insert-command': if given as nil, the character selected
will be inserted 4 times, with any other numerical value N, it
will be inserted N times.  The user inputs IN and then chooses
from a list of modifications, which is the value associated to
the key in 'diacritics-alist' that corresponds to the string
value of IN.  When prompted with a choice of modification, any
input that is not an integer in the selection or IN itself makes
the function evaluate to t.  Repeating IN at the prompt inserts
the base character corresponding to IN."
  (interactive "c\np")
  (when (assoc (char-to-string in) diacritics-alist)
    (let* ((raw-char-list (cdr (assoc (char-to-string in)
                                      diacritics-alist)))
           (char-list (if (> (length raw-char-list) 9)
                          (cl-subseq raw-char-list 0 9)
                        raw-char-list))
           (num-list (mapcar #'number-to-string
                             (number-sequence 1 (length char-list))))
           (selection-alist (cl-mapcar (lambda (x y) (cons x y))
                                       num-list
                                       char-list))
           (prompt (mapconcat (lambda (x) (concat (car x)
                                                  " "
                                                  (cdr x)))
                              selection-alist "  -  "))
           (raw-choice (ignore-errors (read-char prompt t)))
           (choice (char-to-string raw-choice))
           (coeff (if arg arg 1)))
      (if (member choice num-list)
          (insert (make-string coeff
                               (string-to-char
                                (cdr (assoc choice
                                            selection-alist)))))
        (insert (make-string coeff (string-to-char choice)))))))

(provide 'diacritics)

;;; _
;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; diacritics.el ends here
