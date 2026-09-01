;;; match-paren.el --- Useful functions  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Various useful function used during Emacs startup and runtime

;;; Code:
;;; Matching parentheses
;;;
;;;###autoload
(defun match-parenthesis (arg)
  "Match the current character according to the syntax table.

Based on the freely available match-paren.el by Kayvan Sylvan.
I merged code from goto-matching-paren-or-insert and match-it.

When ARG does not belong to matching pair then insert it at point.

You can define new \"parentheses\" (matching pairs).
Example: angle brackets.  Add the following to your .emacs file:

	(modify-syntax-entry ?< \"(>\" )
	(modify-syntax-entry ?> \")<\" )

You can set hot keys to perform matching with one keystroke.
Example: f6 and Control-C 6.

	(global-set-key \"\\C-c6\" #\='match-parenthesis)
	(global-set-key [f6] #\='match-parenthesis)

Simon Hawkin <cema@cs.umd.edu> 03/14/1998"
  (interactive "p")
  (let
      ((syntax (char-syntax (following-char))))
    (cond
     ((eq syntax ?\()
      (forward-sexp 1) (backward-char))
     ((eq syntax ?\))
      (forward-char) (backward-sexp 1))
     (t (self-insert-command (or arg 1)))
     )
    ))

(provide 'match-paren)

;;; match-paren.el ends here
