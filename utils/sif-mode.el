;; sif-mode.el
;; Copyright (C) 2015 by Jan de Muijnck-Hughes
;; Author: Jan de Muijnck-Hughes <jfdm@st-andrews.ac.uk>
;;
;;; DESCRIPTION
;;
;; A derived mode to highlight SIF input files.
;;
;;; INSTALLATION
;;
;; 1. Place sif-mode.el in your .emacs.d directory
;; 2. Ensure that .emacs.d is on your load path.
;; 3. Add the following to your emacs setup file:
;;    (add-to-list 'auto-mode-alist (quote ("\\.sif$" . sif-mode)))
;; -------------------------------------------------------------------
;;; CODE
;; --------------------------------------------------- [ Constraints ]
(defvar sif-relations '(
  "Makes" "Helps" "SomePos" "Unknown" "SomeNeg" "Breaks" "Hurts"
  "Denied" "WeakDen" "WeakSatis" "Satisfied" "Undecided" "None"

))
;; ------------------------------------------------------ [ Keywords ]
(defvar sif-keywords '(
  "sif" "is" "in" "by" "solves"
))

;; --------------------------------------------------------- [ Types ]
(defvar sif-builtin '(
    "solution" "problem"
))
;; --------------------------------------------------------- [ Types ]
(defvar sif-types '( "Problem" "Solution"
    "Functional" "Usability" "Reliability" "Performance" "Supportability"
    "Component" "System" "Deployment" "Admin" "Implementation"
    "Property" "Advantage" "Trait" "Description" "Disadvantage" "Affects"
    "Context"
))
;; ----------------------------------------------------- [ Constants ]
(defvar sif-modifiers '(
    "Abstract" "Concrete"
))
;; -------------------------------------------------- [ Assign Faces ]
(defvar sif-font-lock-defaults
  `((
    ( "\\s-*\\(>\\)\\(.*\\)$"
         (1 font-lock-keyword-face)
         (2 font-lock-comment-face))
    ( ,(regexp-opt sif-relations 'words) . font-lock-constant-face)
    ( ,(regexp-opt sif-keywords  'words) . font-lock-keyword-face)
    ( ,(regexp-opt sif-modifiers 'words) . font-lock-builtin-face)
    ( ,(regexp-opt sif-builtin   'words) . font-lock-type-face)
    ( ,(regexp-opt sif-types     'words) . font-lock-variable-name-face)
    ( "<-" . font-lock-constant-face)
)))
;; --------------------------------------------------- [ Clear memory ]
(setq sif-relations    nil
      sif-keywords     nil
      sif-types        nil
      sif-modifiers    nil
      sif-builtin      nil
)
;; -------------------------------------------------------------------
;; SIF Definition
;; -------------------------------------------------------------------
(define-derived-mode sif-mode fundamental-mode "SIF"
  "Major mode for editing SIF files."
  (defgroup sif-mode nil
    "Derived mode for SIF Files" :group 'languages)
  (defvar sif-mode-hook nil "Hook for sif-mode")
  (modify-syntax-entry ?-  "_ 123" sif-mode-syntax-table)
  (modify-syntax-entry ?\n ">" sif-mode-syntax-table)
  (modify-syntax-entry ?\{  "(}1nb" sif-mode-syntax-table)
  (modify-syntax-entry ?\}  "){4nb" sif-mode-syntax-table)

  (mapcar (lambda (x)
            (modify-syntax-entry x "_" sif-mode-syntax-table))
          ;; Some of these are actually OK by default.
          "!#$%&*+./:<=>?@^|~")


  (make-local-variable 'sif-font-lock-defaults)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-end-skip)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-padding)
  (make-local-variable 'comment-multi-line)
  (make-local-variable 'comment-indent-function)

  (setq font-lock-defaults sif-font-lock-defaults
        comment-start           "-- "
        comment-end             ""
        comment-start-skip      "[-{]-[ \t]*"
        comment-end-skip        "[ \t]*\\(-}\\|\\s>\\)"
        comment-column          60
        comment-padding         0
        comment-multi-line      nil
        comment-indent-function 'java-comment-indent
        indent-tabs-mode        t
  )
 (run-hooks 'sif-mode-hook)
)

(provide 'sif-mode)
