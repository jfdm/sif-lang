;; sif-mode.el -- Derived mode for writing Essence'
;; Copyright (C) 2011 by Jan de Muijnck-Hughes
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
    "extends" "implements" "ofType" "linkedTo" "uses" 
))
;; ------------------------------------------------------ [ Keywords ]
(defvar sif-keywords '(
  "from" "import" "as" "end"
))
;; --------------------------------------------------------- [ Types ]
(defvar sif-types '(
    "Abstract" "Pattern" "Integration"
))
;; ----------------------------------------------------- [ Constants ]
(defvar sif-constants '(
    "NONE" "ALL" "STATIC"
))
;; ----------------------------------------------------- [ Structure ]
(defvar sif-functions '(
                       "pattern"  "catalogue"
))
;; -------------------------------------------------- [ Assign Faces ]
(defvar sif-font-lock-defaults
  `((
    ( ,(regexp-opt sif-relations  'words) . font-lock-reference-face)
    ( ,(regexp-opt sif-keywords     'words) . font-lock-keyword-face)
    ( ,(regexp-opt sif-types        'words) . font-lock-type-face)
    ( ,(regexp-opt sif-constants    'words) . font-lock-constant-face)
    ( ,(regexp-opt sif-functions 'words) . font-lock-function-name-face)

)))
;; --------------------------------------------------- [ Clear memory ]
(setq sif-relations    nil
      sif-keywords     nil
      sif-types        nil
      sif-constants    nil
      sif-functions    nil
)
;; -------------------------------------------------------------------
;; SIF Definition
;; -------------------------------------------------------------------
(define-derived-mode sif-mode fundamental-mode "SIF"
  "Major mode for editing SIF files."
  (defgroup sif-mode nil
    "Derived mode for SIF Files" :group 'languages)
  (defvar sif-mode-hook nil "Hook for sif-mode")
  (modify-syntax-entry ?# "<" sif-mode-syntax-table)
  (modify-syntax-entry ?\n ">" sif-mode-syntax-table)

  (make-local-variable 'sif-font-lock-defaults)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-multi-line)
  (make-local-variable 'comment-indent-function)

  (setq font-lock-defaults sif-font-lock-defaults
        comment-start           "# "
        comment-end             ""
        comment-start-skip      "+ #+ \n"
        comment-column          60
        comment-multi-line      nil
        comment-indent-function 'java-comment-indent
        indent-tabs-mode        t
  )
 (run-hooks 'sif-mode-hook)
)
(provide 'sif-mode)
