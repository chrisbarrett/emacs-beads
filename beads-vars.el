;;; beads-vars.el --- Customization variables for beads -*- lexical-binding: t; -*-

;;; Commentary:

;; Customization variables for the beads Emacs package.

;;; Code:

(defgroup beads nil
  "Interface to the bd (beads) issue tracker."
  :group 'tools
  :prefix "beads-")

(defcustom beads-program "bd"
  "Name or path of the bd program executable."
  :type 'string
  :group 'beads)

(provide 'beads-vars)

;;; beads-vars.el ends here
