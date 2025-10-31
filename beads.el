;;; beads.el --- Emacs interface for bd issue tracker -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Chris Barrett

;; Author: Chris Barrett <chris@walrus.cool>
;; URL: https://github.com/chrisbarrett/emacs-beads
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (magit-section "4.4.0"))
;; Keywords: tools

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs interface for the bd (beads) issue tracker.
;;
;; Beads provides a git-commit-like buffer interface for creating issues,
;; asynchronous process execution with magit-process-style logging, and
;; AI agent integration for natural language issue creation.
;;
;; See the README for more information:
;; https://github.com/chrisbarrett/emacs-beads

;;; Code:

(require 'beads-vars)
(require 'beads-process)
(require 'beads-agent)
(require 'beads-agent-claude)

(provide 'beads)

;;; beads.el ends here
