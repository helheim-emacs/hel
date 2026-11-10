;;; hel.el --- Helix Emulation Layer -*- lexical-binding: t -*-
;;
;; Copyright © 2025-2026 Yuriy Artemyev
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Created: March 27, 2025
;; Version: 0.13.0
;; Homepage: https://github.com/helheim-emacs/hel
;; Package-Requires: ((emacs "29.1") (dash "2.19.1") (avy "0.5.0") (pcre2el "1.12") (ultra-scroll "0.6"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Emulation of the Kakoune/Helix text editing model.
;;
;;; Code:

(require 'pcre2el)

(require 'hel-vars)
(require 'hel-lib)
(require 'hel-macros)
(require 'hel-multiple-cursors-core)
(require 'hel-core)
(require 'hel-text-objects)
(require 'hel-surround)
(require 'hel-commands)
(require 'hel-search)
(require 'hel-scroll)
(require 'hel-integration)
(require 'hel-keybindings)

(provide 'hel)
;;; hel.el ends here
