; Text object patterns of Hel for CSS.
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;
; The file derives from runtime/queries/css/textobjects.scm of Helix,
; https://github.com/helix-editor/helix, at the commit in ../HELIX-REVISION.

; Rule sets and their declaration blocks
(rule_set) @function.around
(rule_set
  (block) @function.inside)

(keyframe_block_list) @function.around
(keyframe_block
  (block) @function.inside)

; At-rules with a body (e.g. @media, @supports)
(media_statement) @class.around
(media_statement
  (block) @class.inside)

; Declarations as key/value entries
(declaration) @entry.around
(declaration
  ":"
  (_) @entry.inside)

(comment) @comment.inside
(comment) @comment.around
