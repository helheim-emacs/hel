; Text object patterns of Hel for CSS.
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;
; The file derives from runtime/queries/css/textobjects.scm of Helix,
; https://github.com/helix-editor/helix, at the commit ba40e547426b.

; A rule set or a keyframe block counts as a function, and an at-rule with
; a body, such as `@media', counts as a class.

(rule_set) @function

(keyframe_block_list) @function

(keyframe_block) @function

(media_statement) @class

(declaration) @entry

(comment) @comment

; The strings, for the quote keys of the pair commands, such as mi".
(string_value) @string
