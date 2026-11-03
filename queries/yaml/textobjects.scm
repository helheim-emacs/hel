; Text object patterns of Hel for YAML.
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;
; The file derives from runtime/queries/yaml/textobjects.scm of Helix,
; https://github.com/helix-editor/helix, at the commit ba40e547426b.

(block_mapping_pair) @entry

(comment) @comment

; The strings, for the quote keys of the pair commands, such as mi".
[(double_quote_scalar) (single_quote_scalar)] @string
