; Text object patterns of Hel for C++.
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;
; The file derives from runtime/queries/cpp/textobjects.scm of Helix,
; https://github.com/helix-editor/helix, at the commit in ../HELIX-REVISION.

; inherits: c

(lambda_expression
  body: (_) @function.inside) @function.around

(class_specifier
  body: (_) @class.inside) @class.around

; Hel's own patterns.
;
; A match replaces the match whose around capture is the node of
; `@_extended'. The object keeps every node that the pattern captures, the
; anonymous `;' included.

; A class declared on its own: the object holds the `;' after it.

((class_specifier body: (_)) @class.around @_extended
 .
 ";" @class.around)

; A template: the object holds the `template <...>' line.

(template_declaration
  (function_definition) @_extended) @function.around

(template_declaration
  [
    (class_specifier body: (_))
    (struct_specifier body: (_))
  ] @_extended) @class.around
