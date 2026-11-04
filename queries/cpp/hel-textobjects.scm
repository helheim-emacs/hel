; Hel's own text object patterns for C++.
;
; This file belongs to Hel and is licensed under GPL-3.0-or-later, unlike
; the Helix file textobjects.scm next to it.
;
; SPDX-License-Identifier: GPL-3.0-or-later
;
; A match replaces the match whose around capture is the node of
; `@_extends'. The object keeps every node that the pattern captures, the
; anonymous `;' included.

; A class declared on its own: the object holds the `;' after it.

((class_specifier body: (_)) @class.around @_extends
 .
 ";" @class.around)

; A template: the object holds the `template <...>' line.

(template_declaration
  (function_definition) @_extends) @function.around

(template_declaration
  [
    (class_specifier body: (_))
    (struct_specifier body: (_))
  ] @_extends) @class.around
