; Hel's own text object patterns for C.
;
; This file belongs to Hel and is licensed under GPL-3.0-or-later, unlike
; the Helix file textobjects.scm next to it.
;
; SPDX-License-Identifier: GPL-3.0-or-later
;
; C++ inherits "c", so it reads this file too. A match replaces the match
; whose around capture is the node of `@_extends'. Unlike a Helix match,
; the object keeps every node that the pattern captures, the anonymous `;'
; included.

; A struct, an enum or a union declared on its own: the object holds the
; `;' after it.

((struct_specifier body: (_)) @class.around @_extends
 .
 ";" @class.around)

((enum_specifier body: (_)) @class.around @_extends
 .
 ";" @class.around)

((union_specifier body: (_)) @class.around @_extends
 .
 ";" @class.around)

; A struct, an enum or a union inside `typedef': the object is the whole
; `typedef'.

(type_definition
  type: [
    (struct_specifier body: (_))
    (enum_specifier body: (_))
    (union_specifier body: (_))
  ] @_extends) @class.around
