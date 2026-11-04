; Hel's own text object patterns for TypeScript and TSX.
;
; This file belongs to Hel and is licensed under GPL-3.0-or-later, unlike
; the Helix file textobjects.scm next to it.
;
; SPDX-License-Identifier: GPL-3.0-or-later
;
; The languages that inherit "_typescript" read this file too.

; Objects that the Helix patterns lack: an abstract class, and a function
; that `declare' introduces.

(abstract_class_declaration
  body: (class_body) @class.inside) @class.around

(ambient_declaration
  (function_signature)) @function.around

; An exported declaration of a TypeScript kind: the object holds `export'.
; A match replaces the match whose around capture is the node of
; `@_extends'.

(export_statement
  declaration: [
    (abstract_class_declaration)
    (interface_declaration)
    (type_alias_declaration)
  ] @_extends) @class.around
