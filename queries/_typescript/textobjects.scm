; Text object patterns of Hel for TypeScript and TSX.
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;
; The file derives from runtime/queries/_typescript/textobjects.scm of Helix,
; https://github.com/helix-editor/helix, at the commit in ../HELIX-REVISION.

[
  (interface_declaration 
    body:(_) @class.inside)
  (type_alias_declaration 
    value: (_) @class.inside)
] @class.around

(enum_body
  (_) @entry.around)

(enum_assignment (_) @entry.inside)

; Hel's own patterns.
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
; `@_extended'.

(export_statement
  declaration: [
    (abstract_class_declaration)
    (interface_declaration)
    (type_alias_declaration)
  ] @_extended) @class.around
