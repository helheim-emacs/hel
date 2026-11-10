; Text object patterns of Hel for TypeScript and TSX.
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;
; The file derives from runtime/queries/_typescript/textobjects.scm of Helix,
; https://github.com/helix-editor/helix, at the commit ba40e547426b.

; The languages that inherit "_typescript" read this file too.

(ambient_declaration
  (function_signature)) @function

(abstract_class_declaration
  body: (class_body)) @class

[
  (interface_declaration
    body: (_))
  (type_alias_declaration
    value: (_))
] @class

; An exported declaration of a TypeScript kind: the object holds `export'.

(export_statement
  declaration: [
    (abstract_class_declaration)
    (interface_declaration)
    (type_alias_declaration)
  ] @_shadowed) @class

(enum_body
  (_) @entry)
