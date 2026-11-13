; Text object patterns of Hel for C.
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;
; The file derives from runtime/queries/c/textobjects.scm of Helix,
; https://github.com/helix-editor/helix, at the commit ba40e547426b.

; C++ inherits "c", so it reads this file too.

(function_definition
  body: (_)) @function

(struct_specifier
  body: (_)) @class

(enum_specifier
  body: (_)) @class

(union_specifier
  body: (_)) @class

; A struct, an enum or a union declared on its own, with or without a
; body: the object holds the `;' after it. A forward declaration such as
; `struct S;' has no body. The anchor keeps out a use of the type, as in
; `struct S s;', where a declarator stands before the `;'.

((struct_specifier) @class @_shadowed
 .
 ";" @class)

((enum_specifier) @class @_shadowed
 .
 ";" @class)

((union_specifier) @class @_shadowed
 .
 ";" @class)

; A struct, an enum or a union inside `typedef': the object is the whole
; `typedef'.

(type_definition
  type: [
    (struct_specifier body: (_))
    (enum_specifier body: (_))
    (union_specifier body: (_))
  ] @_shadowed) @class

(parameter_list
  (_) @argument)

(argument_list
  (_) @argument)

(enumerator) @entry

(initializer_list
  (_) @entry)

(comment) @comment

; The strings, for the quote keys of the pair commands, such as mi".
(string_literal) @string
