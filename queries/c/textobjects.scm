; Text object patterns of Hel for C.
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;
; The file derives from runtime/queries/c/textobjects.scm of Helix,
; https://github.com/helix-editor/helix, at the commit in ../HELIX-REVISION.

(function_definition
  body: (_) @function.inside) @function.around

(struct_specifier
  body: (_) @class.inside) @class.around

(enum_specifier
  body: (_) @class.inside) @class.around

(union_specifier
  body: (_) @class.inside) @class.around

(parameter_list 
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(argument_list
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(comment) @comment.inside @comment.around

(enumerator
  (_) @entry.inside) @entry.around

(initializer_list
  (_) @entry.around)

; Hel's own patterns.
;
; C++ inherits "c", so it reads this file too. A match replaces the match
; whose around capture is the node of `@_extended'. Unlike a Helix match,
; the object keeps every node that the pattern captures, the anonymous `;'
; included.

; A struct, an enum or a union declared on its own: the object holds the
; `;' after it.

((struct_specifier body: (_)) @class.around @_extended
 .
 ";" @class.around)

((enum_specifier body: (_)) @class.around @_extended
 .
 ";" @class.around)

((union_specifier body: (_)) @class.around @_extended
 .
 ";" @class.around)

; A struct, an enum or a union inside `typedef': the object is the whole
; `typedef'.

(type_definition
  type: [
    (struct_specifier body: (_))
    (enum_specifier body: (_))
    (union_specifier body: (_))
  ] @_extended) @class.around
