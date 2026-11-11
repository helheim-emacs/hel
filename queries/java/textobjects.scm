; Text object patterns of Hel for Java.
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;
; The file derives from runtime/queries/java/textobjects.scm of Helix,
; https://github.com/helix-editor/helix, at the commit ba40e547426b.

(method_declaration) @function

(constructor_declaration
  body: (_)) @function

(interface_declaration
  body: (_)) @class

(class_declaration
  body: (_)) @class

(record_declaration
  body: (_)) @class

(enum_declaration
  body: (_)) @class

(formal_parameters
  (_) @argument)

(type_parameters
  (_) @argument)

(type_arguments
  (_) @argument)

(argument_list
  (_) @argument)

(array_initializer
  (_) @entry)

(enum_body
  (enum_constant) @entry)

[
  (line_comment)
  (block_comment)
] @comment
