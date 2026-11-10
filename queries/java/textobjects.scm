; Text object patterns of Hel for Java.
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;
; The file derives from runtime/queries/java/textobjects.scm of Helix,
; https://github.com/helix-editor/helix, at the commit in ../HELIX-REVISION.

(method_declaration
  body: (_)? @function.inside) @function.around

(constructor_declaration
  body: (_) @function.inside) @function.around

(interface_declaration
  body: (_) @class.inside) @class.around

(class_declaration
  body: (_) @class.inside) @class.around

(record_declaration
  body: (_) @class.inside) @class.around

(enum_declaration
  body: (_) @class.inside) @class.around

(formal_parameters
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(type_parameters
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(type_arguments
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(argument_list
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

[
  (line_comment)
  (block_comment)
] @comment.inside

(line_comment) @comment.around

(block_comment) @comment.around

(array_initializer
  (_) @entry.around)

(enum_body
  (enum_constant) @entry.around)
