; Text object patterns of Hel for PHP.
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;
; The file derives from runtime/queries/php/textobjects.scm of Helix,
; https://github.com/helix-editor/helix, at the commit in ../HELIX-REVISION.

(class_declaration
  body: (_) @class.inside) @class.around

(interface_declaration
  body: (_) @class.inside) @class.around

(trait_declaration
  body: (_) @class.inside) @class.around

(enum_declaration
  body: (_) @class.inside) @class.around

(function_definition
  body: (_) @function.inside) @function.around

(method_declaration
  body: (_) @function.inside) @function.around

(arrow_function 
  body: (_) @function.inside) @function.around
  
(anonymous_function
  body: (_) @function.inside) @function.around

(anonymous_class
  body: (_) @class.inside) @class.around

(property_hook
  body: (_) @function.inside) @function.around

(anonymous_function_use_clause
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(formal_parameters
  ([
    (simple_parameter)
    (variadic_parameter)
    (property_promotion_parameter)
  ] @parameter.inside . ","? @parameter.around) @parameter.around)

(arguments
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(comment) @comment.inside @comment.around

(array_creation_expression
  (array_element_initializer
    (_) @entry.inside
  ) @entry.around @entry.movement)

(list_literal
  (_) @entry.inside @entry.around @entry.movement)

[
  (enum_case)
] @entry.around @entry.movement
