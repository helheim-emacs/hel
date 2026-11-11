; Text object patterns of Hel for PHP.
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;
; The file derives from runtime/queries/php/textobjects.scm of Helix,
; https://github.com/helix-editor/helix, at the commit ba40e547426b.

(function_definition
  body: (_)) @function

(method_declaration
  body: (_)) @function

(arrow_function
  body: (_)) @function

(anonymous_function
  body: (_)) @function

(property_hook
  body: (_)) @function

(class_declaration
  body: (_)) @class

(interface_declaration
  body: (_)) @class

(trait_declaration
  body: (_)) @class

(enum_declaration
  body: (_)) @class

(anonymous_class
  body: (_)) @class

(anonymous_function_use_clause
  (_) @argument)

(formal_parameters
  [
    (simple_parameter)
    (variadic_parameter)
    (property_promotion_parameter)
  ] @argument)

(arguments
  (_) @argument)

(array_creation_expression
  (array_element_initializer) @entry)

(list_literal
  (_) @entry)

(enum_case) @entry

(comment) @comment
