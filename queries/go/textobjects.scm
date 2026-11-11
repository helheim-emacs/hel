; Text object patterns of Hel for Go.
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;
; The file derives from runtime/queries/go/textobjects.scm of Helix,
; https://github.com/helix-editor/helix, at the commit ba40e547426b.

(function_declaration) @function

(func_literal) @function

(method_declaration) @function

(type_declaration
  (type_spec
    (type_identifier)
    (struct_type (field_declaration_list)))) @class

(type_declaration
  (type_spec
    (type_identifier)
    (interface_type (method_elem)))) @class

(type_parameter_list
  (_) @argument)

(parameter_list
  (_) @argument)

(argument_list
  (_) @argument)

(comment) @comment
