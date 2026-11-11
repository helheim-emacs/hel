; Text object patterns of Hel for C#.
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;
; The file derives from runtime/queries/c-sharp/textobjects.scm of Helix,
; https://github.com/helix-editor/helix, at the commit ba40e547426b.

(constructor_declaration
  body: (_)) @function

(destructor_declaration
  body: (_)) @function

(method_declaration
  body: (_)) @function

(property_declaration
  (_)) @function

[
  (class_declaration body: (_))
  (struct_declaration body: (_))
  (interface_declaration body: (_))
  (enum_declaration body: (_))
  (delegate_declaration)
  (record_declaration body: (_))
] @class

(parameter
  (_)) @argument

(comment) @comment
