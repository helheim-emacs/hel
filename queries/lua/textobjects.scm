; Text object patterns of Hel for Lua.
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;
; The file derives from runtime/queries/lua/textobjects.scm of Helix,
; https://github.com/helix-editor/helix, at the commit ba40e547426b.

(function_definition
  body: (_)) @function

(function_declaration
  body: (_)) @function

(parameters
  (_) @argument)

(arguments
  (_) @argument)

(table_constructor
  (field) @entry)

(comment) @comment
