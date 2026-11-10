; Text object patterns of Hel for Python.
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;
; The file derives from runtime/queries/python/textobjects.scm of Helix,
; https://github.com/helix-editor/helix, at the commit ba40e547426b.

(function_definition) @function

; A decorated function or class is a `decorated_definition' node: the
; object holds the decorators.

(decorated_definition
  definition: (function_definition) @_shadowed) @function

(class_definition) @class

(decorated_definition
  definition: (class_definition) @_shadowed) @class

(parameters
  (_) @argument)

(lambda_parameters
  (_) @argument)

(argument_list
  (_) @argument)

(list
  (_) @entry)

(tuple
  (_) @entry)

(set
  (_) @entry)

(pair) @entry

(comment) @comment

; The strings, for the quote keys of the pair commands, such as mi".
(string) @string
