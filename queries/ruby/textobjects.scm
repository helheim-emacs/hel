; Text object patterns of Hel for Ruby.
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;
; The file derives from runtime/queries/ruby/textobjects.scm of Helix,
; https://github.com/helix-editor/helix, at the commit ba40e547426b.

(method) @function

(singleton_method) @function

; A method after a visibility modifier, as in `private def m': the object
; holds the modifier.

(call
  method: (identifier) @_modifier
  arguments: (argument_list
               .
               [
                 (method)
                 (singleton_method)
               ] @_shadowed
               .)
  (#match? @_modifier "^\\(private\\|protected\\|public\\|module_function\\|private_class_method\\|public_class_method\\)$")) @function

(do_block) @function

(block) @function

(class) @class

(singleton_class
  value: (_)
  (_)) @class

(call
  receiver: (constant) @_receiver
  method: (identifier) @_method
  (#match? @_receiver "Class")
  (#match? @_method "new")
  (do_block (_))) @class

(module) @class

(method_parameters
  (_) @argument)

(block_parameters
  (_) @argument)

(lambda_parameters
  (_) @argument)

(pair) @entry

(array
  (_) @entry)

(string_array
  (_) @entry)

(symbol_array
  (_) @entry)

(comment) @comment

; The strings, for the quote keys of the pair commands, such as mi".
(string) @string
