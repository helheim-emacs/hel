; Text object patterns of Hel for Ruby.
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;
; The file derives from runtime/queries/ruby/textobjects.scm of Helix,
; https://github.com/helix-editor/helix, at the commit in ../HELIX-REVISION.

; Class and Modules
(class
  body: (_)? @class.inside) @class.around

(singleton_class
  value: (_)
  (_)+ @class.inside) @class.around

(call
  receiver: (constant) @class_const
  method: (identifier) @class_method
  (#match? @class_const "Class")
  (#match? @class_method "new")
  (do_block (_)+ @class.inside)) @class.around
  
(module
  body: (_)? @class.inside) @class.around

; Functions and Blocks
(singleton_method
  body: (_)? @function.inside) @function.around

(method
  body: (_)? @function.inside) @function.around

(do_block
  body: (_)? @function.inside) @function.around

(block
  body: (_)? @function.inside) @function.around

; Parameters      
(method_parameters
  (_) @parameter.inside) @parameter.around
        
(block_parameters 
  (_) @parameter.inside) @parameter.around
        
(lambda_parameters 
  (_) @parameter.inside) @parameter.around

; Comments
(comment) @comment.inside 
(comment) @comment.around

(pair
  (_) @entry.inside) @entry.around

(array
  (_) @entry.around)

(string_array
  (_) @entry.around)

(symbol_array
  (_) @entry.around)

; Hel's own patterns.
;
; A method after a visibility modifier, as in `private def m': the object
; holds the modifier. A match replaces the match whose around capture is
; the node of `@_extended'.

(call
  method: (identifier) @_modifier
  arguments: (argument_list
               .
               [
                 (method)
                 (singleton_method)
               ] @_extended
               .)
  (#match? @_modifier "^\\(private\\|protected\\|public\\|module_function\\|private_class_method\\|public_class_method\\)$")) @function.around
