; Text object patterns of Hel for Python.
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;
; The file derives from runtime/queries/python/textobjects.scm of Helix,
; https://github.com/helix-editor/helix, at the commit in ../HELIX-REVISION.

(function_definition
  body: (block)? @function.inside) @function.around

(class_definition
  body: (block)? @class.inside) @class.around

(parameters
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)
  
(lambda_parameters
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(argument_list
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(comment) @comment.inside @comment.around

((function_definition
   name: (identifier) @_name
   body: (block)? @test.inside) @test.around
 (#match? @_name "^test_"))

(list
  (_) @entry.around)

(tuple
  (_) @entry.around)

(set
  (_) @entry.around)

(pair
  (_) @entry.inside) @entry.around

; Hel's own patterns.
;
; A decorated function or class is a `decorated_definition' node, and the
; Helix patterns capture only the definition inside it. These patterns make
; the decorators a part of the object. A match replaces the Helix match whose
; around capture is the node of `@_extended'.

(decorated_definition
  definition: (function_definition) @_extended) @function.around

(decorated_definition
  definition: (class_definition) @_extended) @class.around
