; Text object patterns of Hel for JavaScript, TypeScript and TSX.
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;
; The file derives from runtime/queries/ecma/textobjects.scm of Helix,
; https://github.com/helix-editor/helix, at the commit in ../HELIX-REVISION.

(function_declaration
  body: (_) @function.inside) @function.around

(function_expression
  body: (_) @function.inside) @function.around

(arrow_function
  body: (_) @function.inside) @function.around

(method_definition
  body: (_) @function.inside) @function.around

(generator_function_declaration
  body: (_) @function.inside) @function.around

(class_declaration
  body: (class_body) @class.inside) @class.around

(class
  (class_body) @class.inside) @class.around

(export_statement
  declaration: [
    (function_declaration) @function.around
    (class_declaration) @class.around 
  ])

(formal_parameters
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(arguments
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(comment) @comment.inside @comment.around

(array 
  (_) @entry.around)

(pair
  (_) @entry.inside) @entry.around

(pair_pattern
  (_) @entry.inside) @entry.around

; Hel's own patterns.
;
; The languages that inherit "ecma" read this file too. A match replaces
; the match whose around capture is the node of `@_extended'.

; An exported declaration: the object holds `export' and `default'.

(export_statement
  declaration: [
    (function_declaration)
    (generator_function_declaration)
  ] @_extended) @function.around

(export_statement
  declaration: (class_declaration) @_extended) @class.around

(export_statement
  value: [
    (function_expression)
    (arrow_function)
  ] @_extended) @function.around

(export_statement
  value: (class) @_extended) @class.around

; A function assigned to a name in a declaration of one variable: the
; object holds the keyword, the name and the semicolon. The same function
; passed as an argument stays an object of its own.

(lexical_declaration
  .
  (variable_declarator
    value: [
      (function_expression)
      (arrow_function)
    ] @_extended)
  .) @function.around

(variable_declaration
  .
  (variable_declarator
    value: [
      (function_expression)
      (arrow_function)
    ] @_extended)
  .) @function.around

(export_statement
  declaration: (lexical_declaration
                 .
                 (variable_declarator
                   value: [
                     (function_expression)
                     (arrow_function)
                   ])
                 .) @_extended) @function.around
