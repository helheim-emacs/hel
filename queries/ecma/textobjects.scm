; Text object patterns of Hel for JavaScript, TypeScript and TSX.
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;
; The file derives from runtime/queries/ecma/textobjects.scm of Helix,
; https://github.com/helix-editor/helix, at the commit ba40e547426b.

; The languages that inherit "ecma" read this file too.

(function_declaration
  body: (_)) @function

(generator_function_declaration
  body: (_)) @function

(function_expression
  body: (_)) @function

(arrow_function
  body: (_)) @function

(method_definition
  body: (_)) @function

(class_declaration
  body: (class_body)) @class

(class
  (class_body)) @class

; An exported declaration: the object holds `export' and `default'.

(export_statement
  declaration: [
    (function_declaration)
    (generator_function_declaration)
  ] @_shadowed) @function

(export_statement
  value: [
    (function_expression)
    (arrow_function)
  ] @_shadowed) @function

(export_statement
  declaration: (class_declaration) @_shadowed) @class

(export_statement
  value: (class) @_shadowed) @class

; A function assigned to a name in a declaration of one variable: the
; object holds the keyword, the name and the semicolon. The same function
; passed as an argument stays an object of its own.

(lexical_declaration
  .
  (variable_declarator
    value: [
      (function_expression)
      (arrow_function)
    ] @_shadowed)
  .) @function

(variable_declaration
  .
  (variable_declarator
    value: [
      (function_expression)
      (arrow_function)
    ] @_shadowed)
  .) @function

(export_statement
  declaration: (lexical_declaration
                 .
                 (variable_declarator
                   value: [
                     (function_expression)
                     (arrow_function)
                   ])
                 .) @_shadowed) @function

(formal_parameters
  (_) @argument)

(arguments
  (_) @argument)

(array
  (_) @entry)

(pair) @entry

(pair_pattern) @entry

(comment) @comment
