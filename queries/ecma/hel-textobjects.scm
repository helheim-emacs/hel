; Hel's own text object patterns for JavaScript, TypeScript and TSX.
;
; This file belongs to Hel and is licensed under GPL-3.0-or-later, unlike
; the Helix file textobjects.scm next to it.
;
; SPDX-License-Identifier: GPL-3.0-or-later
;
; The languages that inherit "ecma" read this file too. A match replaces
; the match whose around capture is the node of `@_extends'.

; An exported declaration: the object holds `export' and `default'.

(export_statement
  declaration: [
    (function_declaration)
    (generator_function_declaration)
  ] @_extends) @function.around

(export_statement
  declaration: (class_declaration) @_extends) @class.around

(export_statement
  value: [
    (function_expression)
    (arrow_function)
  ] @_extends) @function.around

(export_statement
  value: (class) @_extends) @class.around

; A function assigned to a name in a declaration of one variable: the
; object holds the keyword, the name and the semicolon. The same function
; passed as an argument stays an object of its own.

(lexical_declaration
  .
  (variable_declarator
    value: [
      (function_expression)
      (arrow_function)
    ] @_extends)
  .) @function.around

(variable_declaration
  .
  (variable_declarator
    value: [
      (function_expression)
      (arrow_function)
    ] @_extends)
  .) @function.around

(export_statement
  declaration: (lexical_declaration
                 .
                 (variable_declarator
                   value: [
                     (function_expression)
                     (arrow_function)
                   ])
                 .) @_extends) @function.around
