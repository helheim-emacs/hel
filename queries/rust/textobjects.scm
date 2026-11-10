; Text object patterns of Hel for Rust.
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;
; The file derives from runtime/queries/rust/textobjects.scm of Helix,
; https://github.com/helix-editor/helix, at the commit in ../HELIX-REVISION.

(
  (attribute_item)* @function.around
  .
  (function_item
    body: (_) @function.inside) @function.around
)

(closure_expression
  body: (_) @function.inside) @function.around

(struct_item
  body: (_) @class.inside) @class.around

(enum_item
  body: (_) @class.inside) @class.around

(union_item
  body: (_) @class.inside) @class.around

(trait_item
  body: (_) @class.inside) @class.around

(impl_item
  body: (_) @class.inside) @class.around

(parameters 
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(type_parameters
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(type_arguments
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(closure_parameters
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(arguments
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(field_initializer_list  
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

[
  (line_comment)
  (block_comment)
] @comment.inside

(line_comment) @comment.around

(block_comment) @comment.around

(; #[test]
 (attribute_item
   (attribute
     (identifier) @_test_attribute)) @test.around
 ; allow other attributes like #[should_panic] and comments
 [
   (attribute_item) @test.around
   (line_comment) @test.around
 ]*
 .
 ; the test function
 (function_item
   body: (_) @test.inside) @test.around
 (#eq? @_test_attribute "test"))

(array_expression
  (_) @entry.around)

(tuple_expression
  (_) @entry.around)

(tuple_pattern
  (_) @entry.around)

; Commonly used vec macro initializer is special cased
(macro_invocation
  (identifier) @_id (token_tree (_) @entry.around)
  (#eq? @_id "vec"))

(enum_variant) @entry.around

(field_declaration
  (_) @entry.inside) @entry.around

(field_initializer
  (_) @entry.inside) @entry.around

(shorthand_field_initializer) @entry.around

; Hel's own patterns.
;
; The Helix patterns put the attributes above a function into the object,
; and leave them out for the other items. This pattern puts the attributes
; above a struct, an enum, a union, a trait or an impl into the object, so
; that `#[derive(...)]' goes with its type. A match replaces the match whose
; around capture is the node of `@_extended'.

((attribute_item)+ @class.around
 .
 [
   (struct_item body: (_))
   (enum_item body: (_))
   (union_item body: (_))
   (trait_item body: (_))
   (impl_item body: (_))
 ] @class.around @_extended)
