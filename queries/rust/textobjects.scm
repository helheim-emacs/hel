; Text object patterns of Hel for Rust.
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;
; The file derives from runtime/queries/rust/textobjects.scm of Helix,
; https://github.com/helix-editor/helix, at the commit ba40e547426b.

; The object of a function or a type holds the attributes above it, so that
; `#[derive(...)]' goes with its type.

(
  (attribute_item)* @function
  .
  (function_item
    body: (_)) @function
)

(closure_expression
  body: (_)) @function

(struct_item
  body: (_)) @class

(enum_item
  body: (_)) @class

(union_item
  body: (_)) @class

(trait_item
  body: (_)) @class

(impl_item
  body: (_)) @class

((attribute_item)+ @class
 .
 [
   (struct_item body: (_))
   (enum_item body: (_))
   (union_item body: (_))
   (trait_item body: (_))
   (impl_item body: (_))
 ] @class @_shadowed)

(parameters
  (_) @argument)

(type_parameters
  (_) @argument)

(type_arguments
  (_) @argument)

(closure_parameters
  (_) @argument)

(arguments
  (_) @argument)

(field_initializer_list
  (_) @argument)

(array_expression
  (_) @entry)

(tuple_expression
  (_) @entry)

(tuple_pattern
  (_) @entry)

; The elements of `vec![...]'.

((macro_invocation
   (identifier) @_name
   (token_tree
     (_) @entry))
 (#eq? @_name "vec"))

(enum_variant) @entry

(field_declaration) @entry

(field_initializer) @entry

(shorthand_field_initializer) @entry

[
  (line_comment)
  (block_comment)
] @comment
