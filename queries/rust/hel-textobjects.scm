; Hel's own text object patterns for Rust.
;
; This file belongs to Hel and is licensed under GPL-3.0-or-later, unlike
; the Helix file textobjects.scm next to it.
;
; SPDX-License-Identifier: GPL-3.0-or-later
;
; The Helix patterns put the attributes above a function into the object,
; and leave them out for the other items. This pattern puts the attributes
; above a struct, an enum, a union, a trait or an impl into the object, so
; that `#[derive(...)]' goes with its type. A match replaces the match whose
; around capture is the node of `@_extends'.

((attribute_item)+ @class.around
 .
 [
   (struct_item body: (_))
   (enum_item body: (_))
   (union_item body: (_))
   (trait_item body: (_))
   (impl_item body: (_))
 ] @class.around @_extends)
