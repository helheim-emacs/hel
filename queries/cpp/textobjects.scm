; Text object patterns of Hel for C++.
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;
; The file derives from runtime/queries/cpp/textobjects.scm of Helix,
; https://github.com/helix-editor/helix, at the commit ba40e547426b.

; inherits: c

(lambda_expression
  body: (_)) @function

(class_specifier
  body: (_)) @class

; A class declared on its own, with or without a body: the object holds
; the `;' after it. A forward declaration such as `class C;' has no body.

((class_specifier) @class @_shadowed
 .
 ";" @class)

; A template: the object holds the `template <...>' line.

(template_declaration
  (function_definition) @_shadowed) @function

(template_declaration
  [
    (class_specifier)
    (struct_specifier)
  ] @_shadowed) @class
