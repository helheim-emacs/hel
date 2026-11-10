; Text object patterns of Hel for CMake.
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;
; The file derives from runtime/queries/cmake/textobjects.scm of Helix,
; https://github.com/helix-editor/helix, at the commit in ../HELIX-REVISION.

[
  (macro_def)
  (function_def)
] @function.around

(argument) @parameter.inside

[
  (bracket_comment)
  (line_comment)
] @comment.inside

(line_comment) @comment.around

(bracket_comment) @comment.around
