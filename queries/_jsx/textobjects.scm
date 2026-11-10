; Text object patterns of Hel for JSX in JavaScript and TSX.
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;
; The file derives from runtime/queries/_jsx/textobjects.scm of Helix,
; https://github.com/helix-editor/helix, at the commit in ../HELIX-REVISION.

(jsx_self_closing_element) @xml-element.around @xml-element.inside

(jsx_element (jsx_opening_element) (_)* @xml-element.inside (jsx_closing_element))

(jsx_element) @xml-element.around
