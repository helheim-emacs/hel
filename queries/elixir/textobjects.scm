; Text object patterns of Hel for Elixir.
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;
; The file derives from runtime/queries/elixir/textobjects.scm of Helix,
; https://github.com/helix-editor/helix, at the commit in ../HELIX-REVISION.

; Function heads and guards have no body at all, so `keywords` and `do_block` nodes are both optional
((call
   target: (identifier) @_keyword
   (arguments
     [
       (call
         (arguments (_)? @parameter.inside))
       ; function has a guard
       (binary_operator
         left:
           (call
             (arguments (_)? @parameter.inside)))
     ]
     ; body is "do: body" instead of a do-block
     (keywords
       (pair
         value: (_) @function.inside))?)?
   (do_block (_)* @function.inside)?)
 (#match? @_keyword "^\\(def\\|defdelegate\\|defguard\\|defguardp\\|defmacro\\|defmacrop\\|defn\\|defnp\\|defp\\)$")) @function.around

(anonymous_function
  (stab_clause right: (body) @function.inside)) @function.around

((call
   target: (identifier) @_keyword
   (do_block (_)* @class.inside))
 (#match? @_keyword "^\\(defmodule\\|defprotocol\\|defimpl\\)$")) @class.around

((call
  target: (identifier) @_keyword
  (arguments ((string) . (_)?))
  (do_block (_)* @test.inside)?)
 (#match? @_keyword "^\\(test\\|describe\\)$")) @test.around

(comment) @comment.around @comment.inside
