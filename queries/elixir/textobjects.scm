; Text object patterns of Hel for Elixir.
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;
; The file derives from runtime/queries/elixir/textobjects.scm of Helix,
; https://github.com/helix-editor/helix, at the commit ba40e547426b.

; A function head and a guard have no body, so the arguments are optional.

((call
   target: (identifier) @_keyword
   (arguments
     [
       (call
         (arguments (_)? @argument))
       ; The function has a guard.
       (binary_operator
         left: (call
                 (arguments (_)? @argument)))
     ])?)
 (#match? @_keyword "^\\(def\\|defdelegate\\|defguard\\|defguardp\\|defmacro\\|defmacrop\\|defn\\|defnp\\|defp\\)$")) @function

(anonymous_function
  (stab_clause right: (body))) @function

((call
   target: (identifier) @_keyword
   (do_block))
 (#match? @_keyword "^\\(defmodule\\|defprotocol\\|defimpl\\)$")) @class

(comment) @comment
