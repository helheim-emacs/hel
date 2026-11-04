; Hel's own text object patterns for Ruby.
;
; This file belongs to Hel and is licensed under GPL-3.0-or-later, unlike
; the Helix file textobjects.scm next to it.
;
; SPDX-License-Identifier: GPL-3.0-or-later
;
; A method after a visibility modifier, as in `private def m': the object
; holds the modifier. A match replaces the match whose around capture is
; the node of `@_extends'.

(call
  method: (identifier) @_modifier
  arguments: (argument_list
               .
               [
                 (method)
                 (singleton_method)
               ] @_extends
               .)
  (#match? @_modifier "^(private|protected|public|module_function|private_class_method|public_class_method)$")) @function.around
