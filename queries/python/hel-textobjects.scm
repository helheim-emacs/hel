; Hel's own text object patterns for Python.
;
; This file belongs to Hel and is licensed under GPL-3.0-or-later, unlike
; the Helix file textobjects.scm next to it.
;
; SPDX-License-Identifier: GPL-3.0-or-later
;
; A decorated function or class is a `decorated_definition' node, and the
; Helix patterns capture only the definition inside it. These patterns make
; the decorators a part of the object. A match replaces the Helix match whose
; around capture is the node of `@_extends'.

(decorated_definition
  definition: (function_definition) @_extends) @function.around

(decorated_definition
  definition: (class_definition) @_extends) @class.around
