;;;
;;;	The Balsa Asynchronous Hardware Synthesis System
;;;	Copyright (C) 1995-2003 Department of Computer Science
;;;	The University of Manchester, Oxford Road, Manchester, UK, M13 9PL
;;;	
;;;	This program is free software; you can redistribute it and/or modify
;;;	it under the terms of the GNU General Public License as published by
;;;	the Free Software Foundation; either version 2 of the License, or
;;;	(at your option) any later version.
;;;	
;;;	This program is distributed in the hope that it will be useful,
;;;	but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;	GNU General Public License for more details.
;;;	
;;;	You should have received a copy of the GNU General Public License
;;;	along with this program; if not, write to the Free Software
;;;	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;;
;;;	`doc-component-docs.scm'
;;;	Make Breeze component spec. document lout source
;;;
;;;	$Id: doc-component-docs.scm,v 1.4 2003/02/08 19:39:43 bardslea Exp $
;;;

(balsa-scheme-import 'brz 'parser)

;;; Procedures used to write lout

;;; write-integer-value: a literal or a param (print the param name)
(define write-integer-value (lambda (value)
	(if (headed-list? value 'param)
		(print (cadr value)) ; unresolved param value
		(print value)
	)
))

;;; write-unresolved-slice: if both lowIndex and elementCount are literals, print using the
;;;   format n..m. Otherwise print the parameter name on elementCount (lowIndex is assumed to be 0)
(define write-unresolved-slice (lambda (lowIndex elementCount)
	(if (and (integer? lowIndex) (integer? elementCount))
		(print lowIndex ".." (+ lowIndex elementCount -1))
		(print (cadr elementCount)) ; elementCount must be a param
	)
))

;;; write-type-in-breeze-format: write a type name in the Breeze notation
(define write-type-in-breeze-format (lambda (type)
	(cond
		((headed-list? type 'named-type) ; named type, just display the name
			(print (cadr type)))
		((headed-list? type 'numeric-type) ; numeric type: n [signed] bits
			(write-integer-value (caddr type)) ; number of bits
			(if (cadr type) (print "signed")) ; signedness
			(print " bits"))
		((headed-list? type 'array-type) ; arrays, array n..m of type
			(print "array ")
			(write-unresolved-slice (caddr type) (car (cdddr type)))
			(print " of ")
			(write-type-in-breeze-format (cadr type)))
		((headed-list? type 'string) ; strings
			(print "string"))	
	)
))

;;; write-parameter-in-breeze-format: write a parameter decl. in the Breeze notation
(define write-parameter-in-breeze-format (lambda (param)
	(print "parameter " (car param) " : ")
	(write-type-in-breeze-format (cadr param))
))

;;; write-port-in-breeze-format: write a port decl in the Breeze notation
(define write-port-in-breeze-format (lambda (port)
	(cond
		((headed-list? port 'sync-port) ; sense sync name
			(print (caddr port) " sync " (cadr port))
		)
		((headed-list? port 'port) ; sense direction name : type
			(print (caddr port) " " (car (cdddr port)) " " (cadr port) " : ")
			(write-type-in-breeze-format (cadr (cdddr port)))
		)
		((headed-list? port 'arrayed-sync-port) ; array n..m of sense sync name
			(print "array ")
			(write-unresolved-slice (car (cdddr port)) (cadr (cdddr port)))
			(print " of " (caddr port) " sync " (cadr port))
		)
		((headed-list? port 'arrayed-port) ; array n..m of sense direction name : type
			(print "array ")
			(write-unresolved-slice (caddr (cdddr port)) (car (cdddr (cdddr port))))
			(print " of " (caddr port)  " " (car (cdddr port)) " " (cadr port) " : ")
			(write-type-in-breeze-format (cadr (cdddr port)))
		)
	)
))

;;; write-primitive-part-in-breeze-format: write out the parameters and port specification
;;;	  of the given component in the breeze notation
(define write-primitive-part-in-breeze-format (lambda (component)
	(print "part $Brz" (cadr component) " (")
	; parameters
	(for-each (lambda (param) (newline) (print "  ") (write-parameter-in-breeze-format param))
		(cdr (caddr component)))
	; place the parameter/port separator
	(if (not (null? (caddr component))) (print " |"))
	; ports
	(for-each (lambda (port) (newline) (print "  ") (write-port-in-breeze-format port))
		(cdar (cdddr component)))
	(newline)
	(print ")")
	(newline)
))

(define components (read-list (open-input-file breeze-primitives-file)))

;;; Check components
(if (not (apply eager-and (map brz-check-primitive-part? components)))
	(begin
		(display "component file does not contain a valid list of primitive-parts")
		(newline)
	)
)

(with-output-to-file "components.txt" (lambda ()
	(for-each (lambda (c) (newline) (write-primitive-part-in-breeze-format c)) components)
))
