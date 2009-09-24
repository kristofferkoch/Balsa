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
;;;	`net-compass.scm'
;;;	Compass NLS netlist generation
;;;
;;;	$Id: net-compass.scm,v 1.20 2003/02/08 19:39:43 bardslea Exp $
;;;

(balsa-scheme-import 'net)
(balsa-scheme-import 'misc 'date)

;;; compass-netlist-signature: defaults for Compass netlists
(define compass-netlist-signature (lambda () (list
	compass-write-netlist-file ; write-netlist-file
	"!VDD" ; vcc-net-name
	#f ; vcc-component
	"!VSS" ; gnd-net-name
	#f ; gnd-component
	`(nets ("!VDD" 1) ("!VSS" 1) ("BULK" 1)) ; global nets
	compass-map-name ; map-name
	compass-bundle-name ; bundle-name
	"nls" ; filename-suffix
	compass-channel-name ; channel-name
	compass-bus-suffix ; bus-suffix
	"brz" ; brz-prefix
	"brc" ; brz-core-prefix
	"balsa_" ; balsa-prefix
	#f ; write-symbol-file
	"" ; symbol-filename-suffix
)))

;;; compass-map-name: map a string of the form [_A-Za-z][_A-Za-z0-9]* into
;;;		a valid compass (case insensitive) identifier.
;;;		eg. (compass-map-name "_HelloWorld") => "___hello_world"
(define compass-map-name (lambda (str)
	str
	(apply string-append (map (lambda (char)
		(cond
			((or (char=? #\_ char) (char-upper-case? char)) (string #\_ (char-downcase char)))
			((or (char-numeric? char) (char-lower-case? char)) (string char))
			(else ; gimme hex
				(string-append "_" (radix-string 16 (remainder (char->integer char) 128) 2
					lowercase-radix-characters))
			)
		)
	) (string->list str)))
))

;;; compass-portion-suffix: name suffix used for portion `portion'
(define compass-portion-suffix (lambda (portion)
	(case portion
		((node) "n")
		((req) "r")
		((ack) "a")
		((data) "d")
		((req0) "r0d")
		((req1) "r1d")
		((req2) "r2d")
		((req3) "r3d")
		((ack0) "a0d")
		((ack1) "a1d")
		((ack2) "a2d")
		((ack3) "a3d")
	)
))

;;; compass-bundle-name: transform a bundle name <name> which is index <index> in an arrayed
;;;		port or bundle (or #f if not arrayed) corresponding to the request/acknowledge/data portion
;;;		of that port. eg. (compass-bundle-name 'req "inp" 4) => "inp_4r"
;;;		Case smashes names to lower case and produced identifiers which are always distinct
;;;		from the given name.
;;;		If the name begins with #\! then just return the name verbatim
(define compass-bundle-name (lambda (portion name index)
	(if (eqv? (string-ref name 0) #\!)
		(substring name 1 (string-length name))
		(string-append
			(compass-map-name name)
			(string #\_)
			(if index (number->string index) "")
			(compass-portion-suffix portion)
		)
	)
))

;;; compass-channel-name: give a name to portion `portion' of channel number `channel-no'
;;;		eg. (compass-channel-name 'req 34) => "c34_r"
(define compass-channel-name (lambda (portion channel-no)
	(string-append "c" (number->string channel-no) "_" (compass-portion-suffix portion))
))

;;; compass-bus-suffix: produce a valid suffix for a compass vector signal (eg. a data portion
;;;		of a bundle) name. eg. (compass-bus-suffix 4 3) => "[6:4]"
;;;		`low-index' and `width' specify the size of the bus slice, `cardinality' should
;;;		be the width of the whole bus from which this is a slice.  The slice is always
;;;		given in [(low-index+cardinality-1):low-index] form.
(define compass-bus-suffix (lambda (low-index width cardinality)
	(if (= 1 cardinality)
		""
		(string-append
			(string #\[)
			(if (= 1 width)
				""
				(string-append (number->string (+ width low-index -1)) (string #\:))
			)
			(number->string low-index)
			(string #\])
		)
	)
))

;;; compass-date: give the date of the given time-t in the compass format:
;;;		eg. ("DD-MMM-YY TZZ" . "HH:MM:SS TZZ")
;;;		Y2K fix, pity Compass doesn't work: DD-MM-YYYY
(define compass-date (lambda (time-t timezone-name)
	(let
		((two-digit-pad (lambda (val) (overwrite-end-of-string "00" (number->string val))))
		 (date (gmtime time-t))
		)
		(cons
			(string-append (two-digit-pad (expanded-date:mday date)) "-" 
				(month-name (expanded-date:mon date))
				"-" (number->string (+ 1900 (expanded-date:year date))) " " timezone-name)
			(string-append (two-digit-pad (expanded-date:hour date)) ":" (two-digit-pad (expanded-date:min date))
				":" (two-digit-pad (expanded-date:sec date)) " " timezone-name)
		)
	)
))

;;; Compass netlist decoration:

;;; compass-expand-bus: (name n) -> ("name[n-1]" "name[n-2]" ... "name[0]")
(define compass-expand-bus (lambda (name cardinality)
	(letrec
		((expand-bus-tail (lambda (index expanded)
			(if (= index cardinality)
				expanded
				(expand-bus-tail (+ 1 index)
					(cons (string-append name (compass-bus-suffix index 1 cardinality)) expanded))
			)
		)))
		(expand-bus-tail 0 '())
	)
))

;;; compass-print-expanded-bus: (name n) -> printed list of prefix "name[n-1]" prefix "name[n-2]"
;;;		... prefix "name[0]"
(define compass-print-expanded-bus (lambda (out name cardinality prefix)
	(for.. (lambda (index)
		(print-to-port out prefix name (compass-bus-suffix (- cardinality index) 1 cardinality))
	) 1 cardinality)
))

;;; net-annotated-net:{wire-index,net-type}: accessor functions for annotated version
;;;		of net-net generated by compass-annotate-nets
(define net-annotated-net:wire-index caddr)
(define net-annotated-net:net-type cadddr)

;;; compass-annotate-nets: take a list of ports and nets and produce a list of
;;;		(name cardinality wire-index net-type) quads headed with 'nets for each net where `wire-index' is the net
;;;		number attributed to the highest index wire in each net.  Returns a pair:
;;;		(quad-list . next-free-net-number) where `next-free-net-number' is the first net
;;;		number which can be used for expressing unconnected/internal nets.  The net-type field is set to
;;;		one of "X", "G" or "N" to distinguish ports, global nets and local nets
(define compass-annotate-nets (lambda (ports nets)
	(let*
		; annonate-nets: returns a pair (next-free-net-no . annotated-nets)
		((annotate-nets (lambda (nets first-net-no net-type)
			(map-accuml (lambda (index net)
				(let*
					((cardinality (net-net:cardinality net))
					 ; trim leading ! off globals
					 (name (net-net:name net))
					 (bare-net-name (if (eqv? (string-ref name 0) #\!)
						(substring name 1 (string-length name)) name))
					)
					(cons
						(+ index cardinality)
						(list bare-net-name cardinality index net-type)
					)
				)
			) first-net-no (cdr nets))
		 ))
		 (annotated-ports (annotate-nets (net-ports->nets ports) 1 "X"))
		 ; vectored-port-count: number of ports with 2 or more wires
		 (vectored-port-count (foldl (lambda (count port)
			(if (< 1 (net-net:cardinality port)) (+ 1 count) count)
		 ) 0 (cdr annotated-ports)))
		 ; NB. leave a gap of one wire for each vectored port
		 (annotated-globals (annotate-nets tech-global-nets (+ vectored-port-count (car annotated-ports)) "G"))
		 (annotated-locals (annotate-nets nets (car annotated-globals) "N"))
		)
		(cons
			(cons 'nets (append (cdr annotated-ports) (cdr annotated-globals) (cdr annotated-locals)))
			(car annotated-locals)
		)
	)
))

;;; compass-net:{net-number,net-type}: extract the compass net number/type from a net-net
(define compass-net:net-number caddr)
(define compass-net:net-type cadddr)

;;; compass-print-expanded-connection: expand out a net-instance-connection to a list a net numbers (which
;;;		is printed with a prefixing space) returns the next-unconnected-net-number.  `out' is the port
;;;		to print to.  NB. `sorted-nets' should be a name sorted vector of net-nets.
(define compass-print-expanded-connection (lambda (out sorted-nets unconnected-net-number connection)
	(cond
		((headed-list? connection 'vector)
			(compass-print-expanded-connections out sorted-nets unconnected-net-number (cdr connection))
		)
		((string? connection)
			(print-to-port out #\space (compass-net:net-number (net-find-sorted-net sorted-nets connection)))
			unconnected-net-number
		)
		((pair? connection)
			(let*
				((net (net-find-sorted-net sorted-nets (car connection)))
				 (connection-zero-index-net-number (- (+ -1 (compass-net:net-number net)
					(net-net:cardinality net)) (cadr connection)))
				 (connection-max-index-net-number (+ 1 (- connection-zero-index-net-number
					(if (null? (cddr connection)) 1 (caddr connection)))))
				)
				(for.. (lambda (v) (print-to-port out #\space v)) connection-max-index-net-number
					connection-zero-index-net-number)
				unconnected-net-number
			)
		)
		((eqv? 'unconnected connection)
			(print-to-port out #\space unconnected-net-number) (+ 1 unconnected-net-number)
		)
	)
))

;;; compass-print-expanded-connections: apply compass-print-expanded-connection to a list of connections
(define compass-print-expanded-connections (lambda (out annotated-nets unconnected-net-number connections)
	(foldl (lambda (unconnected connection)
		(compass-print-expanded-connection out annotated-nets unconnected connection)
	) (cons '() unconnected-net-number) connections)
))

;;; compass-ports-sense-string: make a ports sense string (eg. ioi for input,output,input) from a ports list
(define compass-ports-sense-string (lambda (ports)
	(let*
		((ports-width (foldl (lambda (width port) (+ width (net-port:cardinality port))) 0 (cdr ports)))
		 (port-direction-char (lambda (port)
			(case (net-port:direction port)
				((input) #\i)
				((output) #\o)
				((inout) #\u)
				((hiz) #\u)
				(else #\u)
			)
		 ))
		 (sense-string (make-string ports-width #\u))
		) ; Fill out sense-string with direction bits
		(foldl (lambda (index port)
			(let
				((direction-char (port-direction-char port))
				 (end-index (+ index (net-port:cardinality port)))
				)
				(do ((i index (+ 1 i))) ((= i end-index) end-index)
					(string-set! sense-string i direction-char)
				)
			)
		) 0 (cdr ports))
		sense-string
	)
))

;;; compass-print-map-line: make and print a M (map) line including the component name,
;;;		port sense list and port names.  `out' is an output port to print to.
(define compass-print-map-line (lambda (out component)
	(let
		((name (net-circuit-decl:name component))
		 (ports (net-circuit-decl:ports component))
		 (nets (net-circuit-decl:nets component))
		)
		(print-to-port out "M NLS " (net-circuit-decl:name component) " * |")
		(for-each (lambda (port)
			(compass-print-expanded-bus out (net-port:name port) (net-port:cardinality port) " ")
		) (cdr ports))
		(print-to-port out " | " (compass-ports-sense-string ports) semi-cr)
	)
))

;;; compass-print-net-lines: print a list of net `X/N/G' lines for the given net.  `post' is a string
;;;		to append to each net line.
(define compass-print-net-lines (lambda (out net post)
	(let
		((name (net-net:name net))
		 (type (compass-net:net-type net))
		 (number (compass-net:net-number net))
		 (cardinality (net-net:cardinality net))
		)
		(if (= 1 cardinality)
			(print-to-port out type " " number " " name post semi-cr)
			(let ((last-net-number (+ number cardinality -1)))
				(for.. (lambda (net-number)
					(print-to-port out type " " net-number " " name (compass-bus-suffix
						(- last-net-number net-number) 1 cardinality) post semi-cr)
				) number last-net-number)
			)
		)
	)
))

;;; Compass netlist files contain:
;;;		1) manager lines
;;;		2) preamble + table of contents
;;;		3) a contents block
;;;		4) a netlist block
;;;		5) a subcells block

;;; {cr,semi-cr}: shorthand strings
(define cr (string #\newline))
(define semi-cr (string #\; #\newline))

(define compass-max-line-length 79)
(define compass-file-preamble (string-append
	"writer        1" cr
	"file_version  1" cr
	"sections      1" cr
))
(define compass-contents-block-tail (string-append
	cr
	"switches            0" cr
	"attributes          0" cr
	"physical            0" cr
	"history             0" cr
	"E contents" cr cr cr
))

;;; compass-write-single-netlist-file: write a single netlist file for a single cell
(define compass-write-single-netlist-file (lambda (circuits cell filename)
	(let*
		((date (compass-date (current-time) "GMT"))
		 (ports (net-circuit-decl:ports cell))
		 (cell-name (net-circuit-decl:name cell))
		 (port-count (length (cdr ports)))
		 (nets (net-circuit-decl:nets cell))
		 (instances (net-circuit-decl:instances cell))
		 (annotated-nets/unconnected-net-number (compass-annotate-nets ports nets))
		 (annotated-nets (car annotated-nets/unconnected-net-number))
		 (annotated-net-count (length (cdr annotated-nets)))
		 ;;; sorted-annotated-nets: a vector of nets sorted by name for faster lookups
		 (sorted-annotated-nets (list->vector
			(merge-sort net-net:name<=? (cdr annotated-nets))
		 ))
		 (unconnected-net-number (cdr annotated-nets/unconnected-net-number))
		 ;;; print-with-semis: print lines with ; as a line terminator
		 (print-with-semis (lambda (port l)
			(for-each (lambda (elem) (print-to-port port elem semi-cr)) l)
		 ))
		 ;;; sub-circuits: sub cells of this circuit, sort and uniq the list
		 (sub-circuits (uniq string=? (merge-sort! string<=? (map net-instance:name (cdr instances)))))
		 ;;; print-map-lines: print the M lines
		 (print-map-lines (lambda (port)
			(for-each (lambda (cell-name)
				(compass-print-map-line port (net-find-expected-circuit circuits cell-name))
			) sub-circuits)
		 ))
		 ;;; print-net-lines: net declaration lines, ports/nets and globals
		 (print-net-lines (lambda (port)
		 	(let print-net-step
		 		((annotated-nets (cdr annotated-nets))
		 		 (ports (cdr ports))
		 		)
				(if (not (null? annotated-nets))
					(let
						((direction-string (if (null? ports)
							""
							(case (net-port:direction (car ports))
								((input) " i")
								((output) " o")
								(else " u")
							)
						)))
						(compass-print-net-lines port
							(car annotated-nets) direction-string)
						(print-net-step (cdr annotated-nets)
							(safe-cdr ports))
					)
				)
			)
		 ))
		 ;;; print-compass-instances: compass netlist lines for instances ("I name ..." ...)
		 (print-compass-instances (lambda (port) (foldl-ma (lambda (instance index u-n-n)
			(print-to-port port
				"I NLS " (net-instance:name instance) " * * " index " U" index " |"
			)
			(let
				((new-u-n-n (compass-print-expanded-connections port sorted-annotated-nets u-n-n
					(net-instance:instance-connections instance)))
				)
				(print-to-port port semi-cr)
				(list
					(+ 1 index)
					new-u-n-n
				)
			)
		 ) (cdr instances) 1 unconnected-net-number)))
		 ;;; print-compass-vectors: V, vector defn. lines
		 (print-compass-vectors (lambda (port) (for-each (lambda (vector)
			(let
				((name (net-net:name vector))
				 (cardinality (net-net:cardinality vector))
				 (net-number (compass-net:net-number vector))
			 	)
				(if (< 1 cardinality)
					(begin
						(print-to-port port "V " name " " (- cardinality 1) " 0 |")
						(for.. (lambda (index) (print-to-port port " " index))
							net-number (+ net-number cardinality -1)
						)
						(print-to-port port semi-cr)
					)
				)
			)
		 ) (cdr annotated-nets))))
		 ;;; file fragments, don't expect to understand this
		 (netlist-toc-line (string-copy "netlist   1:          0"))
		 (netlist-name (string-append " " cell-name cr cr cr))
		 (netlist-name-length (string-length netlist-name))
		 (netlist-contents (string-append "B contents: " cell-name cr "netlist             0" ))
		 (subcells-contents (string-append cr "cells               0"))
		 ;;; output-file
		 (tmp-filename (string-append filename "-tmp"))
		 (out (open-output-file tmp-filename))
		)
		(net-add-file-to-session-file-list filename)
;(print-err "Started " (current-time) cr)
		(print-to-port out
			"#cell2 * " cell-name " nls * 1 any 0 " compass-version cr
			"# \"" (car date) "\" \"" (cdr date) "\" \"" (car date) "\" \"" (cdr date) "\" bardslea * ." cr
			compass-file-preamble
			netlist-toc-line
			netlist-name
			netlist-contents
			subcells-contents
			compass-contents-block-tail
		)
		(print-to-port out "B netlist" cr)
;(print-err "Done pre-amble " (current-time) cr)
		(print-net-lines out)
;(print-err "Done net-lines " (current-time) cr)
		(print-compass-vectors out)
;(print-err "Done vectors " (current-time) cr)
		(print-map-lines out)
;(print-err "Printed map lines " (current-time) cr)
		(print-compass-instances out)
;(print-err "Printed instances " (current-time) cr)
		(print-to-port out "E netlist" cr cr cr)

		(print-to-port out "B subcells" cr)
		(for-each (lambda (sub-circuit)
			(print-to-port out "NLS " sub-circuit " *" cr)
		) sub-circuits)
		(print-to-port out "E subcells" cr cr cr)
		(close-output-port out)
		; split the lines in the file into the real filename target
		(system (string-append "balsa-split-lines -c " (number->string compass-max-line-length)
			" -l '-' -d '#' " filename "-tmp > " filename))
		(system (string-append "/bin/rm " tmp-filename))
	)
))

;;; compass-version: version string to use in manager lines in netlists
(define compass-version "v8r4.10.0")

;;; Features for netlist generation.
(define compass-included-cell-types '())

;;; compass-reset-feature: reset feature settings back to defaults
(define compass-reset-features (lambda ()
	(set! compass-included-cell-types '())
))

;;; compass-write-netlist-file: write a set of netlist files with the prefix `filename-'
;;;		for circuit(s) `top-circuits' using `circuits' provides references for sub-circuit components.
;;;		`top-circuits' can either be a single circuit or a list of circuits.
;;;		Supports the following features:
;;;			No features
(define compass-write-netlist-file (lambda (circuits top-circuits design-name filename . features)
	(compass-reset-features)
	(for-each (lambda (feature)
		(if (not (pair? feature))
			(error "compass-write-netlist-file: features must be (name . value) pairs")
			(case (car feature)
				((included-cell-types) (set! compass-included-cell-types (cdr feature)))
			)
		)
	) features)
	(let*
		((top-circuits-list (if (headed-list? top-circuits 'circuit) (list top-circuits) top-circuits))
		 (all-circuits (net-flatten-circuit-defns top-circuits-list circuits
			(cons "none" compass-included-cell-types)))
		)
; (for-each (lambda (i) (print-err (net-circuit-decl:name (car i)) " " (cdr i) #\newline)) all-circuits)
		(for-each (lambda (circuit/defn)
			(let ((circuit (car circuit/defn)) (defn (cdr circuit/defn)))
				(if (and defn (not (null? (cdr (net-circuit-decl:instances circuit)))))
					(compass-write-single-netlist-file circuits circuit (string-append
						(net-circuit-decl:name circuit) "." tech-filename-suffix))
				)
			)
		) all-circuits)
	)
))
