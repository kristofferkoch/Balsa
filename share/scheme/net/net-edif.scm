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
;;;	`net-edif.scm'
;;;	EDIF 2 0 0 Netlist Generation
;;;
;;;	$Id: net-edif.scm,v 1.27 2003/11/13 15:46:45 bardslea Exp $
;;;

(balsa-scheme-import 'net)
(balsa-scheme-import 'net 'connections)
(balsa-scheme-import 'misc 'date)

;;; edif-netlist-signature: defaults for EDIF netlists
(define edif-netlist-signature (lambda () (list
	edif-write-netlist-file ; write-netlist-file
	"!balsa_vcc" ; vcc-net-name
	#f ; vcc-component
	"!balsa_gnd" ; gnd-net-name
	#f ; vcc-component
	'(nets ("balsa_vcc" 1) ("balsa_gnd" 1)) ; global nets
	edif-map-name ; map-name
	edif-bundle-name ; bundle-name
	"edn" ; filename-suffix
	edif-channel-name ; channel-name
	edif-bus-suffix ; bus-suffix
	"brz" ; brz-prefix
	"brc" ; brz-core-prefix
	"balsa_" ; balsa-prefix
	#f ; write-symbol-file
	"" ; symbol-filename-suffix
)))

;;; edif-tab: tab characters, keep 'em short for small files
(define edif-tab " ")

;;; edif-internal-map-name: case and non alphanumeric smash a string
(define edif-internal-map-name (lambda (str)
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

;;; edif-map-name: map a string of the form [_A-Za-z][_A-Za-z0-9]* into
;;;		a valid edif identifier.
;;;		eg. (edif-map-name "_HelloWorld") => "___hello_world"
; (define edif-map-name edif-internal-map-name)
(define edif-map-name edif-internal-map-name)

;;; edif-real-map-name: really map names into stupid EDIF ids.
(define edif-real-map-name (lambda (str) ; FIXME, make this much much more efficient
	(let
		((mapped-name (edif-internal-map-name str)))
		; Add the & to _ prefixed names
		(if (and (>= (string-length mapped-name) 1) (char=? #\_ (string-ref mapped-name 0)))
			(string-append "&" mapped-name)
			mapped-name
		)
	)
))

;;; edif-flat-map-bus-element-name: map a bus name/element index into a flat
;;;		EDIF name of the form <real-mapped-name(str)>_<index>
(define edif-flat-map-bus-element-name (lambda (str index)
	(string-append (edif-real-map-name str) "_" (number->string index))
))

;;; edif-map-bus-element-name: ditto for bus elements
(define edif-map-bus-element-name (lambda (str index)
	(if edif-use-arrays-for-buses
		(string-append "(member " (edif-real-map-name str) " " (number->string index) ")")
		(edif-flat-map-bus-element-name str index)
	)
))

;;; edif-rename: returns a string "(rename mangled-name "orig-name")"
(define edif-rename (lambda (str)
	(string-append "(rename " (edif-real-map-name str) " \"" str "\")")
))

;;; edif-rename-bus-element: rename a bus element A 1 => &_a_1, A[1]
(define edif-rename-bus-element (lambda (str index)
	(string-append "(rename " (edif-flat-map-bus-element-name str index) " \"" str "["
		(number->string index) "]\")")
))

;;; edif-portion-suffix: name suffix used for portion `portion'
(define edif-portion-suffix (lambda (portion)
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

;;; edif-bundle-name: transform a bundle name <name> which is index <index> in an arrayed
;;;		port or bundle (or #f if not arrayed) corresponding to the request/acknowledge/data portion
;;;		of that port. eg. (edif-bundle-name 'req "inp" 4) => "inp_4r"
;;;		Case smashes names to lower case and produced identifiers which are always distinct
;;;		from the given name.
;;;		If the name begins with #\! then just return the name verbatim
(define edif-bundle-name (lambda (portion name index)
	(if (eqv? (string-ref name 0) #\!)
		(substring name 1 (string-length name))
		(string-append
			(edif-map-name name)
			(string #\_)
			(if index (number->string index) "")
			(edif-portion-suffix portion)
		)
	)
))

;;; edif-channel-name: give a name to portion `portion' of channel number `channel-no'
;;;		eg. (edif-channel-name 'req 34) => "c34_r"
(define edif-channel-name (lambda (portion channel-no)
	(string-append "c" (number->string channel-no) "_" (edif-portion-suffix portion))
))

;;; edif-bus-suffix: produce a valid suffix for a edif vector signal (eg. a data portion
;;;		of a bundle) name. eg. (edif-bus-suffix 4 3) => "[6:4]"
;;;		`low-index' and `width' specify the size of the bus slice, `cardinality' should
;;;		be the width of the whole bus from which this is a slice.  The slice is always
;;;		given in [(low-index+width-1):low-index] form.
(define edif-bus-suffix (lambda (low-index width cardinality)
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

;;; edif-date: give the date of the given time-t in the edif format:
;;;		eg. "YYYY MM DD HH MM SS"
(define edif-date (lambda (time-t)
	(let
		((date (gmtime time-t)))
		(string-append
			(number->string (+ 1900 (expanded-date:year date))) " "
			(number->string (expanded-date:mon date)) " "
			(number->string (expanded-date:mday date)) " "
			(number->string (expanded-date:hour date)) " "
			(number->string (expanded-date:min date)) " "
			(number->string (expanded-date:sec date))
		)
	)
))

;;; Compass netlist decoration:

;;; edif-port-direction-strings: strings to print for the various port dirs.
(define edif-port-direction-strings '(
	(input . "INPUT") (output . "OUTPUT") (inout . "INOUT") (hiz . "INOUT")
))

;;; edif-print-instances: print instance lines in (contents ...)
(define edif-print-instances (lambda (out tabs instances)
	(let
		((print-instance (lambda (circuit-name instance-name)
			(print-to-port out #\newline
				tabs "(instance " instance-name " (viewRef net (cellRef " (edif-real-map-name circuit-name) ")))")
		)))
		(foldl (lambda (instance-no instance)
			(print-instance (net-instance:name instance) (string-append "I" (number->string instance-no)))
			(+ 1 instance-no)
		) 0 (drop-head-symbol 'instances instances))
	)
))

;;; edif-print-port-decls: print port declarations eg. "(port (rename name name) (direction dir))"
(define edif-print-port-decls (lambda (out tabs ports)
	(let
		((print-port (lambda (name direction)
			(print-to-port out #\newline
				tabs "(port " name " (direction "
					(cdr (assv direction edif-port-direction-strings)) "))"
			)
		)))
		(for-each (lambda (port)
			(if (= 1 (net-port:cardinality port))
				(print-port (edif-rename (net-port:name port)) (net-port:direction port))
				(for.. (lambda (i)
					(let ((name (net-port:name port)))
						(print-port
							(edif-rename-bus-element name i)
							(net-port:direction port)
						)
					)
				) 0 (- (net-port:cardinality port) 1))
			)
		) (if (and (not (null? ports)) (eqv? 'ports (car ports))) (cdr ports) ports))
	)
))

;;; edif-print-port-decls-with-arrays: print port declarations eg. "(port (rename name name) (direction dir))"
;;;		Uses (array portNameDef) where appropriate
(define edif-print-port-decls-with-arrays (lambda (out tabs ports)
	(let
		((print-port (lambda (cardinality name direction)
			(print-to-port out #\newline tabs "(port ")
			(if (/= 1 cardinality)
				(print-to-port out "(array ")
			)
			(print-to-port out name)
			(if (/= 1 cardinality)
				(print-to-port out " " cardinality ")")
			)
			(print-to-port out " (direction " (cdr (assv direction edif-port-direction-strings)) "))")
		)))
		(for-each (lambda (port)
			(print-port (net-port:cardinality port)
				(edif-rename (net-port:name port)) (net-port:direction port))
		) (if (and (not (null? ports)) (eq? 'ports (car ports))) (cdr ports) ports))
	)
))

;;; edif-print-single-connection: print an edif representation of a single port
;;;		connection triple (instance-index connection-index wire-index)
;;;		FIXME, this doesn't handle instance names yet
(define edif-print-single-connection (lambda (out tabs conn instances circuits)
	(let*
		((instance-index (net-conn-conn:instance-index conn))
		 (instance (list-ref instances instance-index))
		 (instance-ref (string-append "I" (number->string instance-index)))
		 (connection-index (net-conn-conn:connection-index conn))
		 (wire-index (net-conn-conn:wire-index conn))
		 (component (net-find-expected-circuit circuits (net-instance:name instance)))
		 (component-port (list-ref (net-circuit-decl:ports component) (+ 1 connection-index)))
		 (port-ref ; find the bit in the port
		 	(if (= 1 (net-port:cardinality component-port))
		 		(edif-real-map-name (net-port:name component-port))
					(edif-map-bus-element-name (net-port:name component-port) wire-index)
		 	)
		 )
		)
		(print-to-port out #\newline tabs "(portRef " port-ref " (instanceRef " instance-ref "))")
	)
))

;;; edif-print-net-connections: print net connections in the given instance/connections structures
(define edif-print-net-connections (lambda (out tabs instances connections circuits)
	(let
		 ; print-connection: print a single net connection, do nothing
		 ;		if the connection is null.
		((print-connection (lambda (fullname rename connection nature)
		 	(if (not (null? connection))
		 		(begin
					(print-to-port out #\newline tabs "(net " fullname #\newline
						tabs edif-tab  "(joined")
					; make a connection to the local port if this is a port connected net
					; EDIF seems to keep net and port names distinct, that makes one useful feature
					; so far!
					(if (memv nature net-port-directions)
						(print-to-port out #\newline tabs edif-tab edif-tab "(portRef " rename ")")
					)
					(for-each (lambda (conn)
						(edif-print-single-connection out (string-append tabs edif-tab edif-tab)
							conn instances circuits)
					) connection)
					(print-to-port out "))")
				)
		 	)
		 ))
		)
		(for.. (lambda (i)
			(let*
				((connection (vector-ref connections i))
				 (name (net-connection:name connection))
				)
				(if (= 1 (net-connection:cardinality connection))
					(print-connection
						(edif-rename name)
						(edif-real-map-name name)
						(vector-ref (net-connection:connections connection) 0)
						(net-connection:nature connection)
					)
					(for.. (lambda (i)
						(let
							((name (net-connection:name connection))
							 (rename (edif-map-bus-element-name name i))
							)
							(print-connection
								(edif-rename-bus-element name i)
								rename
								(vector-ref (net-connection:connections connection) i)
								(net-connection:nature connection)
							)
						)
					) 0 (- (net-connection:cardinality connection) 1))
				)
			)
		) 0 (- (vector-length connections) 1))
	)
))

;;; edif-print-circuit: print a circuit for a component from the library
;;;		print the circuit body if `body?' is true. `circuits' is a list of reference
;;;		circuits for use in connection printing.
;;;		Also tests attributes attached to those circuits
;;;		to decide.
;;;		Rules:
;;;			No instances => print module with just port structure
;;;			not edif-print-prototypes => don't even print port structures
;;;			Circuit has (cell-type ...) attribute included in edif-included-cell-types =>
;;;				print the definition
(define edif-print-circuit (lambda (out tabs circuit circuits body?)
	(let*
		((circuit-instances (drop-head-symbol 'instances (net-circuit-decl:instances circuit)))
		 (connections (net-make-connection-table circuit))
; (aa (print-err (current-time) ": made connection table" #\newline))
		 (vcc-net-name (edif-bundle-name 'data tech-vcc-net-name 0)) ; bundle-name smashes global net prefices!
		 (gnd-net-name (edif-bundle-name 'data tech-gnd-net-name 0))
		 (power-instances (let find-power-instances
		 	((index 0) (power-instances '()) (new-instance-no (length circuit-instances)))
			(if (= index (vector-length connections))
				power-instances
				(let*
					((conn (vector-ref connections index))
					 (add-power-connection (lambda (conn net-name component-name)
					 	(if (net-connection-any-connections? conn)
					 		(begin
								; add connection to the new power instance
								(vector-set! (net-connection:connections conn) 0 
									`((,new-instance-no 0 0) . ,(vector-ref (net-connection:connections conn) 0)))
								(find-power-instances (+ 1 index)
									(append power-instances `((instance ,component-name (,net-name))))
									(+ 1 new-instance-no)
								)
							)
							(find-power-instances (+ 1 index) power-instances new-instance-no)
						)
					 ))
					)
					(cond ; assume that power connections go to port 0 of the instance
						((string=? vcc-net-name (net-connection:name conn))
							(add-power-connection conn vcc-net-name tech-vcc-component-name)
						)
						((string=? gnd-net-name (net-connection:name conn))
							(add-power-connection conn gnd-net-name tech-gnd-component-name)
						)
						(else (find-power-instances (+ 1 index) power-instances new-instance-no))
					)
				)
			)
		 ))
		 (all-instances (append circuit-instances power-instances))
		 (print-definition? (and
			(or body?
				(let ((cell-type (net-circuit-decl-find-attribute circuit 'cell-type)))
					(and cell-type (member (cadr cell-type) edif-included-cell-types)))
			)
			(not (null? all-instances))
		 ))
		 (print-prototype? (or edif-print-prototypes print-definition?))
		)
		(if print-prototype?
			(begin
				(print-to-port out #\newline
					tabs "(cell " (edif-rename (net-circuit-decl:name circuit)) #\newline
					tabs edif-tab "(cellType GENERIC)" #\newline
					tabs edif-tab "(view net" #\newline
					tabs edif-tab edif-tab "(viewType NETLIST)" #\newline
					tabs edif-tab edif-tab "(interface"
				)
				((if edif-use-arrays-for-buses
					edif-print-port-decls-with-arrays
					edif-print-port-decls
				) out (string-append tabs edif-tab edif-tab edif-tab) (net-circuit-decl:ports circuit))
				(print-to-port out ")")
; (print-err (current-time) ": printing defn" #\newline)
				(if print-definition?
					(begin
						(print-to-port out #\newline tabs edif-tab edif-tab "(contents")
						(edif-print-instances out (string-append tabs edif-tab edif-tab edif-tab) all-instances)
; (print-err (current-time) ": printed instances" #\newline)
						(edif-print-net-connections out (string-append tabs edif-tab edif-tab edif-tab)
							all-instances connections circuits)
						(print-to-port out ")")
					)
				)
				(print-to-port out "))")
			)
		)
	)
))

;;; edif-print-header: print a boring header for an edif file
(define edif-print-header (lambda (out filename)
	(let
		((date (edif-date (current-time))))
		(print-to-port out "(edif (rename BALSA_NETLIST \"" filename "\")" #\newline
			edif-tab "(edifVersion 2 0 0)" #\newline
			edif-tab "(edifLevel 0)" #\newline
			edif-tab "(keywordMap (keywordLevel 0))" #\newline
			edif-tab "(status" #\newline
			edif-tab edif-tab "(written" #\newline
			edif-tab edif-tab edif-tab "(timeStamp " date ")" #\newline
			edif-tab edif-tab edif-tab "(program \"balsa\" (version \"" balsa-version "\"))))" #\newline
			edif-tab "(library balsa" #\newline
			edif-tab edif-tab "(edifLevel 0)" #\newline
			edif-tab edif-tab "(technology (numberDefinition))" 
		)
	)
))

;;; Features for netlist generation.
(define edif-print-prototypes #t)
(define edif-included-cell-types '())

;;; edif-reset-feature: reset feature settings back to defaults
(define edif-reset-features (lambda ()
	(set! edif-print-prototypes #t)
	(set! edif-included-cell-types '())
))

(define edif-use-arrays-for-buses #t)

;;; edif-write-netlist-file: write a netlist file `filename' for circuit(s) `top-circuits' using `top-circuit-name'
;;;		as that circuit's name in the file.  `circuits' provides references for sub-circuit components
;;;		NB. `top-circuits' can either be a list of circuit-decls or just a single one
;;;		If `top-circuit-name' is not #f then a design is declared on that circuit, NB. this circuit
;;;		should also be {,part of} the `top-circuits'.
;;;		Supports the following features:
;;;			(included-cell-types "type1" "type2" ...) => emit definitions of cells of these
;;;				types as well as the top level cells.
;;;			(emit-prototypes . bool) => choose whether to emit prototypes for used but not defined cells
(define edif-write-netlist-file (lambda (circuits top-circuits top-circuit-name filename . features)
	(let* ((out (open-output-file filename)))
		(net-add-file-to-session-file-list filename)
		(edif-print-header out filename)
		(edif-reset-features)
		(for-each (lambda (feature)
			(if (not (pair? feature))
				(error "edif-write-netlist-file: features must be (name . value) pairs")
				(case (car feature)
					((emit-prototypes) (set! edif-print-prototypes (cdr feature)))
					((included-cell-types) (set! edif-included-cell-types (cdr feature)))
				)
			)
		) features)
		(let*
			((top-circuits-list (if (headed-list? top-circuits 'circuit) (list top-circuits) top-circuits))
			 ;;; power-circuits: power connection circuit names
			 (power-circuits (if tech-vcc-component-name
			 	(cons (net-find-expected-circuit circuits tech-vcc-component-name)
			 	(if tech-gnd-component-name (list (net-find-expected-circuit circuits tech-gnd-component-name)) '()))
				'()
			 ))
			 (all-circuits (net-flatten-circuit-defns (append power-circuits top-circuits-list)
				circuits (cons "none" edif-included-cell-types)))
			 ;;; print-circuits: print prototype and defn circuits.
			 (print-circuits (lambda (port)
				(for-each (lambda (top-circuit/defn)
; (print-err (current-time) ": " (net-circuit-decl:name (car top-circuit/defn)) #\newline)
					(edif-print-circuit port (string-append edif-tab edif-tab)
						(car top-circuit/defn)
						circuits
						(cdr top-circuit/defn)
					)
				) all-circuits)
			 ))
			)
			(print-circuits out)	
			(print-to-port out ")") ; close (library ...)

			(if #f
				(print-to-port out #\newline edif-tab "(design " (edif-rename top-circuit-name) #\newline
					edif-tab edif-tab "(cellRef " (edif-real-map-name top-circuit-name) " (libraryRef balsa)))")
			)
			(print-to-port out ")" #\newline) ; close (edif ...)
		)
		(close-output-port out)
	)
))
