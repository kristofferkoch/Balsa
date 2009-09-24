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
;;;	`gen-hcs.scm'
;;;	`gen' stage of balsa-netlist back end - Handshake component generation
;;;
;;;	$Id: gen-hcs.scm,v 1.43 2004/07/13 09:49:33 tomsw Exp $
;;;

(balsa-scheme-import 'net 'parser)
(balsa-scheme-import 'net 'connections)
(balsa-scheme-import 'gen 'gates)

;;; gen-bundle-bindings->net-nets: map a list of specified gen-bundle-bindings
;;;		into a list of net-net elements
(define gen-bundle-bindings->net-nets (lambda (bindings)
	(foldl append '() (map
		(lambda (node)
			(let
				((name (gen-bundle-binding:name node))
					(cardinality (gen-bundle-binding:cardinality node))
					(bundle-width-list (gen-bundle-binding:width-list node))
					(low-index (gen-bundle-binding:low-index node))
					(count (gen-bundle-binding:count node))
				)
				(if bundle-width-list
					(map.. (lambda (index card)
						(list (tech-bundle-name 'node name index) card)
					) low-index (+ -1 low-index count) bundle-width-list)
					(map (lambda (index)
						; FIXME, check connections, req/ack
						(list (tech-bundle-name 'node name index) cardinality)
					) (.. low-index (+ -1 low-index count)))
				)
			)
		) bindings
	))
))

;;; gen-make-hc-netlist: make a .net netlist for the named handshake component
;;;		(a generating description of which should be present in the primitives description file)
;;;		by applying the given parameters.
;;;		If `composition' is 'flat then only produce a Brz file, never a Brc
(define gen-make-hc-netlist (lambda (name params type-context composition)
	(let*
		((component (brz-find-primitive name)) 
		 (parameters (brz-normalise-parameter-list params))
		 ; formal-parameters: if the actual parameters list for this part is shorter
		 ;	than the formal parameters list then assume that only significant parameters
		 ;	were specified and shorten the formal-parameters list appropriately.
		 (formal-parameters
			(let 
				((fps (brz-primitive-part:parameters component))
				 (is-significant-formal-param? (lambda (param) (not (memv 'not-used (cddr param)))))
				)
				(if (/= (length (cdr fps)) (length parameters)) ;; get rid of 'params header
					(cons 'parameters (filter is-significant-formal-param? (cdr fps)))
					fps
				)
		 	)
		 )
		 (ports (brz-primitive-part:ports component))
		 (specified-ports (brz-specify-part-ports ports parameters formal-parameters))
		 (implementations (brz-primitive-part:implementation component))
		; Find the appropriate technology (allow 'technology and 'style)
		 (tech-style (find-headed-list-elem (cdr implementations) 'style breeze-style))
		 (tech (if tech-style tech-style
			 (find-headed-list-elem (cdr implementations) 'technology breeze-style)))
		 (nodes (brz-implementation-style:nodes tech))
		; Add a definition for the current component name
		 (pre-defines-1 (cons 
			(list 'component-name (brz-primitive-part:name component))
			breeze-primitives-definitions
		 ))
		 (defines (let ((defines (brz-implementation-style:defines tech)))
			(if defines
				(foldl (lambda (existing-defs def)
					(cons (list (car def)
						(brz-specify-expression (cadr def) existing-defs parameters formal-parameters))
						existing-defs
					)
				) pre-defines-1 (cdr defines))
				pre-defines-1
			)
		 ))
		; specify-expr: specify an expression using the current parameters
		 (specify-expr (lambda (expr) (brz-specify-expression expr defines parameters formal-parameters)))
		 (specified-nodes (reverse! (foldl (lambda (acc node)
		 	(let*
				((node-name (specify-expr (car node)))
				 (card (specify-expr (cadr node)))
				 (low-index (specify-expr (caddr node)))
				 (count (specify-expr (cadddr node)))
				 (width-list 
					(if (> (length node) 4)
						(list (map specify-expr (list-ref node 4)))
						'()
					)
				 )
				 (new-node (list* node-name card low-index count width-list))
				)
				(if (not (zero? card))
					(cons
						(list* (car node) card low-index count width-list)
						acc
					)
					acc
				)
			)
		 ) '() (cdr nodes))))
		 (abs-gates (brz-implementation-style:gates tech))
		 (no-abs-gates (or (not abs-gates) (null? (cdr abs-gates))))
		 (abs-connections (brz-implementation-style:connections tech))
		 (no-abs-connections (or (not abs-connections) (null? (cdr abs-connections))))
		 (port-bindings (gen-brz-ports->gen-bundle-bindings specified-ports type-context))
		 (bindings (append port-bindings specified-nodes))
		; pre-defines: predefined definitions.  HC entry
		; defns (eg. lambdas with prebound contexts)
		; FIXME, combine these with defines-1 some time
		 (pre-defines-2
			(list
				(list 'normalise-slices
					(brz-make-lambda-from-scheme-lambda (lambda slices (apply append (gen-expand-slices
						(flatten-unheaded-list slices) bindings defines parameters formal-parameters)))
						breeze-primitives-definitions)
				)
			)				
		 )
		 (all-defines (append pre-defines-2 defines))
		; new-bindings/expanded-abs-gates: expanded gates and internal nodes generated therein
		 (new-bindings/expanded-abs-gates (cdr
			(foldl (lambda (ret abs-gate)
				(let ; call self on each gate and carry over the internal-name-index
					((i/b/g (gen-expand-abs-gate abs-gate all-defines parameters formal-parameters bindings
						"int" (car ret))))
					(if i/b/g
						(list* (car i/b/g) (append (cadr i/b/g) (cadr ret)) (append (cddr i/b/g) (cddr ret)))
						ret
					)
				)
			) '(0 () . ()) (cdr abs-gates))
		 ))
		 (new-bindings (car new-bindings/expanded-abs-gates))
		 (expanded-abs-gates (cdr new-bindings/expanded-abs-gates))
		; connection-gates: gates and new nodes generated for the (connections ...) desc.
		 (connection-gates (if no-abs-connections '()
			(foldl (lambda (ret abs-gate)
				(let ((i/b/g (gen-expand-abs-gate abs-gate defines parameters formal-parameters
					bindings "int" abs-gate)))
					(if i/b/g
						(let*
							((expanded-abs-gates (cddr i/b/g))
							 (simple-gates (apply append (map
								(lambda (abs-gate) (cdr (gen-abs-gate->simple-gates abs-gate "" 0 0)))
								expanded-abs-gates
							 )))
							 (default-gates (map (compose gen-abs-gate->net-gate gen-simple-gate->default-gate)
								simple-gates))
							)
							(append! ret default-gates)
						)
						ret
					)
				)
			) '() (cdr abs-connections))
		 ))
		 ; FIXME, connection-bindings should be null
		; expanded-nodes: node defns. within the gates cell
		 (expanded-nodes (gen-bundle-bindings->net-nets (append specified-nodes new-bindings)))
		 ; Generate default target technology gates for the circuit.  Use the name `internal' for internal nodes.
		 (last-internal-index/default-gates (foldl-ma (lambda (gate internal-base-index prev-gates)
			(let*
				((next-internal/gates (gen-abs-gate->simple-gates gate "internal" 0 internal-base-index))
			 	 (gates (map gen-simple-gate->default-gate (cdr next-internal/gates)))
				)
				(list (car next-internal/gates) (append prev-gates (map gen-abs-gate->net-gate gates)))
			)
		 ) expanded-abs-gates 0 '()))
		 ; net-{ports,nets}: ports and nets in final Brz netlist
		 (net-ports (gen-brz-ports->net-ports specified-ports type-context #f))
		 (net-nets (if (zero? (car last-internal-index/default-gates))
			expanded-nodes
			(cons (list (tech-bundle-name 'node "internal" 0) (car last-internal-index/default-gates))
				expanded-nodes
			)
		 ))
		 (mangled-name (tech-map-cell-name (tech-mangle-hc-name name parameters)))
		 (mangled-core-name (tech-map-cell-name (tech-mangle-hc-core-name name parameters)))
		 (make-circuit-decl (lambda (name ports nets instances)
			(list 'circuit name (cons 'ports ports) (cons 'nets nets) (cons 'instances instances))
		 ))
		)
		(cond
			((eq? composition 'flat) ; onlt a Brz containing everything
				(list (make-circuit-decl mangled-name net-ports net-nets
					(append
						(cadr last-internal-index/default-gates)
						connection-gates
					)
				))
			)
			(no-abs-connections ; only gates, all ports used?
				(list (make-circuit-decl mangled-name net-ports net-nets
					(cadr last-internal-index/default-gates)
				))
			)
			(no-abs-gates ; only connections, all ports used?
				(list (make-circuit-decl mangled-name net-ports net-nets connection-gates))
			)
			(else
				(let*
					((fully-ported-core (make-circuit-decl mangled-core-name net-ports net-nets
						(cadr last-internal-index/default-gates)
					 ))
					 (core-connections (net-make-connection-table fully-ported-core))
					 (trimmed-ports (foldl (lambda (trimmed port)
						(let ((port-connections (net-find-sorted-connection core-connections (net-port:name port))))
							(if (and port-connections (net-connection-any-connections? port-connections))
								(cons port trimmed)
								trimmed
							)
						)
					 ) '() net-ports))
					)
					(list
						(make-circuit-decl mangled-core-name
							trimmed-ports
							net-nets
							(cadr last-internal-index/default-gates)
						)
						(make-circuit-decl
							mangled-name
							net-ports
							net-nets
							(if no-abs-gates
								connection-gates
								(cons
									(list 'instance mangled-core-name (map gen-net-port->net-connection trimmed-ports))
									connection-gates
								)
							)
						)
					)
				)
			)
		)
	)
))

;;; gen-make-hc-header: make a .net netlist for the named handshake component.
;;;		This netlist should have only name and ports fields set and be the minimum necessary
;;;		netlist to define the external interface of the component.
(define gen-make-hc-header (lambda (name mangled-name parameters type-context)
	(let*
		((component (brz-find-primitive name)) 
		 (formal-parameters (brz-primitive-part:parameters component))
		 (ports (brz-primitive-part:ports component))
		 (specified-ports (brz-specify-part-ports ports parameters formal-parameters))
		)
		(list 'circuit
			mangled-name
			(cons 'ports (gen-brz-ports->net-ports specified-ports type-context #f))
			(list 'nets) (list 'instances)
		)
	)
))

;;; gen-make-hc-other-files: make miscellaneous/CAD system
;;;		dependent files.  `filename-stub' will be a directory/filename w/o
;;;		an extension
(define gen-make-hc-other-files (lambda
	(name mangled-name parameters type-context filename-stub)
	(let*
		((component (brz-find-primitive name)) 
		 (formal-parameters (brz-primitive-part:parameters component))
		 (ports (brz-primitive-part:ports component))
		 (specified-ports (brz-specify-part-ports ports parameters formal-parameters))
		)
		(if tech-write-symbol-file
			(tech-write-symbol-file name mangled-name specified-ports filename-stub)
		)
	)
))

;;; gen-brz-ports->name/index-list: make a vector of (portname . portVectorIndex) from a list of
;;;		brz ports.  Single ports (port/sync-port) become single vector elements and arrayed ports
;;;		become elements from index 0 upto port-count-1. If buffered is true appends "_buf" to each
;;; 	name to allow top-level ports to be buffered.
(define gen-brz-ports->name/index-list (lambda (ports buffered)
	(letrec
		((get-name (lambda (port) (if buffered (string-append (brz-port:name port) "_buf") (brz-port:name port))))
		 (make-arrayed-ports (lambda (name low-index port-count tail-of-list)
			(let tail
				((index low-index)
				 (ports tail-of-list)
				)
				(if (= index (+ low-index port-count))
					ports
					(tail (+ 1 index) (cons (cons name index) ports))
				)
			)
		)))
		(list->vector (reverse! (foldl (lambda (names/indices port)
			(case (car port)
				((sync-port port) (cons (cons (get-name port) 0) names/indices))
				((arrayed-port)
					(make-arrayed-ports (get-name port)
						(brz-arrayed-port:low-index port) (brz-arrayed-port:port-count port) names/indices)
				)
				((arrayed-sync-port)
					(make-arrayed-ports (get-name port)
						(brz-arrayed-sync-port:low-index port) (brz-arrayed-sync-port:port-count port) names/indices)
				)
			)
		) '() (if (headed-list? 'ports ports) (cdr ports) ports))))
	)
))

;;; gen-buffer-ports - produce a list of net buffer instances to buffer each port. Port-nets is a list
;;; of .net port descriptions and channel-nets is a list of .net channel descriptions for the internal
;;; buffered channels
(define gen-buffer-ports (lambda (port-nets channel-nets)
 (let*
   ((gate-desc (cadr (assoc "buf" breeze-gate-mappings)))
    (buffer (car gate-desc))
    (mapping (cdr gate-desc))
		(mapping-fn (lambda (ports)
			(map (lambda (index)
				(if (eqv? 'unconnected index) 'unconnected (list-ref ports index))
			) mapping)	
		))
  )
	(foldl (lambda (instances port channel)
  	(let
    	((port-name (net-port:name port))
        (channel-name (net-net:name channel))
        (direction (net-port:direction port))
        (width (net-port:cardinality port))
      )
    	(append instances
       	(map.. (lambda (index)
				 	(list 'instance buffer
          	(mapping-fn 
					 		(case direction ;; buffer - dst, src
            		((input) 
             			(list (list channel-name index) (list port-name index))
								)
								((output) 
             			(list (list port-name index) (list channel-name index))
								)
								(else ;;; uh, don't know what to do with inouts -lets treat them as inputs...
									(list (list channel-name index) (list port-name index))
								)
            	)
           	)
					)
        ) 0 (- width 1))
       )
     )
   ) '() port-nets channel-nets)
 )
))

;;; gen-make-breeze-part-netlist: make a .net netlist for a given breeze part which expresses the
;;;		same handshake component connectivity
(define gen-make-breeze-part-netlist (lambda (procedure type-context buffered)
	(let*
		((channels (brz-breeze-part:channels procedure))
		 (ports (cdr (brz-breeze-part:ports procedure)))
		 (port-/channel-nets (gen-brz-ports->net-ports ports type-context buffered))
		 (port-nets (if buffered (car port-/channel-nets) port-/channel-nets))
		 (buffered-channel-nets (if buffered (cdr port-/channel-nets) '()))
		 ; vector of (port-name . index) pairs for each real port eg.
		 ; sync a; array 2 of sync b => #((a . 0) (b . 0) (b . 1))
		 (unvectored-ports (gen-brz-ports->name/index-list ports buffered))
		 (buffered-instances (if buffered (gen-buffer-ports port-nets buffered-channel-nets) '()))
		 (port-count (vector-length unvectored-ports))
		 ; channel-name: a wrapper to allow us to keep port names
		 (channel-name (lambda (portion channel-no)
			(if (<= channel-no port-count)
				(let ((name/index (vector-ref unvectored-ports (- channel-no 1))))
					(tech-bundle-name portion (car name/index) (cdr name/index))
				)
				(tech-channel-name portion channel-no)
			)
		 ))
		 (portion-for-channel-sense (lambda (sense)
			(case sense
				((push) tech-push-channel-portions)
				((pull) tech-pull-channel-portions)
				((sync) tech-sync-channel-portions)
			)
		 ))
		 ; channel-nets: local channels expanded into (nets ...) elements
		 (channel-nets (car (foldl-ma (lambda (channel nets index)
			(list
				(let*
					((portions (portion-for-channel-sense (car channel)))
					 (channel-has-width (and (> (length channel) 1) (number? (cadr channel))))
					 ; turn portion lists into channel wires
					 (processed-portions
						(reverse! (foldl (lambda (acc portion)
							(let
								((width (if channel-has-width
									((caddr portion) (cadr channel))
									((caddr portion) 1)
								)))
								(if width 
									(cons (list (channel-name (car portion) index) (if (zero? width) 1 width)) acc)
									acc
								)
							)
						) '() portions))
					 )
					)
					(append! processed-portions nets)
				)
				(+ 1 index)
			)
		 ) (list-tail (cdr channels) port-count) '() (+ 1 port-count))))
		 ; channel-cardinalities: a vector of all the channel widths
		 (channel-cardinalities (list->vector (map (lambda (channel)
			(case (car channel) ((push pull) (cadr channel)) ((sync) 0))) (cdr channels))))
		 ; channel-sense : a vector of all the channel senses
		 (channel-senses (list->vector (map car (cdr channels))))
		 ; channel-portions: connections for channel number channel-no
		 (channel-portions (lambda (channel-no)
			(let
				((cardinality (vector-ref channel-cardinalities (- channel-no 1)))
				 (portions (portion-for-channel-sense (vector-ref channel-senses (- channel-no 1))))
				)
				(reverse! (foldl (lambda (acc portion)
	 				(let
	   					((width ((caddr portion) cardinality)))
	   					(cond
							((not width) acc)
							((zero? width) (cons (channel-name (car portion) channel-no) acc))
							(else (cons (list (channel-name (car portion) channel-no) 0 width) acc))
						)
					)
				) '() portions))
			)
		 ))
		 (instances (foldl (lambda (hc-list hc)
			(let ((name (brz-get-primitive-part-name (cadr hc))))
				(if name
					(let* ; primitive parts
						((hc-defn (brz-find-primitive name))
						 (formal-parameters (brz-primitive-part:parameters hc-defn))
						 (actual-parameters (brz-normalise-parameter-list (caddr hc)))
						 (significant-parameters (gen-extract-significant-parameters
							(cdr formal-parameters) actual-parameters))
						 (mangled-name (tech-map-cell-name (tech-mangle-hc-name name significant-parameters)))
						 (flat-ports (flatten-unheaded-list (cadddr hc)))
						)
						(cons (list 'instance
							mangled-name
							(apply append! (map channel-portions flat-ports))
						) hc-list)
					)
					(case (car hc)
						((component)
							(let* ; Non primitive part, (component name () ports)
								((mangled-name (tech-map-cell-name (tech-mangle-breeze-part-name (cadr hc))))
								 (flat-ports (flatten-unheaded-list (cadddr hc)))
								)
								(cons (list 'instance
									mangled-name
									(apply append! (map channel-portions flat-ports))
								) hc-list)
							)
						)
						((undeclared-component)
							(let* ; Non primitive part, (undeclared-component some-name params actual-ports
								; (ports formal-ports) ...)
								((mangled-name (tech-map-cell-name (tech-mangle-builtin-function-name (cadr hc) (caddr hc))))
								 (flat-ports (flatten-unheaded-list (cadddr hc)))
								)
								(cons (list 'instance
									mangled-name
									(apply append! (map channel-portions flat-ports))
								) hc-list)
							)
						)
					)
				)
		 	)
		 ) '() (cdr (brz-breeze-part:components procedure))))
		)
		(list 'circuit
			(tech-map-cell-name (tech-mangle-breeze-part-name (brz-breeze-part:name procedure)))
			(cons 'ports port-nets)
			(cons 'nets (append buffered-channel-nets channel-nets))
			(cons 'instances (append buffered-instances instances))
		)
	)
))
