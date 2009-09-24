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
;;;	`net-drive.scm'
;;;	Drive/load analysis procedures
;;;
;;;	$Id: net-drive.scm,v 1.12 2003/02/08 19:39:43 bardslea Exp $
;;;

(balsa-scheme-import 'net 'connections)

(define new-drive-hash #f)

(define reset-drive-hash (lambda ()
	(set! new-drive-hash (make-hash-table 57))
	(for.. (lambda (index)
		(let ((elem (vector-ref breeze-gate-drives index)))
			(hash-set! new-drive-hash (car elem) (cadr elem))
		)
	) 0 (- (vector-length breeze-gate-drives) 1))
))

(reset-drive-hash)

;;; net-no-d/l: the null drive value
(define net-no-d/l '(#f . 0))

;;; net-circuit-is-driving-gate?: is the circuit `name' a gate in the foundary provided cell library
;;;		(does it have a drive table entry, returns #f or that entry)
(define net-circuit-is-driving-gate? (lambda (name) (net-find-gate-drive name)))

;;; net-tables: tables for use with netlist manipulation.  These include:
;;;		circuit-decl: circuit declaration (circuit "name" ...)
;;;		port-mapping: vector mapping port numbers into signal table indices (as used in sc/lc tables)
;;;		instance-vector: vectorised copy of (cdr (net-circuit-decl:instances circuit-decl)), for convenience
;;;		sc-table: signal connection table (see net-make-connection-table) mapping signal/bit indices
;;;			into lists of (instance-index connection-index wire-index) triples describing connections to insts.
;;;		ic-table: same content as sc-table but indexed the opposite way around (ie. a 3-dimensional vector
;;;			of (signal-index bit-index) pairs)
;;;		lc-table: same indexing as sc-table but elements identify drive/load and local signal-signal
;;;			connectivity of circuit
;;;		id-table: drive table (2D vector of signal-index/bit-index) of signal drives gathered from instances
(define net-tables:circuit-decl car)
(define net-tables:port-mapping cadr)
(define net-tables:instance-vector caddr)
(define net-tables:sc-table (nth 3))
(define net-tables:ic-table (nth 4))
(define net-tables:lc-table (nth 5))

;;; net-find/make-table-entry: find a table entry at index `index' and make
;;;		a new entry (a vector of #f's) of size `cardinality' if no entry exists
(define net-find/make-table-entry (lambda (table index cardinality)
	(let
		((existing-entry (vector-ref table index)))
		(if existing-entry
			existing-entry
			(let
				((new-entry (make-vector cardinality #f)))
				(vector-set! table index new-entry)
				new-entry
			)
		)
	)
))

;;; net-drive<=?: sorting predicate for drive tables
(define net-drive<=? net-net/port:name<=?)

;;; net-sum-drive/load-pairs: find the minimum drive and sum of loads of two drive/load pairs.
;;;		Unconnected drive is represented as #f and should result in #f if boths inputs are unconnected
(define net-sum-drive/load-pairs (lambda (left right)
	(cons
		(if (car left)
			(if (car right) (min (car left) (car right)) (car left))
			(car right)
		)
		(+ (cdr right) (cdr left))
	)
))

;;; net-find-gate-drive: find the drive table entry for a library cell, returns #f
;;;		if no entry is found
(define net-find-gate-drive (lambda (name)
	(let ((found (hash-ref new-drive-hash name)))
		found
		; (if found found (begin (print-err "can't find drive for `" name "'" #\newline) #f))
	)
))

;;; net-make-port->signal-mapping-table: make a vector indexed by port number each
;;;		element of which is a signal index for that port in the sc-table
(define net-make-port->signal-mapping-table (lambda (port-count sc-table)
	(let
		((ret (make-vector port-count 0)))
		(for.. (lambda (signal-index)
			(let ((signal (vector-ref sc-table signal-index)))
				(if (net-connection-is-to-port? signal)
					(vector-set! ret (net-connection:port/net-index signal) signal-index)
				)
			)
		) 0 (- (vector-length sc-table) 1))
		ret
	)
))

;;; net-make-instance-connection-table: make a table which reverse maps an sc-table.
;;;		ie. it is a vector of instance port->signal connections rather than signal->instance connections.
(define net-make-instance-connection-table (lambda (netlist-tables instance-vector sc-table)
	(let ((ret (make-vector (vector-length instance-vector) #f)))
		; First build the table, pretty boring
		(for.. (lambda (inst-index) (let*
			((instance (vector-ref instance-vector inst-index))
			 (inst-tables (hash-ref netlist-tables (net-instance:name instance)))
			 (inst-ports (cdr (net-circuit-decl:ports (net-tables:circuit-decl inst-tables))))
			 (inst-port-count (length inst-ports))
			 (inst-entry (make-vector inst-port-count #f))
			)
			(vector-set! ret inst-index inst-entry)
			(foldl (lambda (index port) (let*
				((port-cardinality (net-port:cardinality port))
				 (port-entry (make-vector port-cardinality #f))
				)
				(vector-set! inst-entry index port-entry)
				(+ 1 index)
			)) 0 inst-ports)
		)) 0 (- (vector-length instance-vector) 1))
		; Then populate it
		(for.. (lambda (signal-index) (let
			((signal-entry (net-connection:connections (vector-ref sc-table signal-index))))
			(for.. (lambda (bit-index) (let
				((bit-entry (vector-ref signal-entry bit-index)))
				(for-each (lambda (connection)
					(vector-set!
						(vector-ref (vector-ref ret (net-conn-conn:instance-index connection))
							(net-conn-conn:connection-index connection))
						(net-conn-conn:wire-index connection)
						(list signal-index bit-index)
					)
				) bit-entry)
			)) 0 (- (vector-length signal-entry) 1))
		)) 0 (- (vector-length sc-table) 1))
		ret
	)
))

;;; net-make-feedthrough-lc-table: make an lc-table for a component with a ``feedthrough''
;;;		attribute.  This will consist solely of port aliases.
(define net-make-feedthrough-lc-table (lambda (sc-table port->signal-mapping feedthroughs)
	(let ((ret (make-vector (vector-length sc-table) #f)))
		(for-each (lambda (feedthrough)
			(let*
				((from-port-index (car feedthrough))
				 (from-signal-index (vector-ref port->signal-mapping from-port-index))
				 (from-connection-entry (vector-ref sc-table from-signal-index))
				 (cardinality (net-connection:cardinality from-connection-entry))
				 (from-entry (make-vector cardinality #f))
				 (to-signal-indices (map (lambda (i) (vector-ref port->signal-mapping i)) (cdr feedthrough)))
				)
				(vector-set! ret from-signal-index from-entry)
				; make the target entry
				(for-each (lambda (to-signal-index)
					(vector-set! ret to-signal-index (make-vector cardinality #f))
				) to-signal-indices)
				; iterate over the bits and fill in both input and alias entries
				(for.. (lambda (bit-index)
					(let
						((to-ports/signals (foldl-ma (lambda (to-signal-index to-ports to-signals)
							(vector-set! (vector-ref ret to-signal-index) bit-index
								; LC-ENTRY
								(list 'alias from-signal-index bit-index)
							)
							(if (net-connection-is-to-port? (vector-ref sc-table to-signal-index))
								(list (cons (list to-signal-index bit-index) to-ports) to-signals)
								(list to-ports (cons (list to-signal-index bit-index) to-signals))
							)
						 ) to-signal-indices '() '()))
						)
						(vector-set! from-entry bit-index
							; LC-ENTRY
							(list 'source net-no-d/l
								(reverse! (car to-ports/signals))
								(reverse! (cadr to-ports/signals))
								#f
							)
						)
					)
				) 0 (- cardinality 1))
			)
		) feedthroughs)
		ret
	)
))

;;; net-make-leaf-circuit-lc-table: make an lc table for a component with drive description from
;;;		`drive-table'.  This cell must have no feedthroughs.
(define net-make-leaf-circuit-lc-table (lambda (sc-table port->signal-mapping drive-table)
	(let ((ret (make-vector (vector-length sc-table) #f)))
		(for.. (lambda (port-index) (let*
			((port-entry (vector-ref drive-table port-index))
			 (cardinality (vector-length port-entry))
			 (ret-port-entry (make-vector cardinality #f))
			)
			(vector-set! ret (vector-ref port->signal-mapping port-index) ret-port-entry)
			(for.. (lambda (bit-index) (let
				((d/l (vector-ref port-entry bit-index)))
				(vector-set! ret-port-entry bit-index
					(if (car d/l) ; has drive, must be a source
						; LC-ENTRY
						(list 'source d/l '() '() #f)
						; LC-ENTRY
						(list 'load d/l)
					)
				)
			)) 0 (- cardinality 1))
		)) 0 (- (vector-length drive-table) 1))
		ret
	)
))

;;; net-make-full-lc-table: return an lc-table for the given circuit.  This consists of a vector
;;;		of elements of the same length as the sc-table with like-indexed elements relating to
;;;		the net/port in that position in the connection table.  Each vector element is:
;;;			#f -- this signal isn't interesting for some unspecified reason
;;;			#(wire-purpose ...) -- vector of wire purposes
;;;				The vector here is the same length as the signal cardinality.  Each vector element is:
;;;					#f -- uninteresting signal (for some reason)
;;;					('source d/l to-ports to-signals sourced-by-gate) -- origin of a signal which is aliased by
;;;						ports `to-ports' and internal signals `to-signals' (both have the form:
;;;						list of (signal-index bit-index))
;;;						note that both to-... lists can be empty identifying this as a unique signal
;;;						sourced-by-gate is an icw triple if the signal is sourced by a gate at this level of hierarchy
;;;					('alias from-signal-index from-bit-index)
;;;					('load d/l) -- signal connected only to a loading gate input 
(define net-make-full-lc-table (lambda (netlist-tables sc-table ic-table instance-vector)
	(let
		((ret (make-vector (vector-length sc-table) #f)))
		(letrec
			; source-signal: find the sourcing signal (at this level) for the net-connection `connection'
			;	of signal `signal/bit-index' in the sc-table.  Returns (source-si/bi #f)
			((source-signal (lambda (connection signal/bit-index)
				(let next-conn-conn
					((connection connection)
					 (connection-index 0)
					)
					(if (null? connection)
						#f
						(let*
							((conn-conn (car connection))
							 (instance (vector-ref instance-vector (net-conn-conn:instance-index conn-conn)))
							 (inst-name (net-instance:name instance))
							 (inst-tables (hash-ref netlist-tables inst-name))
							 (inst-lc-table (net-tables:lc-table inst-tables))
							 (inst-sc-table (net-tables:sc-table inst-tables))
							 (inst-port-mapping (net-tables:port-mapping inst-tables))
							 (inst-signal-for-conn-conn (vector-ref inst-port-mapping
							 	(net-conn-conn:connection-index conn-conn)))
							 (inst-lc-signal-entry (vector-ref inst-lc-table inst-signal-for-conn-conn))
							 (inst-lc-bit-entry (vector-ref inst-lc-signal-entry (net-conn-conn:wire-index conn-conn)))
							)
							(if inst-lc-bit-entry
								(case (car inst-lc-bit-entry)
									; chase aliases and ports marked with no-drive source
									((alias)
										(let*
											((src-sc-entry (vector-ref inst-sc-table (cadr inst-lc-bit-entry))))
											(if (net-connection-is-to-port? src-sc-entry)
												; is a port
												(let*
													((port-no (net-connection:port/net-index src-sc-entry))
													 (port-ic-entry (vector-ref (vector-ref ic-table
														(net-conn-conn:instance-index conn-conn)) port-no))
													 ; signal and port wire indices match for an output<-port alias
													 (port-bit-ic-entry (vector-ref port-ic-entry
														(caddr inst-lc-bit-entry)))
													 (signal-entry (vector-ref sc-table (car port-bit-ic-entry)))
													 (signal-bit-entry
														(vector-ref (net-connection:connections signal-entry)
														(cadr port-bit-ic-entry)))
													)
													; this is an input at the top level
													(if (eq? 'input (net-connection:nature signal-entry))
														(list port-bit-ic-entry #f)
														(source-signal signal-bit-entry port-bit-ic-entry)
													)
												)
												; isn't a port, must be an internal drive
												(error "not yet")
											)
										)
									)
									((source proxy-source) ; must have drive
										(if (car (cadr inst-lc-bit-entry)) ; has drive?
											(list signal/bit-index connection-index)
											; no drive, visit next signal
											(next-conn-conn (cdr connection) (+ 1 connection-index))
										)
									)
									(else (next-conn-conn (cdr connection) (+ 1 connection-index)))
								)
								(next-conn-conn (cdr connection) (+ 1 connection-index))
							)
						)
					)
				)
			 ))
			 ; get-icw-lc-bit: get the lc bit entry for an (instance-index conn-index wire-index) triple	
			 (get-icw-lc-bit (lambda (icw)
				(let*
					((instance (vector-ref instance-vector (net-conn-conn:instance-index icw)))
					 (inst-name (net-instance:name instance))
					 (inst-tables (hash-ref netlist-tables inst-name))
					 (inst-lc-table (net-tables:lc-table inst-tables))
					 (inst-port-mapping (net-tables:port-mapping inst-tables))
					 (inst-signal-for-conn-conn (vector-ref inst-port-mapping (net-conn-conn:connection-index icw)))
					 (inst-lc-signal-entry (vector-ref inst-lc-table inst-signal-for-conn-conn))
					)
					(vector-ref inst-lc-signal-entry (net-conn-conn:wire-index icw))
				)
			 ))
			 ; instance-signal-to-signal: convert a signal/bit pair in an instance's signal space into
			 ;		a signal/bit pair at this level (or return #f if it's not possible)
			 (instance-signal-to-signal (lambda (instance-index instance-signal/bit)
				(let*
					((instance (vector-ref instance-vector instance-index))
					 (inst-name (net-instance:name instance))
					 (inst-tables (hash-ref netlist-tables inst-name))
					 (inst-sc-table (net-tables:sc-table inst-tables))
					 (inst-signal-entry (vector-ref inst-sc-table (car instance-signal/bit)))
					)
					(if (net-connection-is-to-port? inst-signal-entry)
						(let*
							((port-index (net-connection:port/net-index inst-signal-entry)))
							(vector-ref (vector-ref (vector-ref ic-table instance-index) port-index)
								(cadr instance-signal/bit))
						)
						#f
					)
				)	
			 ))
			 ; accumulate-signal-d/l-and-aliases: using signal as a starting point, visit all the
			 ;	connections and aliases of this signal and accumulate their drive/loads (and a list of those aliases)
			 ;	marks the entries in ret for signal/bit pairs with an 'alias entry corresponding
			 ;	to `real-source-signal'
			 (accumulate-signal-d/l-and-aliases (lambda (signal real-source-signal)
			 	(let*
			 		((signal-sc-entry (vector-ref sc-table (car signal)))
			 		 (signal-sc-bit (vector-ref (net-connection:connections signal-sc-entry) (cadr signal)))
			 		)
					; visit the aliases, for each visit the connected component ports and accrue d/l
					(let
						((v (foldl-ma (lambda (conn-conn d/l visited-signals)
							(let
								((lc-bit (get-icw-lc-bit conn-conn))
								 (visited visited-signals)
								)
								(case (car lc-bit)
									((source proxy-source)
										; visit all the externally visible sources of this signal
										(foldl-ma (lambda (instance-signal d/l visited-signals)
											(let*
												((signal (instance-signal-to-signal
													(net-conn-conn:instance-index conn-conn) instance-signal))
												 (signal-ret-entry (net-find/make-table-entry ret (car signal)
												 	(net-connection:cardinality (vector-ref sc-table (car signal)))))
												 (alias-info (accumulate-signal-d/l-and-aliases
													signal real-source-signal))
												)
												(vector-set! signal-ret-entry (cadr signal)
													; LC-ENTRY
													(list 'alias
														(car real-source-signal)
														(cadr real-source-signal)
													)
												)
												(list
													(net-sum-drive/load-pairs d/l (car alias-info))
													(cons signal (append (cadr alias-info) visited-signals))
												)
											)
										) (caddr lc-bit)
											(net-sum-drive/load-pairs d/l (cadr lc-bit)) visited)
									)
									((alias)
										(list d/l visited)
									)
									((load)
										(list (net-sum-drive/load-pairs d/l (cadr lc-bit)) visited)
									)
									(else (error "Ouch!"))
								)
							)
						) signal-sc-bit net-no-d/l '())))
						v
					)
				)
			 ))
			)
			(for.. (lambda (signal-index) (let*
				((signal (vector-ref sc-table signal-index))
				 (cardinality (net-connection:cardinality signal))
				 (signal-ret-entry (net-find/make-table-entry ret signal-index cardinality))
				)
				(for.. (lambda (bit-index) (let
					((bit (vector-ref (net-connection:connections signal) bit-index))
					 (bit-ret-entry (vector-ref signal-ret-entry bit-index))
					)
					(if bit-ret-entry ; skip (already visited)
						#f
						(let
							((signal-source
								; first find the origin of this signal
								(if (eq? 'input (net-connection:nature signal))
									(list (list signal-index bit-index) #f)
									(source-signal bit (list signal-index bit-index))
								)
							))
							(if signal-source
								(let*
									((source-si/bi (car signal-source))
									 (d/l-aliases (accumulate-signal-d/l-and-aliases
										source-si/bi source-si/bi))
									 (d/l (car d/l-aliases))
									 (aliased-signals (cadr d/l-aliases))
									 ; source-connection-index: index in `bit' of connection to sourcing circuit (if any)
									 (source-connection-index (cadr signal-source))
									 (source-icw
										(if (and source-connection-index
											(= (car source-si/bi) signal-index)
											(= (cadr source-si/bi) bit-index))
											(let ((icw (list-ref bit source-connection-index)))
												(if (net-circuit-is-driving-gate?
													(net-instance:name (vector-ref instance-vector (car icw))))
													icw
													#f
												)
											)
											#f
										)
									 )
									 (port/internal-aliases (filter2 
										(lambda (signal) (net-connection-is-to-port?
											(vector-ref sc-table (car signal))))
										aliased-signals
									 ))
									)
									; if the source isn't a port but it's aliased to one ...
									; nominate the port as a proxy source, mark all the
									; other ports and internal aliases to point to this
									(if (and (not (net-connection-is-to-port? (vector-ref sc-table signal-index)))
										(not (null? (car port/internal-aliases))))
										(let*
											((proxy-source (car (car port/internal-aliases)))
											 (port-aliases (cdr (car port/internal-aliases)))
											 (internal-aliases (cadr port/internal-aliases))
											 (mark-aliases (lambda (aliases)
												(for-each (lambda (alias)
													(vector-set! (vector-ref ret (car alias))
														(cadr alias)
														(list 'alias (car proxy-source) (cadr proxy-source))
													)
												) aliases)
											 ))
											)
											(mark-aliases port-aliases)
											(mark-aliases internal-aliases)
											(vector-set! (vector-ref ret (car proxy-source))
												(cadr proxy-source)
												(list 'proxy-source d/l port-aliases internal-aliases
													(list signal-index bit-index))
											)
											(vector-set! signal-ret-entry bit-index
												(list 'proxied-source d/l proxy-source source-icw)
											)
										)
										(vector-set! signal-ret-entry bit-index
											(if (and (null? aliased-signals) (not (car d/l)))
												; no drive, no aliases, it's a 'load
												(list 'load d/l)
												; must be a source of some flavour
												(list 'source d/l (car port/internal-aliases) (cadr port/internal-aliases)
													source-icw)
											)
										)
									)
								)
							)
						)
					)
				)) 0 (- cardinality 1))
			)) 0 (- (vector-length sc-table) 1))
			ret	
		)
	)
))

;;; net-make-circuit-lc-table: make an lc-table for the given circuit and associated info.
(define net-make-circuit-lc-table
	(lambda (netlist-tables circuit sc-table ic-table instance-vector port->signal-mapping)
	(let*
		((circuit-name (net-circuit-decl:name circuit))
		 (drive-table-entry (net-find-gate-drive circuit-name))
		 (feedthrough-attribute (net-circuit-decl-find-attribute circuit 'feedthrough))
		)
		(cond
			(feedthrough-attribute (net-make-feedthrough-lc-table sc-table port->signal-mapping
				(cdr feedthrough-attribute)))
			(drive-table-entry (net-make-leaf-circuit-lc-table sc-table port->signal-mapping drive-table-entry))
			(else (net-make-full-lc-table netlist-tables sc-table ic-table instance-vector))
		)
	)
))

;;; net-make-instance-drive-table-from-lc-table: make an instance drive table from a circuit's
;;;		lc-table (which should contain some interesting initial values)
(define net-make-instance-drive-table-from-lc-table (lambda (sc-table lc-table)
	(let ((ret (make-vector (vector-length sc-table) #f)))
		(for.. (lambda (signal-index)
			(let*
				((cardinality (net-connection:cardinality (vector-ref sc-table signal-index)))
				 (lc-entry (vector-ref lc-table signal-index))
				 (new-signal-entry (make-vector cardinality net-no-d/l))
				)
				(vector-set! ret signal-index new-signal-entry)
				(if lc-entry
					(for.. (lambda (bit-index)
						(let ((lc-bit-entry (vector-ref lc-entry bit-index)))
							(vector-set! new-signal-entry bit-index
								(if lc-bit-entry
									(case (car lc-bit-entry)
										((alias)
											(cadr (vector-ref (vector-ref lc-table (cadr lc-bit-entry))
												(caddr lc-bit-entry)))
										)
										((load source proxy-source proxied-source)
											(cadr lc-bit-entry)
										)
									)
									net-no-d/l
								)
							)
						)
					) 0 (- cardinality 1))
				)
			)
		) 0 (- (vector-length sc-table) 1))
		ret
	)
))

;;; maxf: max with #f handling
(define maxf (lambda (a b)
	(cond
		(a (if b (max a b) a))
		(b (if a (max a b) b))
		(else #f)
	)
))

;;; net-populate-id-tables: call yy after adjusting making an id-tables entry for the top level circuit (the one
;;;		`circuit-tables' corresponds to.  Mark desired port d/ls in that circuit's `id-tables'
;;;		entry.  Position is a path (in reverse order) of the instances decended into to get to this point.
(define net-populate-id-tables (lambda (netlist-tables id-tables circuit-tables position)
	(let*
		((sc-table (net-tables:sc-table circuit-tables))
		 (circuit-decl (net-tables:circuit-decl circuit-tables))
		 (ic-table (net-tables:ic-table circuit-tables))
		 (id-table (hash-ref id-tables (net-circuit-decl:name circuit-decl)))
		 (instances (net-tables:instance-vector circuit-tables))
		)
		; Visit all the instances, make port-d/ls for each instance and recurse
		(for.. (lambda (instance-index)
			(let ((instance (vector-ref instances instance-index)))
				; if it's in the drive table then its output drives are not negotiable
				(if (not (net-circuit-is-driving-gate? (net-instance:name instance)))
					(let*
						((instance-tables (hash-ref netlist-tables (net-instance:name instance)))
						 (ic-table-entry (vector-ref ic-table instance-index))
						 (instance-port-mapping (net-tables:port-mapping instance-tables))
						 (instance-sc-table (net-tables:sc-table instance-tables))
						 (instance-lc-table (net-tables:lc-table instance-tables))
						 (instance-id-table (let ((existing-id-table (hash-ref id-tables (net-instance:name instance))))
							(if existing-id-table
								existing-id-table
								(let ((new-id-table (net-make-instance-drive-table-from-lc-table
									instance-sc-table instance-lc-table)))
									(hash-set! id-tables (net-instance:name instance) new-id-table)
									new-id-table
								)
							)
						 ))
						 (instance-name (net-instance:instance-name instance))
						 (new-position (cons (if instance-name instance-name instance-index) position))
						 (verb #f)
						)
						; FIXME
						(if (string=? (net-instance:name instance) "BrzDecisionWait_1") (begin
							(print new-position #\newline)
							(set! verb #t)
						))
						; Insert revised drives/loads into the port signals in the id-table of the instance reflecting
						;	d/ls of those signals at this level of hierarchy
						(for.. (lambda (connection-index) (let*
							((connection-entry (vector-ref ic-table-entry connection-index))
							 (instance-signal-no (vector-ref instance-port-mapping connection-index))
							 (instance-signal-entry (vector-ref instance-id-table instance-signal-no))
							)
							(for.. (lambda (bit-index) (let*
								((bit-entry (vector-ref connection-entry bit-index))
								 (circuit-bit-d/l (vector-ref (vector-ref id-table (car bit-entry)) (cadr bit-entry)))
								 (instance-bit-d/l (vector-ref instance-signal-entry bit-index))
								)
								(if verb (begin
									(print connection-index " " bit-index " " circuit-bit-d/l " " instance-bit-d/l #\newline)
								))
								(vector-set! instance-signal-entry bit-index
									; use instance drive and circuit load
									(cons (car instance-bit-d/l) (maxf (cdr circuit-bit-d/l) (cdr instance-bit-d/l)))
								)
							)) 0 (- (vector-length connection-entry) 1))
						)) 0 (- (vector-length ic-table-entry) 1))
						; Recurse
						(net-populate-id-tables netlist-tables id-tables instance-tables new-position)
					)
				)
			)
		) 0 (- (vector-length instances) 1))
	)
))

;;; net-make-id-tables-starting-from-circuit: make a hash of instance-drive/load tables
;;;		for the circuit `name' and it's instances (and sub instances...) with maximum
;;;		signal loads marked.  circuit-decl/{lc,ic,sc...}-table data is to be taken from
;;;		`netlist-tables'.
(define net-make-id-tables-starting-from-circuit (lambda (netlist-tables name)
	(let*
		((tables (hash-ref netlist-tables name))
		 (sc-table (net-tables:sc-table tables))
		 (lc-table (net-tables:lc-table tables))
		 (id-tables (make-hash-table 57))
		 (circuit-id-table (net-make-instance-drive-table-from-lc-table sc-table lc-table))
		)
		(hash-set! id-tables name circuit-id-table)
		(net-populate-id-tables netlist-tables id-tables tables '())
		id-tables
	)
))
;;; net-make-tables-for-circuit: make tables for a single circuit with reference to
;;;		circuit tables in `netlist-tables'
(define net-make-tables-for-circuit (lambda (netlist-tables circuit)
	(let*
		((instance-vector (list->vector (cdr (net-circuit-decl:instances circuit))))
		 (sc-table (net-make-connection-table circuit))
		 (ic-table (net-make-instance-connection-table netlist-tables instance-vector sc-table))
		 (port->signal-mapping (net-make-port->signal-mapping-table
			(length (cdr (net-circuit-decl:ports circuit))) sc-table))
		 (lc-table (net-make-circuit-lc-table netlist-tables circuit sc-table ic-table
		 	instance-vector port->signal-mapping))
		)
		(list
			circuit
			port->signal-mapping
			instance-vector
			sc-table
			ic-table
			lc-table
		)
	)
))

;;; net-populate-tables-from-netlist: fill a hash of circuit tables with newly made tables from
;;;		the circuits in `netlist'
(define net-populate-netlist-tables-from-netlist (lambda (tables netlist)
	(for-each (lambda (circuit)
		(hash-set! tables (net-circuit-decl:name circuit) (net-make-tables-for-circuit tables circuit))
	) netlist)
))

;;;;;;;;;;;; Stuff from drives.scm below, need to rationalise!

;;; net-lc-bit-source-icw: get the instance-index/connection-index/wire-index element
;;;		out of an lc-table bit entry
(define net-lc-bit-source-icw (lambda (lc-bit)
	(if lc-bit
		(case (car lc-bit)
			((source) (list-ref lc-bit 4))
			((proxied-source) (list-ref lc-bit 3))
			(else #f)
		)
		#f
	)
))

;;; net-summarise-circuit-drives: returns a summary of drive and load requirements for circuits in
;;;		the circuit related to `id-table' and `circuit-tables'.  Only report sourcing signals/bits
;;;		with load above `load-threshold'.  Estimate drive using `l/d-ratio' as an acceptable
;;;		ratio of drive to load.
;;;		The return list consists of elements: (circuit-name signal-index bit-index d/l source-icw required-drive)
(define net-summarise-circuit-drives (lambda (id-table circuit-tables l/d-ratio load-threshold circuit-name)
	(let
		((lc-table (net-tables:lc-table circuit-tables))
		 (sc-table (net-tables:sc-table circuit-tables))
		 (ret '())
		)
		(for.. (lambda (signal-index) (let
			((lc-entry (vector-ref lc-table signal-index))
			 (sc-entry (vector-ref sc-table signal-index))
			 (id-entry (vector-ref id-table signal-index))
			)
			(if lc-entry
				(for.. (lambda (bit-index) (let
					((lc-bit (if lc-entry (vector-ref lc-entry bit-index) #f))
					 (id-bit (vector-ref id-entry bit-index))
					)
					(if (and lc-bit ; has a purpose
							(memq (car lc-bit) '(source proxied-source)) ; is a source
							(car id-bit) ; has some drive
							(> (cdr id-bit) load-threshold) ; `grace' load for unit drive
							(< (* l/d-ratio (car id-bit)) (cdr id-bit)) ; drive*l/d-ratio < load
						)
						(let*
							((source-icw (net-lc-bit-source-icw lc-bit))
							 (float-drive (/ (cdr id-bit) l/d-ratio))
							 (integer-drive (inexact->exact (floor float-drive)))
							)
							(if (and source-icw (> integer-drive 1))
								(begin
									(set! ret (cons
										(list
											circuit-name
											signal-index
											bit-index
											id-bit
											source-icw
											float-drive
										)
										ret
									))
								)
							)
						)
					)
				)) 0 (- (vector-length id-entry) 1))
			)
		)) 0 (- (vector-length lc-table) 1))
		ret
	)
))

;;; net-drive-report:{circuit-name...}: drive report elements
(define net-drive-report:circuit-name car)
(define net-drive-report:signal-index cadr)
(define net-drive-report:bit-index caddr)
(define net-drive-report:id-bit (nth 3))
(define net-drive-report:source-icw (nth 4))
(define net-drive-report:float-drive (nth 5))

;;; net-buffer: drive buffer cell name, FIXME, make tech attr.
(define net-buffer "q1bufd1")

(define do-replacement #t)

;;; net-insert-drive: follow a single drive-report to insert a drive-up on a
;;;		single signal bit.
(define net-insert-drive (lambda (instance-vector sc-table drive-report internal-signal)
	(let*
		((icw (net-drive-report:source-icw drive-report))
		 (instance (vector-ref instance-vector (car icw)))
		 (gate-name (net-instance:name instance))
		 (drive-up-entry (hash-ref net-drive-up-table gate-name))
		 (integer-drive (inexact->exact (floor (net-drive-report:float-drive drive-report))))
		 (find-drive (lambda (drive-up-entry drive-reqd)
			(let next
				((drives drive-up-entry))
				(cond
					((null? drives) #f)
					((null? (cdr drives)) (car drives))
					(else
						; drive reqd. is between this element and the next
						(if (and (<= (car (car drives)) drive-reqd)
								(> (car (cadr drives)) drive-reqd))
							(car drives)
							(next (cdr drives))
						)
					)
				)
			)
		 ))
		 (replacement-gate (if drive-up-entry (find-drive drive-up-entry integer-drive)))
		 (drive-close-enough (lambda (given-drive reqd-drive)
			(or (>= given-drive reqd-drive)
				(>= given-drive (* 0.75 reqd-drive))
			)
		 ))
		 (print-instance-info (lambda ()
			(print
				integer-drive
				" in: "
				(net-drive-report:circuit-name drive-report)
				" signal: "
				(net-connection:name (vector-ref sc-table (net-drive-report:signal-index drive-report)))
				"["
				(net-drive-report:bit-index drive-report)
				"] "
				(net-drive-report:id-bit drive-report)
				" from instance: "
				icw
				" (" gate-name ")"
				#\newline
			)
		 ))
		)
		(if (/= 0 (caddr icw)) ; wire-index
			(error "can't handle non 1 wire-indices in net-insert-drive yet")
		)
		(if replacement-gate
			(if (drive-close-enough (car replacement-gate) integer-drive)
				(if (string=? (cdr replacement-gate) gate-name) ; same gate type, no replacement
					(cons #f '())
					(begin
						(print-instance-info)
						(print " replace gate with `" (cdr replacement-gate) "'" #\newline)
						(if do-replacement
							(begin
								(set-car! (cdr instance) (cdr replacement-gate))
								(cons #f (list (cdr replacement-gate)))
							)
							(cons #f '())
						)
					)
				)
				(begin
					(print-instance-info)
					(print " gate drive not enough " replacement-gate " " integer-drive " using buffers" #\newline)
					(let*
						((buffer-drive-up-entry (hash-ref net-drive-up-table net-buffer))
						 (appropriate-buffer (find-drive buffer-drive-up-entry integer-drive))
						 (buffer-count (inexact->exact (floor (/ integer-drive (car appropriate-buffer)))))
						)
						(print " need to place " buffer-count " " (cdr appropriate-buffer) #\newline)
						(if do-replacement
							(begin
								(let*
									((connections (net-instance:instance-connections instance))
									 (original-dest-connection (list-ref connections (cadr icw)))
									)
									; change target signal in original gate
									(set-car! (list-tail connections (cadr icw)) internal-signal)
									(let next
										((instances '())
										 (count buffer-count)
										)
										(if (zero? count)
											(cons instances (list (cdr appropriate-buffer)))
											(next
												(cons
													; FIXME, need to respect signal ordering
													(list 'instance (cdr appropriate-buffer)
														(list original-dest-connection internal-signal))
													instances
												)
												(- count 1)
											)
										)
									)
								)
							)
							(cons #f '())
						)
					)
				)
			)
			(begin
				(print-instance-info)
				(error " can't find replacement gate")
			)
		)
	)
))

;;; net-insert-circuit-drives: insert drive-ups according to the list `drive-reports'
;;;		(as generated by net-summarise-circuit-drives)
(define net-insert-circuit-drives (lambda (circuit-tables drive-reports)
	(let*
		((internal-signal-prefix "drive")
		 (sc-table (net-tables:sc-table circuit-tables))
		 (instance-vector (net-tables:instance-vector circuit-tables))
		 ; insert new gates/drive up gates
		 (internal-signal-count/new-instances/new-cells (foldl-ma (lambda (drive-report signal-no instances cells)
		 	(let
		 		((new-instances/new-cells (net-insert-drive instance-vector sc-table drive-report
					(list internal-signal-prefix signal-no))
				))
		 		(if (car new-instances/new-cells)
		 			(list (+ 1 signal-no) (append (car new-instances/new-cells) instances)
		 				(append (cdr new-instances/new-cells) cells))
		 			(list signal-no instances (append (cdr new-instances/new-cells) cells))
		 		)
		 	)
		 ) drive-reports 0 '() '()))
		 (internal-signal-count (car internal-signal-count/new-instances/new-cells))
		 (new-instances (cadr internal-signal-count/new-instances/new-cells))
		 (new-cells (caddr internal-signal-count/new-instances/new-cells))
		)
		; insert a ``net'' entry in the circuit
		(if (/= 0 internal-signal-count)
			(let*
				((nets (net-circuit-decl:nets (net-tables:circuit-decl circuit-tables)))
				 (instances (net-circuit-decl:instances (net-tables:circuit-decl circuit-tables)))
				 (instances-contents (cdr instances))
				)
				(insert-after-car! nets
					(list internal-signal-prefix internal-signal-count)
				)
				(set-cdr! instances
					(append new-instances instances-contents)
				)
				(unsorted-uniq string=? new-cells)
			)
			new-cells
		)
	)
))

(define net-insert-netlist-drives (lambda (id-tables netlist-tables l/d-ratio load-threshold netlist)
	(unsorted-uniq string=?
		(foldl (lambda (new-cells circuit)
			(let*
				((circuit-name (net-circuit-decl:name circuit))
				 (id-table (hash-ref id-tables circuit-name))
				 (circuit-tables (hash-ref netlist-tables circuit-name))
				)
				(cond
					((not id-table)
						(print-err "can't find id-table entry for cell `" circuit-name "'" #\newline)
						new-cells
					)
					((not (net-circuit-is-driving-gate? circuit-name))
						(let ((drive-reports
							(net-summarise-circuit-drives id-table circuit-tables l/d-ratio
								load-threshold circuit-name)))
							(if (not (null? drive-reports))
								(append (net-insert-circuit-drives circuit-tables drive-reports) new-cells)
								new-cells
							)
						)
					)
					(else new-cells)
				)
			)
		) '() netlist)
	)
))

(define net-make-drive-up-table (lambda (gate-mappings)
	(let
		((ret (make-hash-table 57)))
		(for-each (lambda (gate)
			(let
				((drive/name-pairs (map
					(lambda (mapping-entry) (cons (car mapping-entry) (cadr mapping-entry))) (cddr gate))))
				(for-each (lambda (d/n)
					(hash-set! ret (cdr d/n) drive/name-pairs)
				) drive/name-pairs)
			)
		) gate-mappings)
		ret
	)
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define net-drive-up-table (net-make-drive-up-table breeze-gate-mappings))

(define w (lambda (netlist-filename top-name outfile-name)
	(print "reading netlist: " netlist-filename #\newline)
	(let	
		((netlist (get-file netlist-filename))
		 (before-netlist-filename (string-append outfile-name "-before.net"))
		 (after-netlist-filename (string-append outfile-name "-after.net"))
		 (changes-report-filename (string-append outfile-name "-changes"))
		 (instance-drive-filename (string-append outfile-name "-id"))
		 (circuit-tables-filename (string-append outfile-name "-circuit-tables"))
		 (instance-drive-table-notes-filename (string-append outfile-name "-id-notes"))
		 (c-tables (make-hash-table 57))
		)
		(print "building circuit tables and dumping to file: " circuit-tables-filename #\newline)
		(net-populate-netlist-tables-from-netlist c-tables netlist)
		(with-output-to-file circuit-tables-filename
			(lambda () (write c-tables) (newline))
		)
		(let
			((id-tables
				(with-output-to-file instance-drive-table-notes-filename (lambda ()
					(net-make-id-tables-starting-from-circuit c-tables top-name)
				))
			 )
			 (new-cells '())
			)
			(print "instance-drive tables dumped to file: " instance-drive-filename #\newline)
			(with-output-to-file instance-drive-filename
				(lambda () (write id-tables) (newline)))
			(print "writing ``before'' netlist file: " before-netlist-filename #\newline)
			(with-output-to-file before-netlist-filename (lambda ()
				(for-each (lambda (circuit)
					(write circuit) (newline)
				) netlist)
			))
			(print "inserting buffers/changing gates, report in file: " changes-report-filename #\newline)
			(with-output-to-file changes-report-filename (lambda ()
				(set! new-cells (net-insert-netlist-drives id-tables c-tables 120 120 netlist))
			))
			(print "writing ``after'' netlist file: " after-netlist-filename #\newline)
			(with-output-to-file after-netlist-filename (lambda ()
				(let*
					((inserted-cells (foldl (lambda (cells cell)
						(if (not (net-find-circuit netlist cell)) ; not in netlist
							(cons (net-find-circuit breeze-gate-defns cell) cells)
							cells
						)
					 ) '() new-cells))
					)
					(for-each (lambda (circuit)
						(write circuit) (newline)
					) inserted-cells)
					(for-each (lambda (circuit)
						(write circuit) (newline)
					) netlist)
				)
			))
		)
	)
))
