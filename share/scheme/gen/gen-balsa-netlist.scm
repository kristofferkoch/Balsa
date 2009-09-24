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
;;;	`gen-balsa-netlist.scm'
;;;	Breeze -> Netlist generator
;;;
;;; $Id: gen-balsa-netlist.scm,v 1.64 2004/07/13 09:32:54 tomsw Exp $
;;;

(balsa-scheme-import 'brz)
(balsa-scheme-import 'brz 'tech)
(balsa-scheme-import 'net 'parser)
(balsa-scheme-import 'net 'modify)
(balsa-scheme-import 'gen 'hcs)
(balsa-scheme-import 'misc 'switches)
(balsa-scheme-import 'misc 'banners)
(balsa-scheme-import 'gen 'undeclared)
(balsa-scheme-import 'gen 'netlists)
(balsa-scheme-import 'gen 'compositions)
(balsa-scheme-import 'net 'insert-buffers)

;;; balsa-netlist-{no-banner,...}: command line switches
(define balsa-netlist-no-banner #f)
(define balsa-netlist-force #f)
(define balsa-netlist-verbose #f)
(define balsa-netlist-test-comp #f) ;; Test abs component descriptions
(define balsa-netlist-other-netlists '())
(define balsa-netlist-make-cad-netlist #t)
(define balsa-netlist-read-old-cell-names #t)
(define balsa-netlist-features '())
(define balsa-included-cell-types '())
(define balsa-excluded-cell-names '())
(define balsa-netlist-import-path balsa-search-path)
(define balsa-netlist-file-list-filename #f)
(define balsa-netlist-build-all-parts #f)
(define balsa-netlist-log-stream #f)
(define balsa-netlist-log-file-name #f)
(define balsa-netlist-propagate-globals #f)
(define balsa-netlist-top-level-cell #f)
;;; added for inserting buffers
(define balsa-netlist-insert-buffers #f)


;;; balsa-netist-composition: choice of how to make Brc/Brz cells
;;;		flat: only make a Brz containing both instances and pin-pin connections
(define balsa-netlist-hc-composition 'flat)

;;; balsa-netlist-print-banner: pose, pose a bit more
(define balsa-netlist-print-banner (lambda ()
	(make-program-banner "balsa-netlist" "balsa-netlist: Netlist generator"
		"1999-2003, The University of Manchester")
))

;;; balsa-netlist-usage: command usage
(define balsa-netlist-usage (lambda ()
	(balsa-netlist-print-banner)
	(error
		"version " balsa-version #\newline
		"usage: balsa-netlist {<switch>}* <block/file-name>" #\newline #\newline
		"switches: -h or -?    - Display this message (--help)" #\newline
		"          -b          - Don't print the balsa-netlist banner (--no-banner)" #\newline
		"          -v          - Be verbose, print cell names as they are produced (--verbose)" #\newline
		"          -c          - Don't try to make a CAD system native netlist (--no-cad-netlist)" #\newline
		"          -m          - Don't read in old cell name mappings from the .map file" #\newline
		"                        (--no-old-cell-names)" #\newline
		"          -n <format> - Dump a netlist in the given format (edif, verilog, compass ...)" #\newline
		"                        as well as any other scheduled netlist writes, several -n can be" #\newline
		"                        used (--make-other-netlist)" #\newline
		"                        NB. Name mapping/mangling occurs when the internal netlist is" #\newline
		"                        generated, all of these additional netlists will contain names" #\newline
		"                        mapped to work with the default format" #\newline
		"          -d          - Don't print prototypes for undefined cells (where appropriate)" #\newline
		"                        (--no-prototypes)" #\newline 
		"          -p          - Do print prototypes for undefined cells (where appropriate)" #\newline
		"                        (--prototypes)" #\newline 
		"          -i <type>   - Add cell type <type> to the list of cell types to netlist.  If no" #\newline
		"                        additional cell types are given, then only the netlist" #\newline
		"                        definitions for Balsa cells are emitted (--include-cell-type)" #\newline
        "          -x <cellname> - Exclude the cell <cellname> from the generated netlist" #\newline
        "                        No definition or prototype will be emitted (--exclude-cell)" #\newline
		"          -I <directory> - Add named directory to the Breeze import path (--import)" #\newline
		"          -t <component> <args> - create test component (--test-component)" #\newline
		"          -l <filename> - Make a list of generated files in file <filename> (--file-list)" #\newline
		"          -a          - Emit definitions for all parts found even if the top level" #\newline
		"                        block doesn't need them (--all-parts)" #\newline
		"          -s          - Insert simulation initialisation code in netlist formats which" #\newline
		"                        support this option (--simulation-initialise)" #\newline
		"          -L <filename> - write a log of balsa-netlist messages to file <filename> (--log)" #\newline
		"          -f          - Replace feedthrough cells with netlist appropriate aliases" #\newline
		"                        (--replace-feedthroughs)" #\newline
		"          -g          - Propagate global ports on cells (--propagate-globals)" #\newline
        "          -e          - Produce encounter compatible netlist (--encounter)" #\newline
        "          -B          - Automatic buffer insertion (--insert-buffers)" #\newline
        "         --verbose-bi - Verbose automatic buffer insertion" #\newline
	)
))

;;; balsa-netlist-command-line-rules: command-line-args action rules
(define balsa-netlist-command-line-rules `(
	(#\b "no-banner" 0 ,(lambda (args) (set! balsa-netlist-no-banner #t)))
	(#\n "make-other-netlist" 1 ,(lambda (args) (set! balsa-netlist-other-netlists
		(append balsa-netlist-other-netlists (list (car args))))))
	(#\c "no-cad-netlist" 0 ,(lambda (args) (set! balsa-netlist-make-cad-netlist #f)))
	(#\h "help" 0 ,(lambda (args) (balsa-netlist-usage)))
	(#\? "help" 0 ,(lambda (args) (balsa-netlist-usage)))
	(#\v "verbose" 0 ,(lambda (args) (set! balsa-netlist-verbose #t)))
	(#\m "no-old-cell-names" 0 ,(lambda (args) (set! balsa-netlist-read-old-cell-names #f)))
	(#\d "no-prototypes" 0 ,(lambda (args) (set! balsa-netlist-features (cons
		(cons 'emit-prototypes #f) balsa-netlist-features))))
	(#\p "prototypes" 0 ,(lambda (args) (set! balsa-netlist-features (cons
		(cons 'emit-prototypes #t) balsa-netlist-features))))
	(#\i "include-cell-type" 1 ,(lambda (args) (set! balsa-included-cell-types
		(cons (car args) balsa-included-cell-types))))
	(#\x "exclude-cell" 1 ,(lambda (args) (set! balsa-excluded-cell-names
		(cons (car args) balsa-excluded-cell-names))))
	(#\I "import" 1 ,(lambda (args) (set! balsa-netlist-import-path (append balsa-netlist-import-path (list (car args))))))
	(#\t "test-component" 0 ,(lambda (args) (set! balsa-netlist-test-comp #t)))
	(#\a "all-parts" 0 ,(lambda (args) (set! balsa-netlist-build-all-parts #t)))
	(#\l "file-list" 1 ,(lambda (args) (set! balsa-netlist-file-list-filename (car args))))
	(#\s "simulation-initialise" 0 ,(lambda (args) (set! balsa-netlist-features (cons
		'(simulation-initialise) balsa-netlist-features))))
	(#\f "replace-feedthroughs" 0 ,(lambda (args) (set! balsa-netlist-features (cons
		'(replace-feedthroughs) balsa-netlist-features))))
	(#\L "log" 1 ,(lambda (args)
		(set! balsa-netlist-log-file-name (car args))
		(set! balsa-netlist-log-stream (open-output-file balsa-netlist-log-file-name))
	))
	(#\g "propagate-globals" 0 ,(lambda (args) (set! balsa-netlist-propagate-globals #t)))
	(#\e "encounter" 1 ,(lambda (args) (set! balsa-netlist-top-level-cell (car args))  (set! balsa-netlist-features (cons
		`(top-level-cell ,(car args)) balsa-netlist-features)))) ;; feature of netlists - allows for handling of global gnd/vcc
    (#\B "insert-buffers" 0 ,(lambda (args) (set! balsa-netlist-insert-buffers #t)))
    (#\_ "verbose-bi" 0 ,(lambda (args) (set! balsa-netlist-insert-buffers #t)
                                                    (set! balsa-insert-b-verbose #t)))
))

;;; balsa-netlist-parse-command-line: parse switches from the given command line list, set
;;;		the balsa-netlist-... globals and return the tail of the command line.
(define balsa-netlist-parse-command-line (lambda (args)
	(if (null? args)
		(balsa-netlist-usage)
		(let
			((args-tail (parse-command-line "balsa-netlist"
				balsa-netlist-command-line-rules balsa-netlist-usage args))
			)
			(if (and (/= 1 (length args-tail)) (not balsa-netlist-test-comp))
			(balsa-netlist-usage)
			args-tail
			)
		)
	)
))

(define note (lambda args
	(if balsa-netlist-log-stream (apply print-to-port balsa-netlist-log-stream args))
	(apply print args)
))

;;; backslash-escape-string : replace NL in string with \n
(define backslash-escape-string (lambda (string)
	(list->string (reverse! (foldl (lambda (ret char)
		(if (char=? char #\nl)
			(cons #\n (cons #\\ ret))
			(cons char ret)
		)
	) '() (string->list string))))
))

(define pretty-hc-parameters (lambda (params)
	(build-separated-string (map (lambda (param)
		(cond
			((number? param) (number->string param))
			((symbol? param) (symbol->string param))
			((string? param)
				(string-append "\"" (backslash-escape-string param) "\"")
			)
			(else (->string param))
		)
	) params) " ")
))

;;; balsa-netlist-normalise-features: remove repeated elements from the feature list
(define balsa-netlist-normalise-features (lambda ()
	(set! balsa-netlist-features
		(list*
			(cons 'included-cell-types balsa-included-cell-types)
			(cons 'excluded-cell-names balsa-excluded-cell-names)
			(unsorted-uniq (lambda (a b) (eq? (car a) (car b))) balsa-netlist-features)
		)
	)
))

(define balsa-netlist-write-file-list-file (lambda ()
	(note "writing file list to file: `" balsa-netlist-file-list-filename "'" #\newline)
	(with-output-to-file balsa-netlist-file-list-filename (lambda ()
		(for-each (lambda (filename)
			(print filename #\newline)
		) net-session-file-list)
		(if balsa-netlist-log-stream
			(print balsa-netlist-log-file-name #\newline)
		)
		;;; (print balsa-netlist-file-list-filename #\newline)
	))
))

;;; balsa-netlist-convert: convert a .net netlist into the CAD native format
(define balsa-netlist-convert (lambda (filename)
	(balsa-set-tech #f)
	(brz-load-gates)
	(let*
		((base-filename (let ((root (final-string filename ".net"))) (if root root filename)))
		 (netlist (get-file filename))
		 (top-level-block (car (last-pair netlist)))
		 (netlist-filename (string-append base-filename "." tech-filename-suffix))
		)
		(note "using technology: `" (brz-pretty-format-tech-name) "'" #\newline)
		(note "processing Balsa netlist: `" filename "'" #\newline)
		(note #\newline)

		(note "making netlists" #\newline)

		(balsa-netlist-normalise-features)

		(if balsa-netlist-make-cad-netlist (begin
			(note "writing CAD native netlist to file: `" netlist-filename "'" #\newline)
			(apply tech-write-netlist-file (list* netlist netlist
				(net-circuit-decl:name top-level-block) netlist-filename balsa-netlist-features))
		))

		(if (not (null? balsa-netlist-other-netlists))
			(for-each (lambda (format)
				(let*
					((signature (net-signature-for-netlist-format format #f))
					 (filename (string-append base-filename "." (net-signature:filename-suffix signature)))
					)
					(note "writing " format " netlist to file: `" filename "'" #\newline)
					(apply (net-signature:write-netlist-file signature)
						(list* netlist top-level-block (net-circuit-decl:name top-level-block) filename
						balsa-netlist-features))
				)
			) balsa-netlist-other-netlists)
		)

		(note "finished writing netlist" #\newline)
		(if balsa-netlist-file-list-filename (balsa-netlist-write-file-list-file))
		(if balsa-netlist-log-stream (close-port balsa-netlist-log-stream))
	)
))

;;; convert-test-param: converts input test params into relevent type.
(define convert-test-param (lambda (a-param f-param)
	(cond 
		((/= (length (cadr f-param)) 1) ;; If not string
			(let*
				((param-type-dec (cadr f-param)) ;;parameter-type-description
				(param-type (car param-type-dec))) ;; actual type
				(cond
					((eqv? param-type 'numeric-type) (string->number a-param))
					((eqv? param-type 'named-type) 
						(let*
							((param-type-name (cadr param-type-dec))
							(get-type-decl (lambda (elem) (string=? param-type-name (brz-type-decl:name elem))))
							(type-decl (find-with-predicate brz-type-context get-type-decl))
							(type-type (car (brz-type-decl:body type-decl))))
							(if (string=? "boolean" param-type-name)
								a-param ; removed symbol conv. 2002-05-06 AB
								(cond
									((eqv? type-type 'numeric-type) (string->number a-param))
									((eqv? type-type 'enumeration-type) a-param)
									(else a-param) ; removed symbol conv. 2002-05-06 AB
								)
							)
						)
					)
				)
			)
		)
		(else a-param)
	)
))

;;; balsa-netlist-test: generate netlists for the given HC Component
(define balsa-netlist-test (lambda (args)
	(balsa-set-tech #f)
	(brz-load-primitives)
	(brz-load-gates)
	(let*
		((part-name (car args))
		 (part-params (cdr args))
		 (part-primitive (brz-find-primitive part-name))
		 (formal-parameters
			(let 
				((fps (cdr (brz-primitive-part:parameters part-primitive)))
				 (is-significant-formal-param? (lambda (param) (not (memv 'not-used (cddr param)))))
				)
				(if (/= (length fps) (length part-params))
					(filter is-significant-formal-param? fps)
					fps
				)
		 	)
		 )
		 (param-list (map convert-test-param part-params formal-parameters))
		 (netlist (gen-make-hc-netlist part-name param-list brz-type-context balsa-netlist-hc-composition))
		 (defn (append netlist breeze-gate-defns))
		 (globals (if balsa-netlist-propagate-globals
				(begin
			 		(note "propagating global signals" #\newline)
			 		(balsa-netlist-add-globals! defn)
			 	)
			))
		 (rails (if balsa-netlist-top-level-cell
				(begin
			 		(note "propagating global rails" #\newline)
			 		(balsa-netlist-add-global-rails! defn)
			 	)
			))
		 (netlist-filename (string-append part-name "." tech-filename-suffix))
		 (celllist-filename (string-append part-name ".lst"))
		 (symbol-filename (if tech-write-symbol-file (string-append part-name "." tech-symbol-filename-suffix) #f))
		 (cell-name-mapping-file (string-append breeze-tech ".map"))
		)
		(note "using technology: `" (brz-pretty-format-tech-name) "'" #\newline)
		(note "processing component: `" part-name "'")
		(note " (" (pretty-hc-parameters param-list) ")" #\newline)
		(if (and balsa-netlist-read-old-cell-names (file-exists? cell-name-mapping-file) tech-map-cell-name-import)
			(begin
				(note "reading cell name mapping file: `" cell-name-mapping-file "'" #\newline)
				(tech-map-cell-name-import cell-name-mapping-file)
			)
		)
		
		(balsa-netlist-normalise-features)

		(if balsa-netlist-make-cad-netlist (begin
			(note "writing CAD native netlist to file: `" netlist-filename "'" #\newline)
			(apply tech-write-netlist-file
				(list* defn netlist #f netlist-filename balsa-netlist-features))
		))

		(if (not (null? balsa-netlist-other-netlists))
			(for-each (lambda (format)
				(let*
					((signature (net-signature-for-netlist-format format #f))
					 (filename (string-append part-name "." (net-signature:filename-suffix signature)))
					)
					(note "writing " format " netlist to file: `" filename "'" #\newline)
					(apply (net-signature:write-netlist-file signature)
						(list* defn netlist #f filename balsa-netlist-features))
				)
			) balsa-netlist-other-netlists)
		)

		(if symbol-filename (begin
			(note "writing symbol description file: `" symbol-filename "'" #\newline)
			(verilog-write-pin-file netlist symbol-filename)
		))

		(note "writing cell list to file: `" celllist-filename "'" #\newline)
		(net-add-file-to-session-file-list celllist-filename)
		(with-output-to-file celllist-filename (lambda ()
			(for-each (lambda (cell)
				(print (net-circuit-decl:name cell) #\newline)
			) netlist)
		))
		(note "finished writing netlist" #\newline)
		
		(if tech-map-cell-name-export
			(begin
				(note "writing cell name mapping file: `" cell-name-mapping-file "'" #\newline)
				(tech-map-cell-name-export cell-name-mapping-file)
			)
		)
	)
))

;;; balsa-netlist-generate: generate netlists for the given Balsa module
(define balsa-netlist-generate (lambda (filename)
	(balsa-set-tech #f)
	(brz-load-primitives)
	(brz-load-gates)
	(let*
		; "my_proc" =eg=> "Balsamy_proc"
		(;(mangled-proc-name (string-append tech-balsa-prefix (tech-map-name proc-name)))
		 ; top-level/imported-blocks/imported-decls/visited-blocks
		 (path/name/ext (find-filename filename "breeze" #f balsa-netlist-import-path))
		 (flat-path (apply string-append path/name/ext))
		 (t/i/i-d/v (begin
		 	(note "loading file: `" flat-path "'" #\newline)
		 	(get-flattened-block flat-path "breeze" '() balsa-netlist-import-path)
		 ))
		 (top-level-decls (car t/i/i-d/v))
		 (import-blocks (cadr t/i/i-d/v))
		 (imported-decls (caddr t/i/i-d/v))
		 (visited-blocks (cadddr t/i/i-d/v))
		 ; is-breeze-entity?: is the given decl a breeze-part, breeze-composition or breeze-netlist
		 (is-breeze-entity? (lambda (decl) (and (headed-list? decl #f)
		 	(memq (car decl) '(breeze-part breeze-composition breeze-netlist)))))
		 (local-entities (filter is-breeze-entity? top-level-decls))
		 (imported-entities (filter is-breeze-entity? imported-decls))
		 (all-entities (append local-entities imported-entities))
		 ; flat-dependencies: dependency pair (parts-in-order . hcs) for all parts in local scope
		 (flat-dependencies (begin
		 	(note "determining dependencies" #\newline)
		 	(foldl (lambda (visited entity)
				(brz-breeze-entity-flat-dependencies visited all-entities entity)
			) '(() () () () ()) (if balsa-netlist-build-all-parts all-entities local-entities))
		 ))
		 (parts-to-build (reverse (car flat-dependencies)))
		 (compositions-to-build (reverse (cadr flat-dependencies)))
		 (netlists-to-build (reverse (caddr flat-dependencies)))
		 (undeclareds-to-build
		 	(uniq str/int/sym-list<=? (merge-sort! str/int/sym-list<=?
				(apply append! (list-ref flat-dependencies 3))))
		 )
		 (hcs-to-build
		 	(uniq str/int/sym-list<=? (merge-sort! str/int/sym-list<=?
		 		(apply append! (list-ref flat-dependencies 4))))
		 )
		 (is-type-decl? (lambda (decl) (headed-list? decl 'type)))
		 (all-types (append (filter is-type-decl? top-level-decls) (filter is-type-decl? imported-decls)))
		 (base-name (cadr path/name/ext))
		 (netlist-filename (string-append base-name "." tech-filename-suffix))
		 (celllist-filename (string-append base-name ".lst"))
		 (symbol-filename (if tech-write-symbol-file (string-append base-name "." tech-symbol-filename-suffix) #f))
		 (cell-name-mapping-file (string-append breeze-tech ".map"))
		 ; report-required-entities : pretty print function to report entities to be built
		 (report-required-entities (lambda (name find-procedure entities)
			(if (not (null? entities)) (begin
				(note "required " name " (in order, [top-level]):" #\newline)
				(for-each (lambda (entity)
					(let ((entity-name (cadr entity)))
						(if (find-procedure local-entities entity-name)
							(note "[" entity-name "]")
							(note " " entity-name)
						)
						(note #\newline)
					)
				) entities)
				(note #\newline)
			))
		 ))
		)

		(note "using technology: `" (brz-pretty-format-tech-name) "'" #\newline)
		(note "processing file/block: `" filename "'" #\newline)
		(report-required-entities "Breeze netlists" brz-find-breeze-netlist netlists-to-build)
		(report-required-entities "Breeze compositions" brz-find-breeze-composition compositions-to-build)
		(report-required-entities "Breeze parts" brz-find-breeze-part parts-to-build)
		(note "making these HCs:")
		(foldl (lambda (last-hc-type hc)
			(if (not (string=? last-hc-type (car hc)))
				(note #\newline (car hc) ":")
			)
			(note " (" (pretty-hc-parameters (cdr hc)) ")")
			(car hc)
		) "" hcs-to-build)
		(note #\newline "finished composing HCs" #\newline)

		(if (not (null? undeclareds-to-build)) (begin
			(note #\newline "making these parameterised builtin functions:" #\newline)
			(for-each (lambda (undeclared)
				(if (not (gen-undeclared-is-builtin-function? (cddr undeclared)))
					(error "balsa-netlist: undeclared-component `" (car undeclared)
						"' must implement builtin-functions" #\newline)
					(print (car undeclared) " (" (pretty-hc-parameters (cadr undeclared)) ")" #\newline)
				)
			) undeclareds-to-build)
			(note #\newline)
		))

		(if (and balsa-netlist-read-old-cell-names (file-exists? cell-name-mapping-file) tech-map-cell-name-import)
			(begin
				(note "reading cell name mapping file: `" cell-name-mapping-file "'" #\newline)
				(tech-map-cell-name-import cell-name-mapping-file)
			)
		)
		(note #\newline)
		(let*
			; make-netlist-types : do fancy progress bar netlist construction
			((make-netlist-types (lambda (name sources source-name-proc build-proc)
				(if (null? sources)
					(begin (note "No " name " netlists to build" #\newline) '())
					(begin
						(note "making " name " netlists (" (length sources) " to build)" #\newline)
						(let ((netlists (foldl-with-progress-bar (lambda (acc elem) (append! acc (build-proc elem)))
							(if balsa-netlist-verbose source-name-proc #f) note '() sources)))
							(note #\newline)
							(if (not balsa-netlist-verbose) (note #\newline))
							netlists
						)
					)
				)
			 ))
			 (hc-netlists (make-netlist-types "HC" hcs-to-build
				(lambda (hc) (string-append (car hc) ": " (->string (cdr hc))))
				(lambda (hc) (gen-make-hc-netlist
					(car hc) (cdr hc) brz-type-context balsa-netlist-hc-composition))
			 ))
			 (undeclared-netlists (make-netlist-types "undeclared component" undeclareds-to-build
				(lambda (undeclared) (string-append (car undeclared) ": " (pretty-hc-parameters (cadr undeclared))))
				(lambda (undeclared) (gen-make-undeclared-component-netlist
					(car undeclared) (cadr undeclared) (cddr undeclared) all-types))
			 ))
			 (netlist-netlists (make-netlist-types "Breeze netlist" netlists-to-build
				brz-breeze-netlist:name
				(lambda (netlist) (list (gen-make-netlist-netlist netlist)))
			 ))
			 (composition-netlists (make-netlist-types "Breeze composition" compositions-to-build
			 	brz-breeze-composition:name
				(lambda (comp) (gen-make-composition-netlist comp all-types)) 
			 ))
			 (part-netlists (make-netlist-types "Breeze part" parts-to-build brz-breeze-part:name
				(lambda (part) (list
					(if (find-headed-sub-list (brz-breeze-part:attributes part) 'is-builtin-function)
						(gen-make-builtin-function-part-netlist part all-types)
						(gen-make-breeze-part-netlist part all-types ;; buffer top-level ports
							(and balsa-netlist-top-level-cell (string=? balsa-netlist-top-level-cell (brz-breeze-part:name part))))
					)
				))
			 ))
			 (final-netlist (append hc-netlists
			 	netlist-netlists
			 	composition-netlists
			 	undeclared-netlists
			 	part-netlists
			 ))
             (all-defns (if balsa-netlist-insert-buffers
                            (append breeze-gate-defns (net-insert-buffers final-netlist))
                            (append breeze-gate-defns final-netlist)
             ))
			)
			(if balsa-netlist-propagate-globals
				(let
					((global-signals (balsa-netlist-add-globals! all-defns))
					)
					(begin
			 			(note "propagating global signals" #\newline)
			 			(set! balsa-netlist-features (cons (cons 'global-nets global-signals) balsa-netlist-features))
			 		)
				)
			)
			(if balsa-netlist-top-level-cell
				(begin
			 		(note "propagating global rails" #\newline)
			 		(balsa-netlist-add-global-rails! all-defns)
			 	)
			)
			(balsa-netlist-normalise-features)

			(if (not (null? balsa-netlist-other-netlists))
				(for-each (lambda (format)
					(let*
						((signature (net-signature-for-netlist-format format #f))
						 (filename (string-append base-name "." (net-signature:filename-suffix signature)))
						)
						(note "writing " format " netlist to file: `" filename "'" #\newline)
						(apply (net-signature:write-netlist-file signature)
							(list* all-defns final-netlist #f filename balsa-netlist-features))
					)
				) balsa-netlist-other-netlists)
			)

			(if balsa-netlist-make-cad-netlist (begin
				(note "writing CAD native netlist to file: `" netlist-filename "'" #\newline)
				(apply tech-write-netlist-file
					(list* all-defns final-netlist #f netlist-filename balsa-netlist-features))
			))

			(if symbol-filename (begin
				(note "writing symbol description file: `" symbol-filename "'" #\newline)
				(verilog-write-pin-file final-netlist symbol-filename)
			))

			(note "writing cell list to file: `" celllist-filename "'" #\newline)
			(net-add-file-to-session-file-list celllist-filename)
			(with-output-to-file celllist-filename (lambda ()
				(for-each (lambda (cell)
					(print (net-circuit-decl:name cell) #\newline)
				) final-netlist)
			))
			(note "finished writing netlist" #\newline)
		)
		(if tech-map-cell-name-export
			(begin
				(note "writing cell name mapping file: `" cell-name-mapping-file "'" #\newline)
				(net-add-file-to-session-file-list cell-name-mapping-file)
				(tech-map-cell-name-export cell-name-mapping-file)
			)
		)
		(if balsa-netlist-file-list-filename (balsa-netlist-write-file-list-file))
		(if balsa-netlist-log-stream (close-port balsa-netlist-log-stream))
	)
))
