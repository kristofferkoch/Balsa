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
;;;	`brz-breeze2ps.scm'
;;;	Make pretty printable Postscript piccies of Breeze files
;;;	Adapted from the original breeze2ps Perl
;;;
;;;	$Id: brz-breeze2ps.scm,v 1.22 2004/05/05 13:59:41 janinl Exp $
;;;

(balsa-scheme-import 'brz)
(balsa-scheme-import 'misc 'switches)
(balsa-scheme-import 'misc 'banners)

;;; dot-print-ports: print dot(1) nodes for each of the ports in `ports'.
(define dot-print-ports (lambda (ports)
	(print "subgraph ports {" #\newline)
	(foldl (lambda (index port)
		(case (car port)
			((sync-port port)
				(print "port" (+ 1 index) " [label=\"" (brz-port:name port) "\" " dot-port-style " ]" #\newline)
				(+ 1 index)
			)
			((arrayed-sync-port)
				(let
					((low-index (brz-arrayed-sync-port:low-index port))
					 (port-count (brz-arrayed-sync-port:port-count port))
					 (port-name (brz-arrayed-sync-port:name port))
					)
					(for.. (lambda (i)
						(print "port" (+ 1 index (- i low-index)) " [label=\"" port-name
							"[" (number->string i) "]\" " dot-port-style " ]" #\newline)
					) low-index (+ low-index port-count -1))
					(+ index port-count)
				)
			)
			((arrayed-port)
				(let
					((low-index (brz-arrayed-port:low-index port))
					 (port-count (brz-arrayed-port:port-count port))
					 (port-name (brz-arrayed-port:name port))
					)
					(for.. (lambda (i)
						(print "port" (+ 1 index (- i low-index)) " [label=\"" port-name
							"[" (number->string i) "]\" " dot-port-style " ]" #\newline)
					) low-index (+ low-index port-count -1))
					(+ index port-count)
				)
			)
		)
	) 0 ports)
	(print "}" #\newline)
))

;;; brz-specify-case-expr: evaluate a case expression called with value `switch-eval'
;;;		where `cases' is a list of (matches expr) pairs (or ('else expr)) and `eval-proc'
;;;		is a procedure capable of evaluating a sub-expression.  Returns #f if no other
;;;		option presents itself.
(define brz-specify-case-expr (lambda (switch-val cases eval-proc)
	(cond
		((null? cases) #f)
		((headed-list? (car cases) 'else) (eval-proc (cadar cases)))
		(else (let
			((match-values (map eval-proc (caar cases))))
			(if (member switch-val match-values)
				(eval-proc (cadar cases))
				(brz-specify-case-expr switch-val (cdr cases) eval-proc)
			)
		))
	)
))

;;; brz-specify-symbol-expr: specify the expressions used in the .abs files to
;;;		define the symbol used in the centre of the HCs on a printout.  This
;;;		procedure just returns a string.
(define brz-specify-symbol-expr (lambda (expr actual formal)
	(let ((specify-expr (lambda (expr) (brz-specify-expression expr '() actual formal))))
		(cond
			((headed-list? expr)
				(case (car expr)
					((centre-string)
						(foldl (lambda (str arg)
							(let ((specified-arg (specify-expr arg)))
								(string-append str
									(if (string? specified-arg)
										specified-arg
										(->string specified-arg)
									)
								)
							)
						) "" (cdr expr))
					)
					(else "NO NAME")
				)
			)
			(else "")
		)
	)
))

(define dot-component-label (lambda (comp)
	(let ((primitive-part-name (brz-get-primitive-part-name (brz-component:name comp))))
		(if primitive-part-name
			(let ((part (brz-find-primitive primitive-part-name)))
				(brz-specify-symbol-expr (cadr (brz-primitive-part:symbol part))
					(brz-component:parameters comp) (brz-primitive-part:parameters part))
			)
			(brz-component:name comp)
		)
	)
))

(define dot-print-components (lambda (comps label)
	(print "subgraph cluster_components {" #\newline)
	(print " label=\"" label "\"" #\newline)
	(foldl (lambda (index comp)
		(print "comp" index " [label=\"" (dot-component-label comp) "\"]" #\newline)
		(+ 1 index)
	) 0 comps)
	(print "}" #\newline)
))

(define dot-arc-label (lambda (chan-no chan chan-links comps)
	(let
		((comp-type (lambda (index)
			(brz-component:name (vector-ref comps index))
		)))
		(string-append
			(string-append "C" (number->string chan-no) ": " (brz-channel:name chan))
;;;			(if (eq? 'no-component (vector-ref chan-links 2)) ; active end is port?
;;;				""
;;;				(string-append "\\n* " (comp-type (vector-ref chan-links 2)))
;;;			)
;;;			(if (eq? 'no-component (vector-ref chan-links 0)) ; passive end is port?
;;;				""
;;;				(string-append "\\no " (comp-type (vector-ref chan-links 0)))
;;;			)
		)
	)
))

(define dot-arc-end-label (lambda (passiveNactive chan-no chan chan-links comps)
	(let
		((comp-type (lambda (index)
			(brz-component:name (vector-ref comps index))
		 ))
		 (comp-offset (if passiveNactive 0 2))
		)
		(if (eq? 'no-component (vector-ref chan-links comp-offset))
			""
			(number->string (vector-ref chan-links (+ 1 comp-offset)))
		)
	)
))

(define dot-chan-arrow-style (lambda (chan)
	(case (car chan)
		((sync) "arrowhead=odot arrowtail=dot dir=forward")
		((push) "arrowhead=normal arrowtail=dot dir=forward")
		((pull) "arrowhead=odot arrowtail=normal dir=back")
	)
))

(define dot-print-arcs (lambda (channels channel-links comps)
	(foldl (lambda (index chan)
		(let ((chan-links (vector-ref channel-links index)))
			(if (eq? 'no-component (vector-ref chan-links 2))
				(print "port" (+ 1 index))
				(print "comp" (vector-ref chan-links 2))
			)
			(print " -> ")
			(if (eq? 'no-component (vector-ref chan-links 0))
				(print "port" (+ 1 index))
				(print "comp" (vector-ref chan-links 0))
			)
			(print " [ label=\"" (dot-arc-label (+ 1 index) chan chan-links comps) "\" "
				(dot-chan-arrow-style chan) " "
				"headlabel=\"" (dot-arc-end-label #t (+ 1 index) chan chan-links comps) "\" " 
				"taillabel=\"" (dot-arc-end-label #f (+ 1 index) chan chan-links comps) "\" " 
				"]" #\newline)
		) (+ 1 index)
	) 0 channels)
))

(define make-part-dot-code (lambda (procedure context)
	(let*
		((channel-count (length (brz-breeze-part:channels procedure)))
		 (linked-c-and-c (brz-link-channels-and-components
			(- channel-count 1)
			(brz-breeze-part:components procedure) context)) ; Join channels to components
		 (channels (car linked-c-and-c))
		 (components (cadr linked-c-and-c))
		 (attributes (brz-breeze-part:attributes procedure))
		 (proc-line-no (assoc "line" (cdr attributes)))
		)
;		(print "size=\"7x10\"" #\newline)
		(print "digraph anything {" #\newline)
		(if breeze2ps-landscape
			(print "size=\"10,7\"" #\newline "rotate=90" #\newline)
			(print "size=\"7,10\"" #\newline)
		)
		(print "edge [ " dot-edge-style " ]" #\newline)
		(print "node [ " dot-node-style " ]" #\newline)
		(dot-print-ports (cdr (brz-breeze-part:ports procedure)))
		(dot-print-components (cdr (brz-breeze-part:components procedure)) (brz-breeze-part:name procedure))
		(dot-print-arcs (cdr (brz-breeze-part:channels procedure)) channels components)
		(print "}" #\newline)
	)
))

;;; breeze2ps-{no-banner,...}: command line switches
(define breeze2ps-no-banner #f)
(define breeze2ps-keep-dot-files #f)
(define breeze2ps-single-file #t)
(define breeze2ps-landscape #f)
(define breeze2ps-import-path balsa-search-path)

;;; breeze2ps-print-banner: print the breeze2ps banner
(define breeze2ps-print-banner (lambda ()
	(make-program-banner "breeze2ps" "breeze2ps: Breeze -> Postscript Converter"
		"2000, The University of Manchester")
))

;;; breeze2ps-usage: command usage
(define breeze2ps-usage (lambda ()
	(breeze2ps-print-banner)
	(error
		"version " balsa-version #\newline
		"usage: breeze2ps {<switch>}* <block/file-name>" #\newline #\newline
		"switches: -h or -?           - Display this message (--help)" #\newline
		"          -b                 - Don't print the breeze2ps banner (--no-banner)" #\newline
		"          -k                 - Keep intermediate .dot files (--keep-dot-files)" #\newline
		"          -s                 - Create separate files for each procedure (--separate-files)" #\newline
		"          -l                 - Produce a landscape plot (--landscape)" #\newline
		"          -p                 - Produce a portrait (default) plot (--portrait)" #\newline
		"          -I <directory>     - Add named directory to the import path (--import)" #\newline
	)
))

;;; breeze2ps-command-line-rules: command-line-args action rules
(define breeze2ps-command-line-rules `(
	(#\b "no-banner" 0 ,(lambda (args) (set! breeze2ps-no-banner #t)))
	(#\h "help" 0 ,(lambda (args) (breeze2ps-usage)))
	(#\? "help" 0 ,(lambda (args) (breeze2ps-usage)))
	(#\k "keep-dot-files" 0 ,(lambda (args) (set! breeze2ps-keep-dot-files #t)))
	(#\s "separate-files" 0 ,(lambda (args) (set! breeze2ps-single-file #f)))
	(#\l "landscape" 0 ,(lambda (args) (set! breeze2ps-landscape #t)))
	(#\p "portrait" 0 ,(lambda (args) (set! breeze2ps-landscape #f)))
	(#\I "import" 1 ,(lambda (args) (set! breeze2ps-import-path (append breeze2ps-import-path
		(list (car args))))))
))

;;; breeze2ps-parse-command-line: parse switches from the given command line list, set
;;;		the breeze2ps-... globals and return the tail of the command line.
(define breeze2ps-parse-command-line (lambda (args)
	(if (null? args)
		(breeze2ps-usage)
		(let ((args-tail (parse-command-line "breeze2ps" breeze2ps-command-line-rules breeze2ps-usage args)))
			(if (/= 1 (length args-tail))
				(error "breeze2ps: expecting exactly one file name" #\newline)
				args-tail
			)
		)
	)
))

;;; dot-{port,edge,...}-style: dot(1) definitions for graph component styles
(define dot-port-style "style=bold shape=box")
(define dot-edge-style "decorate=1 fontsize=8 fontname=Helvetica labelfontname=Helvetica labelfontsize=8")
(define dot-node-style "fontsize=12 fontname=\"Helvetica-Bold\" shape=ellipse")

;;; breeze2ps: main, reads from the file `filename'
(define breeze2ps (lambda (filename)
	(balsa-set-tech "common")
	(brz-load-primitives)
	(let*
		((path/name/ext (find-filename filename "breeze" #f breeze2ps-import-path))
		 (path (car path/name/ext))
		 (name (cadr path/name/ext))
		 (decls/imports/imported-decls/visited-blocks (get-flattened-block
			(foldl string-append "" path/name/ext) "breeze" '() breeze2ps-import-path))
		 (decls (car decls/imports/imported-decls/visited-blocks))
		 (imports	
			(merge-sort! string<=? (cadr decls/imports/imported-decls/visited-blocks))
		 )
		 (imported-decls (caddr decls/imports/imported-decls/visited-blocks))
		 (context (append decls imported-decls))
		 (parts (rev-filter brz-check-breeze-part? decls)) ; Breeze parts read form the input file
		 (imported-types (filter brz-check-type-decl? imported-decls))
		 (local-types (filter brz-check-type-decl? decls))
		 (types (append local-types imported-types))
		 ; prune-file/run-dot : helper procedures for later stuff
		 (prune-file (lambda (filename)
			(if (not breeze2ps-keep-dot-files)
				(system (string-append "/bin/rm -f " filename))
			)
		 ))
		 (run-dot (lambda (dot-file ps-file)
			(system (string-append "dot -Tps " dot-file " > " ps-file))
		 ))
		)
		(if breeze2ps-single-file
			(let
				((dot-file (string-append filename ".dot"))
				 (ps-file (string-append filename ".ps"))
				)
				(with-output-to-file dot-file (lambda ()
					(for-each (lambda (proc) (make-part-dot-code proc context)) parts)
				))
				(run-dot dot-file ps-file)
				(prune-file dot-file)
			)
			(for-each (lambda (proc)
				(let*
					((procedure-file (string-append filename "-" (brz-breeze-part:name proc)))
					(dot-file (string-append procedure-file ".dot"))
					(ps-file (string-append procedure-file ".ps"))
					)
					(with-output-to-file dot-file (lambda () (make-part-dot-code proc context)))
					(run-dot dot-file ps-file)
					(prune-file dot-file)
				)
			) parts)
		)
	)
))
