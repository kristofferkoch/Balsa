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
;;;	`net-merge-netlists.scm'
;;;	Script to merge .net netlists
;;;
;;;	$Id: net-merge-netlists.scm,v 1.4 2003/02/08 19:39:44 bardslea Exp $
;;;

(balsa-scheme-import 'brz)
(balsa-scheme-import 'misc 'switches)
(balsa-scheme-import 'misc 'banners)

(balsa-scheme-import 'net 'verilog)

;;; net-merge-netlists-{no-banner,...}: command line switches
(define net-merge-netlists-no-banner #f)
(define net-merge-netlists-output-filename #f)

;;; net-merge-netlists-print-banner: print the balsa-net-merge-netlists banner
(define net-merge-netlists-print-banner (lambda ()
	(make-program-banner "balsa-net-merge-netlists" "Balsa netlist merging script"
		"2002, The University of Manchester")
))

;;; net-merge-netlists-usage: command usage
(define net-merge-netlists-usage (lambda ()
	(net-merge-netlists-print-banner)
	(error
		"version " balsa-version #\newline
		"usage: balsa-net-merge-netlists {<switch>}* <netlist-file-name> ..." #\newline #\newline
		"switches: -h or -?           - Display this message (--help)" #\newline
		"          -b                 - Don't print the balsa-net-merge-netlists banner (--no-banner)" #\newline
		"          -o <file-name>     - Output netlist filename *REQUIRED* (--output-file)" #\newline
	)
))

;;; net-merge-netlists-command-line-rules: command-line-args action rules
(define net-merge-netlists-command-line-rules `(
	(#\o "output-file" 1 ,(lambda (args) (set! net-merge-netlists-output-filename (car args))))
	(#\b "no-banner" 0 ,(lambda (args) (set! net-merge-netlists-no-banner #t)))
	(#\h "help" 0 ,(lambda (args) (net-merge-netlists-usage)))
	(#\? "help" 0 ,(lambda (args) (net-merge-netlists-usage)))
))

;;; balsa-net-merge-netlists-parse-command-line: parse switches from the given command line list, set
;;;		the net-merge-netlists-... globals and return the tail of the command line.
(define balsa-net-merge-netlists-parse-command-line (lambda (args)
	(if (null? args)
		(net-merge-netlists-usage)
		(parse-command-line "balsa-net-merge-netlists" net-merge-netlists-command-line-rules net-merge-netlists-usage args)
	)
))

;;; balsa-net-merge-netlists: merge the named netlist files into a single file (whose name should
;;;		have been passed with the -o switch)
(define balsa-net-merge-netlists (lambda (filenames)
	(if (not net-merge-netlists-output-filename)
		(error "balsa-net-merge-netlists: -o option must be used to name output file")
	)
	(let*
		((read-hash (make-hash-table 57))
		 (full-netlist (reverse! (foldl (lambda (netlist filename)
			(foldl (lambda (netlist circuit)
				(if (headed-list? circuit 'circuit)
					(let ((circuit-name (net-circuit-decl:name circuit)))
						; Already visited
						(if (hash-ref read-hash circuit-name)
							netlist
							(begin
								(hash-set! read-hash circuit-name #t)
								(cons circuit netlist)
							)
						)
					)
					netlist
				)
			) netlist (get-file filename))
		 ) '() filenames)))
		)
		(net-write-netlist-file full-netlist full-netlist "" net-merge-netlists-output-filename)
	)
))
