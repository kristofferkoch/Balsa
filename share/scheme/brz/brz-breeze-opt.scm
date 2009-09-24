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
;;;	`brz-breeze-opt.scm'
;;;	Breeze peephole optimiser
;;;
;;;	$Id: brz-breeze-opt.scm,v 1.3 2003/11/04 12:11:44 bardslea Exp $
;;;

(balsa-scheme-import 'brz)
(balsa-scheme-import 'misc 'switches)
(balsa-scheme-import 'misc 'banners)

;;; breeze-opt-{no-banner,...}: command line switches
(define breeze-opt-no-banner #f)
(define breeze-opt-import-path balsa-search-path)

;;; breeze-opt-print-banner: print the breeze-opt banner
(define breeze-opt-print-banner (lambda ()
	(make-program-banner "breeze-opt" "breeze-opt: Breeze Peephole Optimiser"
		"2002, The University of Manchester")
))

;;; breeze-opt-usage: command usage
(define breeze-opt-usage (lambda ()
	(breeze-opt-print-banner)
	(error
		"version " balsa-version #\newline
		"usage: breeze-opt {<switch>}* <block/file-name>" #\newline #\newline
		"switches: -h or -?           - Display this message (--help)" #\newline
		"          -b                 - Don't print the breeze-opt banner (--no-banner)" #\newline
		"          -I <directory>     - Add named directory to the import path (--import)" #\newline
	)
))

;;; breeze-opt-command-line-rules: command-line-args action rules
(define breeze-opt-command-line-rules `(
	(#\b "no-banner" 0 ,(lambda (args) (set! breeze-opt-no-banner #t)))
	(#\I "import" 1 ,(lambda (args) (set! breeze-opt-import-path (append breeze-opt-import-path (list (car args))))))
	(#\h "help" 0 ,(lambda (args) (breeze-opt-usage)))
	(#\? "help" 0 ,(lambda (args) (breeze-opt-usage)))
))

;;; breeze-opt-parse-command-line: parse switches from the given command line list, set
;;;		the breeze-opt-... globals and return the tail of the command line.
(define breeze-opt-parse-command-line (lambda (args)
	(if (null? args)
		(breeze-opt-usage)
		(let ((args-tail (parse-command-line "breeze-opt" breeze-opt-command-line-rules breeze-opt-usage args)))
			(if (/= 1 (length args-tail))
				(error "breeze-opt: expecting exactly one file name" #\newline)
				args-tail
			)
		)
	)
))

;;; breeze-opt: generate a LARD file from the given breeze file.  The output LARD file will have
;;;		commands built up from the activation tree of the input breeze file
(define breeze-opt (lambda (filename)
	(balsa-set-tech #f)
	(brz-load-primitives)
	;;; do stuff FIXME
))
