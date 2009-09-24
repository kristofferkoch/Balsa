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
;;;	`balsa-split-lines.scm'
;;;	Line splitting script
;;;
;;;	$Id: balsa-split-lines.scm,v 1.3 2003/02/08 19:39:44 bardslea Exp $
;;;

(balsa-scheme-import 'misc 'switches)
(balsa-scheme-import 'misc 'banners)

(define column-count 79)
(define continuation-string "-")
(define continue-at-end #t)
(define dont-split-start #f)
(define dont-split-len 0)

;;; balsa-split-lines-print-banner: print the balsa-split-lines banner
(define balsa-split-lines-print-banner (lambda ()
	(make-program-banner "balsa-split-lines" "balsa-split-lines: Split file lines into short chunks"
		"1998, The University of Manchester")
))

;;; balsa-split-lines-usage: command usage
(define balsa-split-lines-usage (lambda ()
	(balsa-split-lines-print-banner)
	(error
		"usage: balsa-split-lines {<switch>}* {<file-name>}+" #\newline #\newline
		"switches: -h or -?           - Display this message (--help)" #\newline
		"          -c <column-count>  - Allow at most column count chars. per line" #\newline
		"                               (not including the continuation str.) (--columns)" #\newline
		"          -l <line-cont-str> - Use line-cont-str as a string to indicate line continuation" #\newline
		"                               (--continuation-string)" #\newline
		"          -e                 - Print continuation string at end of line (--continue-at-end)" #\newline
		"          -s                 - Print continuation string at start of line (--continue-at-start)" #\newline
		"          -d <string>        - Don't split lines which start with <string> (--dont-split)" #\newline
	)
))

;;; balsa-split-lines-command-line-rules: command-line-args action rules
(define balsa-split-lines-command-line-rules `(
	(#\h "help" 0 ,(lambda (args) (balsa-split-lines-usage)))
	(#\? "help" 0 ,(lambda (args) (balsa-split-lines-usage)))
	(#\c "column-count" 1 ,(lambda (args) (set! column-count (string->number (car args)))))
	(#\l "continuation-string" 1 ,(lambda (args) (set! continuation-string (car args))))
	(#\e "continue-at-end" 0 ,(lambda (args) (set! continue-at-end #t)))
	(#\s "continue-at-start" 0 ,(lambda (args) (set! continue-at-end #f)))
	(#\d "dont-split" 1 ,(lambda (args) (set! dont-split-start (car args))
		(set! dont-split-len (string-length (car args)))))
))


;;; balsa-split-lines-from-port: read lines from port `port'.  Split 'em and spit 'em out.
(define balsa-split-lines-from-port (lambda (port)
	(let
		((line (read-line port)))
		(cond 
			((eof-object? line) #f)
			((and dont-split-start
				(>= (string-length line) dont-split-len)
				(string=? (substring line 0 dont-split-len) dont-split-start)
			 )
			 (print line #\newline)
			 (balsa-split-lines-from-port port)
			)
			(else
				(line-print (split-line line column-count continuation-string continue-at-end))
				(balsa-split-lines-from-port port)
			)
		)
	)
))

(top-level (lambda (args)
	(let
	 	((files (parse-command-line "balsa-split-lines" balsa-split-lines-command-line-rules
			balsa-split-lines-usage args)))
		(if (null? files)
			(balsa-split-lines-usage)
			(for-each (lambda (file)
				(balsa-split-lines-from-port (open-input-file file))
			) files)
		)
	)
) (cdr command-line-args))
