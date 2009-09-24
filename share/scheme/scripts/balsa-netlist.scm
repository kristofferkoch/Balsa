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
;;;	`balsa-netlist.scm'
;;;	Breeze -> Netlist generator, all manner of netlist functions
;;;
;;;	$Id: balsa-netlist.scm,v 1.7 2003/02/08 19:39:44 bardslea Exp $
;;;

(balsa-scheme-import 'gen 'balsa-netlist)

(top-level (lambda (args)
	(let
		((args-tail (balsa-netlist-parse-command-line args)))
		(if (not balsa-netlist-no-banner)
			(balsa-netlist-print-banner)
		)
		(set! net-session-file-list '())
		(cond
			((null? args-tail) (balsa-netlist-usage))
			(balsa-netlist-test-comp (balsa-netlist-test args-tail))
			((final-string (car args-tail) ".net") (apply balsa-netlist-convert args-tail))
			(else (apply balsa-netlist-generate args-tail))
		)
	)
) (cdr command-line-args))
