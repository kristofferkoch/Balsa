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
;;;	$Id: balsa-make-helpers.scm,v 1.6 2003/02/08 19:39:44 bardslea Exp $
;;;

(balsa-scheme-import 'gen 'make-helpers)

(top-level (lambda (args)
	(let
		((args-tail (balsa-make-helpers-parse-command-line args)))
		(if (not balsa-helper-no-banner)
			(balsa-helper-print-banner)
		)
		(cond
			((null? args-tail) (balsa-helper-usage))
			(balsa-make-cells (gen-helper-test args-tail))
			(else (gen-new-helper-cells args))
		)
	)
) (cdr command-line-args))

