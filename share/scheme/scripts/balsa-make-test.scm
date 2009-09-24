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
;;;	`balsa-make-test.scm'
;;;	Balsa test harness generator
;;;
;;;	$Id: balsa-make-test.scm,v 1.2 2003/08/23 21:48:46 bardslea Exp $
;;;

(balsa-scheme-import 'sim 'balsa-test)

(top-level (lambda (args)
	(let
		((args-tail (balsa-test-parse-command-line args)))
		(if (not balsa-test-no-banner)
			(balsa-test-print-banner)
		)
		(apply balsa-test args-tail)
	)
) (cdr command-line-args))
