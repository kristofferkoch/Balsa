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
;;;	`balsa-sim-impl.scm'
;;;	Simulate an implementation
;;;
;;;	$Id: balsa-sim-impl.scm,v 1.2 2003/09/15 15:59:22 bardslea Exp $
;;;

(balsa-scheme-import 'sim 'impl-sim)

(top-level (lambda (args)
	(let
		((args-tail (impl-sim-parse-command-line args)))
		(if (not impl-sim-no-banner)
			(impl-sim-print-banner)
		)
		(apply impl-sim args-tail)
	)
) (cdr command-line-args))
