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
;;;	`breeze2c.scm'
;;;	Breeze -> C simulation model generator script
;;;
;;;	$Id: breeze2c.scm,v 1.3 2003/02/08 19:39:44 bardslea Exp $
;;;

(balsa-scheme-import 'brz 'breeze2c)

(top-level (lambda (args)
	(let
		((args-tail (breeze2c-parse-command-line args)))
		(if (not breeze2c-no-banner)
			(breeze2c-print-banner)
		)
		(apply breeze2c args-tail)
	)
) (cdr command-line-args))
