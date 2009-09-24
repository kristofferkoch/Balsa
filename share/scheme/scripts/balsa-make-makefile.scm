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
;;;	`balsa-make-makefile.scm'
;;;	Balsa "make-depend" Makefile generating script
;;;
;;;	$Id: balsa-make-makefile.scm,v 1.2 2003/08/26 21:16:33 bardslea Exp $
;;;

(balsa-scheme-import 'brz 'balsa-make-makefile)

(top-level (lambda (args)
	(balsa-make-makefile-parse-command-line args)
	(if (not balsa-make-makefile-no-banner)
		(balsa-make-makefile-print-banner)
	)
	(apply balsa-make-makefile balsa-make-makefile-files)
) (cdr command-line-args))
