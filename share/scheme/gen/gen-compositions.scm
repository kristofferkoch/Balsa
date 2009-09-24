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
;;;	`gen-compositions.scm'
;;;	`gen' stage of balsa-netlist back end - `breeze-composition' expansion
;;;
;;;	$Id: gen-compositions.scm,v 1.1 2003/11/04 12:11:47 bardslea Exp $
;;;

;;; gen-make-composition-netlist: make a netlist for a breeze-composition type
;;;		construction.  A breeze-composition is a component which encapsulates the composition
;;;		of *wire* ported netlist instances into a *channel* ported part
(define gen-make-composition-netlist (lambda (composition context)
	(let*
		((name "Hello")
		 (ports '(ports))
		 (nets '(nets))
		 (instances '(instances))
		)
		(list 'circuit name
			ports
			nets
			instances
		)
	)
))
