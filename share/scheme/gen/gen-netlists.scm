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
;;;	`gen-netlists.scm'
;;;	`gen' stage of balsa-netlist back end - `breeze-netlist' expansion
;;;
;;;	$Id: gen-netlists.scm,v 1.1 2003/11/04 12:11:47 bardslea Exp $
;;;

;;; gen-make-netlist-netlist: make a netlist for a breeze-netlist Breeze element.
;;;		breeze-netlists are wrappers for .net netlists or automatically generated
;;;		.net netlists generated by a method described in the breeze-netlist itself.
;;;		For the moment, this interface synthesises each netlist separately rather
;;;		than trying to make a single pass with the tool described in the breeze-netlist
(define gen-make-netlist-netlist (lambda (netlist context)
	(let
		((mangled-name (tech-map-cell-name (tech-mangle-breeze-part-name (brz-breeze-netlist:name netlist))))
		)
		(list 'circuit mangled-name
			'(ports) '(nets) '(instances)
		)
	)
))