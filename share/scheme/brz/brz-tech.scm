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
;;; `brz-tech.scm'
;;; Setup technology options
;;;
;;;	$Id: brz-tech.scm,v 1.34 2004/03/08 15:45:45 guptan Exp $
;;;

(balsa-scheme-import 'net 'parser)

;;; Procedures to return the size of the portion according to the various protocols
;;; Return 0 if single wire, n if full width, and #f if not used.

;;; width-one: sync portions
(define width-one (lambda (width) 0))

;;; width-bundled/dualrail: full width bundled data and dual rail
(define width-bundled/dualrail id)

;;; width-dualrail/1of4: req0, req1 etc, dual rail/one of four  
(define width-dualrail/1of4 (lambda (width) (quotient (+ width 1) 2)))

;;; width-1of4: req2, req3 one of four
(define width-1of4 (lambda (width) 
	(let 
		((slice-width (quotient width 2)))
		(if (zero? slice-width) 
			#f
			slice-width
		)
	)
))
	
(define brz-get-technology (lambda ()
	(if (not tech)
		(begin
			(let
				((tech-description-file (string-append breeze-tech-dir breeze-tech)))
				(if (file-exists? tech-description-file)
					(load tech-description-file)
					(error "brz-get-technology: invalid technology `" breeze-tech "'")
				)
			)
			(cond ;; FIXME, improve this
				;; NB. `push' here means the forward control direction, `pull' is the opposite direction
				((string=? breeze-style "four_e_e")
					(set! tech-sync-channel-portions 
						`((req push ,width-one) (ack pull ,width-one)))
					(set! tech-push-channel-portions 
						`((req push ,width-one) (ack pull ,width-one) (data push ,width-bundled/dualrail)))
					(set! tech-pull-channel-portions 
						`((req push ,width-one) (ack pull ,width-one) (data pull ,width-bundled/dualrail)))
				)
				((string=? breeze-style "four_b_rb")
					(set! tech-sync-channel-portions 
						`((req push ,width-one) (ack pull ,width-one)))
					(set! tech-push-channel-portions 
						`((req push ,width-one) (ack pull ,width-one) (data push ,width-bundled/dualrail)))
					(set! tech-pull-channel-portions 
						`((req push ,width-one) (ack pull ,width-one) (data pull ,width-bundled/dualrail)))
				)
				((string=? breeze-style "dual_b")
					(set! tech-sync-channel-portions 
						`((req push ,width-one) (ack pull ,width-one)))
					(set! tech-push-channel-portions 
						`((req0 push ,width-bundled/dualrail)
							(req1 push ,width-bundled/dualrail) (ack pull ,width-one)))
					(set! tech-pull-channel-portions 
						`((req push ,width-one) (ack0 pull ,width-bundled/dualrail)
							(ack1 pull ,width-bundled/dualrail)))
				)
				((string=? breeze-style "one_of_2_4")
					(set! tech-sync-channel-portions 
						`((req push ,width-one) (ack pull ,width-one)))
					(set! tech-push-channel-portions 
						`((req0 push ,width-dualrail/1of4) (req1 push ,width-dualrail/1of4) 
							(req2 push ,width-1of4) (req3 push ,width-1of4) (ack pull ,width-one)))
					(set! tech-pull-channel-portions 
						`((req push ,width-one) (ack0 pull ,width-dualrail/1of4)
							(ack1 pull ,width-dualrail/1of4) (ack2 pull ,width-1of4) (ack3 pull ,width-1of4)))
				)
			)
			; always include node in portions.
			(set! tech-node-portions `((node push ,width-bundled/dualrail)))
			(set! tech-net-portions `((net push ,width-one)))
			(set! all-portions 
				(append tech-sync-channel-portions tech-push-channel-portions 
					tech-pull-channel-portions tech-node-portions tech-net-portions))
			(set! tech #t)
		)
	)
))
