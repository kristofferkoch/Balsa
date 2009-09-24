(balsa-init)
(balsa-scheme-import 'brz 'parser)

(define tech-common-home "../")

(define part-names (get-file (string-append tech-common-home "/components.lst")))

(define add-style-to-primitive-part (lambda (part style-name style-directory)
	(let
		((implementation (find-headed-sub-list part 'implementation))
		 (new-implementation `(style ,style-name (include tech "common"
			,(string-append style-directory "/" (brz-primitive-part:name part)))))
		)
		(if implementation
			(append! implementation (list new-implementation))
			(append! part (list 'implementation new-implementation))
		)
		part
	)
))

(define write-part-to-file (lambda (part filename)
	(with-output-to-file filename (lambda ()
		(simple-print part "" '(primitive-part parameters ports symbol implementation))
		(newline)
	))
))

(define add-style-to-parts (lambda (style-name style-directory)
	(for-each (lambda (part-name)
		(let
			((part (car (get-file (string-append part-name ".abs")))))
			(write-part-to-file part (string-append part-name ".bak"))
			(with-output-to-file (string-append style-directory "/" part-name ".abs")
				newline)
			(write-part-to-file
				(add-style-to-primitive-part part style-name style-directory)
				(string-append part-name ".abs")
			)
		)
	) part-names)
))
