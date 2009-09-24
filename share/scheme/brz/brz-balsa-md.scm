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
;;;	`brz-balsa-md.scm'
;;;	Balsa Makefile generator
;;;
;;;	$Id: brz-balsa-md.scm,v 1.81 2003/08/10 00:58:43 bardslea Exp $
;;;

(balsa-scheme-import 'brz)
(balsa-scheme-import 'misc 'switches)
(balsa-scheme-import 'misc 'date)
(balsa-scheme-import 'misc 'banners)

;;; brz-rule:{target,dependencies}: field extractors
(define brz-rule:target car)
(define brz-rule:dependencies cdr)

;;; rule<=?: ordering predicate on rules
(define rule<=? (lambda (l r)
	(string<=? (brz-rule:target l) (brz-rule:target r))
))

;;; brz-lard-lcd-libs: make a list of "-llibname -llibname2" type
;;;		lcd command line args from the given list of dotted-paths
(define brz-lard-lcd-libs (lambda (blocks)
	(foldl (lambda (libs block)
		(if (member block brz-empty-lard-balsa-blocks)
			libs
			(string-append libs "-l" (brz-dotted-to-slashed-name (brz-trim-square-brackets block)) " ")
		)
	) "" blocks)
))

;;; brz-make-balsa-import-path: make a list of `-I' for balsa-c from a given path list
(define brz-make-balsa-import-path (lambda (path)
	(build-string path "-I " " ")
))

;;; brz-make-lard-import-path: make a list of `-I' for lard from a given path list
(define brz-make-lard-import-path (lambda (path)
	(build-string path "-L" " ")
))


;;; brz-list-imported-blocks: returns a list of blocks imported by the Balsa block
;;;		`block'.  NB. This procedure uses Balsa not Breeze blocks and uses balsa-c
;;;		to find the import list from the Balsa format input files.
(define brz-list-imported-blocks (lambda (block)
	(let*
		((temp-file "./balsa-md-deps")
		 (rv (system (string-append "balsa-c " (brz-make-balsa-import-path balsa-md-import-path)
			" -M '" block "' > " temp-file)))
		 (ret (if (zero? rv)
			(car (get-file temp-file))
			(error "brz-list-imported-blocks: can't expand dependencies for block `" block "'")
		 ))
		)
		(system (string-append "rm " temp-file))
		(map (lambda (name) (string-append "[" name "]")) ret)
		;;; ret
	)
))

;;; brz-make-rule-set: add to the list of rules `rules' rules which describe the dependencies
;;;		inherant in the blocks named in the list `to-visit'.  `visited' contains a list of
;;;		already visited blocks which should not be re-visited duirn rules construction.  If
;;;		`dotted-path?' is true then the first (typically the only) element of `to-visit' is
;;;		treated as a dotted path rather than a file path or a square bracketted dotted path.
;;;		Typical use example: (brz-make-rule-set '("mytopfile") brz-library-balsa-blocks '() #f)
;;;		NB. `to-visit' should be ordered
(define brz-make-rule-set (lambda (to-visit visited rules dotted-path?)
	(if (null? to-visit)
		(reverse! rules)
		(let*
			((imports (brz-list-imported-blocks (car to-visit))))
			(brz-make-rule-set ; Add to to-visit, visited, rules
				(union string<=? (cdr to-visit) (differ string<=? (merge-sort string<=? imports) visited))
				(union string<=? (list (car to-visit)) visited)
				(cons (cons (car to-visit) imports) rules)
				#t
			)
		)
	)
))

;;; brz-make-top-rule-set: make a rule set for the top level files `top-files'
(define brz-make-top-rule-set (lambda top-files
	(brz-make-rule-set (merge-sort string<=? top-files) brz-library-balsa-blocks '() #f)
))

;;; brz-flatten-dependencies: make a new list of dependencies which is a flattened version of the
;;;		input dep. list. A flattened dependency list has each target for which a rule exists in
;;;		that rule set expanded to its terminal (non-rule) targets.
;;;		`rules' should contain a rule set for the flattening and `dependencies' should be a
;;;		dependency list for this rule.
;;;		NB. for a single rule r this procedure should be called:
;;;			(brz-flatten-dependencies rules (brz-rule:dependencies rule) '())
(define brz-flatten-dependencies (lambda (rules dependencies visited-deps)
	(if (null? dependencies)
		(unsorted-uniq string=? visited-deps)
		(let
			((sub-rule (assoc (car dependencies) rules))
			 (next (lambda (new-dependencies) (brz-flatten-dependencies rules new-dependencies
				(cons (car dependencies) visited-deps)
			 )))
			)
			(if sub-rule
				(next (unsorted-uniq string=? (append (cdr dependencies) (cdr sub-rule))))
				(next (cdr dependencies))
			)
		)
	)
))

;;; brz-flatten-rule-set: flatten an entire rules set list
(define brz-flatten-rule-set (lambda (rules)
	(map (lambda (rule)
		(cons (brz-rule:target rule)
			(brz-flatten-dependencies rules (brz-rule:dependencies rule) '())
		)
	) rules)
))

;;; brz-block-name->path: find a path/filename to a given block type and return a path which
;;;		has $(BALSAHOME) and $(LARDHOME) substituted for initial path components which
;;;		match their current values and an extension as per `target-ext'.  Blocks in the
;;;		current directory have no prefixing path.
;;;		`source-ext' is the extension for the block type to be found.
(define brz-block-name->path (lambda (dotted-path source-ext target-ext)
	(let*
		((balsa-home balsa-home) ; allow options?
		 (path/name/ext (find-filename dotted-path source-ext #t balsa-md-import-path))
		 (path (car path/name/ext))
		 (name (cadr path/name/ext))
; Reintroduce if BALSAHOME/LARDHOME prefices are common
;		 (balsa-init (initial-string path balsa-home))
		)
		(string-append (cond
;			(balsa-init (string-append "$(BALSAHOME)" path))
			((string=? path "./") "")
			(else path)
		) name (if (string=? target-ext "") "." ".") target-ext)
	)
))

;;; print-make-header: print the fancy header ont the make file
(define print-make-header (lambda ()
	(let
		((unix-name (let ((u-name (uname))) (if (vector? u-name) u-name
			#("unknown" "unknown" "unknown" "1" "unknown")))))
		(print "### Balsa Makefile" #\newline)
		(print "### Created: " (ctime) #\newline)
		(print "### By: " (vector-ref (getpw (getuid)) 0)
			"@" (vector-ref unix-name 1) " (" (vector-ref unix-name 0) ")" #\newline)
		(print "### With balsa-md version: " balsa-version #\newline)
		(print "### Command: balsa-md " balsa-md-command-line-string #\newline #\newline)
	)
))

;;; print-variable-definition: print a single variable definition in the form `NAME = value' #\newline
;;;		unless the variable name is pre-defined/undefined.
;;;		NB. If the name of the variable is 'comment then just print the value as a comment; this is usefull
;;;		for inserting the ### Programs / ### Options separators in the makefile with the default rules.
;;;		If the var. name is 'newline then just print a newline.
(define print-variable-definition (lambda (name value)
	(cond
		((eq? 'comment name) (print "### " value #\newline))
		((eq? 'newline name) (newline))
		((member name balsa-md-undefined-variables)
			(print "# -- " name " left undefined" #\newline)
		)
		((assoc name balsa-md-defined-variables)
			(print "# -- " name " defined by user" #\newline)
		)
		((string=? "BALSAIMPORTPATH" name) ; handle exception for BALSAIMPORTPATH
			; cut the first (length balsa-search-path) elements from balsa-md-import-path
			(simple-print-variable-definition name (brz-make-balsa-import-path
				(list-tail balsa-md-import-path (length balsa-search-path))))
		)
		((string=? "LARDLIBPATH" name) ; same for LARDLIBPATH
			; cut the first (length balsa-search-path) elements from balsa-md-import-path
			(simple-print-variable-definition name (brz-make-lard-import-path
				(list-tail balsa-md-import-path (length balsa-search-path))))
		)
		(else (simple-print-variable-definition name value))
	)
))

;;; simple-print-variable-definition: as above but don't respect -U/-D or 'comment/'newline
(define simple-print-variable-definition (lambda (name value)
	(print name " = ")
	; Add the debugging flag to the necessary utils.
	(if (and balsa-md-debug (member name '("B2LOPTS" "LCDOPTS"))) (print "-g "))
	(print (build-separated-string (map bourne-escape-command-line-arg (parse-string value " ")) " ") #\newline)
))

;;; default-variable-definitions: variable definitions which need not be computed but may
;;;		be overridded at the command line to balsa-md.
;;;		This is a (name value) 2 elem. list suitable for passing to print-variable-definition
;;;		and for assoc'ing
(define default-variable-definitions '(
	(comment "Programs")
	("BALSAC" "balsa-c")
	("BREEZE2LARD" "breeze2lard -b")
	("BALSANETLIST" "balsa-netlist")
	("BREEZELINK" "breeze-link")
	("BREEZESIM" "breeze-sim")
	("BREEZESIMCTRL" "breeze-sim-ctrl")
	("GTKWAVE" "gtkwave")
	("LCD" "balsa-lcd")
	("LI" "balsa-li")
	("MAKE" "make")
	("RM" "rm -f")
	(newline "")
	(comment "Options")
	("BALSAIMPORTPATH" "")
	("BALSACOPTS_COMPLETE" "-b $(BALSAIMPORTPATH) $(BALSACOPTS)")
	("BALSANETLISTOPTS" "$(BALSAIMPORTPATH)")
	("B2LOPTS" "$(BALSAIMPORTPATH)")
;	("BREEZESIMOPTS" "")
	("BREEZELINKOPTS" "")
	("LCDOPTS" "")
	("LARDLIBPATH" "")
	("LARDRUNCMD" "balsa run")
	("LARDWINCMD" "balsawin")
	(newline "")
	(comment "Implementation technology")
))

;;; test-fixture-variables: variables which are specific to a test harness and so have a 
;;;		`_testHarnessName' suffix when placed.
(define test-fixture-variables '(
	"TESTOPTS" "SIMOPTS" "SIMARGS"
))
(define lard-test-fixture-variables '(
	"TESTOPTS" "SIMOPTS" "SIMARGS"
))

;;; implementation-variables: variables generated for each implementation with
;;;		similar name structures to test-fixture-variables
(define implementation-variables '(
	"IMPLPRE" "IMPLOPTS" "IMPLTECH"
))

;;; print-feature-variables: print out variables specific to a list of Makefile
;;;		features (ie. test fixtures or implementations) using the list of variable
;;;		prefices `feature-variables' and the given comment prefix.
;;;		NB. The name of feature must be in (car `feature')
(define print-feature-variables (lambda (features feature-variables comment-prefix)
	(for-each (lambda (feature)
		(newline)
		(print "### " comment-prefix " " (car feature) " variables" #\newline)
		(for-each (lambda (variable)
			(print-variable-definition (string-append variable "_" (car feature)) "")
		) feature-variables)
	) features)
))

;;; print-make-definitions: print the variable/suffix rule definitions block
(define print-make-definitions (lambda ()
	(for-each (lambda (var-defn) (apply print-variable-definition var-defn)) default-variable-definitions)
	; Print the TESTOPTS_<name> variables too
	(print-feature-variables balsa-md-test-fixtures
		test-fixture-variables "Test fixture (breeze2lard generated LARD)")
	(print-feature-variables balsa-md-lard-test-fixtures
		lard-test-fixture-variables "Test fixture (hand written LARD)")
	; Print the IMPLOPTS_<name> variable
	(print-feature-variables balsa-md-implementations implementation-variables "Implementation")
	; Print user (re)defined variables
	(if (not (null? balsa-md-defined-variables))
		(begin
			(newline)
			(print "### User defined variables" #\newline)
			(for-each (lambda (var-defn) (apply simple-print-variable-definition var-defn))
				(reverse balsa-md-defined-variables))
		)
	)
	(newline)
	(print "### Suffix rules" #\newline)
	(print ".SUFFIXES: .balsa .breeze .ps .l" #\newline)
	(newline)
	(print ".breeze.ps:" #\newline)
	(print #\ht "breeze2ps $*" #\newline)
	(print ".balsa.breeze:" #\newline)
	(print #\ht "$(BALSAC) $(BALSACOPTS_COMPLETE) $*" #\newline)
	(print ".breeze.l:" #\newline)
	(print #\ht "$(BREEZE2LARD) $(B2LOPTS) $*" #\newline)

	(balsa-md-add-to-target-help "suffix rule .ps" "Generate a PostScript graph of a Breeze file")
	(balsa-md-add-to-target-help "suffix rule .breeze" "Compile Balsa to Breeze")
	(balsa-md-add-to-target-help "suffix rule .l" "Generate LARD model of a Breeze file")
))

;;; print-balsa-rules: print `target: dependencies' rules for only those
;;;		rules in the given `rules' which have dependencies.  This procedure makes
;;;		breeze targets from breeze dependencies and produces no actions as these are
;;;		handled by the balsa->breeze suffix rule.
(define print-balsa-rules (lambda (rules)
	(let
		((block-names (lambda (blocks ext)
		 	(map (lambda (n) (brz-block-name->path n "balsa" ext)) blocks)
		)))
		(for-each (lambda (rule)
			(if (not (null? (brz-rule:dependencies rule)))
				(print (brz-block-name->path (brz-rule:target rule) "balsa" "breeze") ": "
					(build-separated-string (block-names (brz-rule:dependencies rule) "breeze") " ")
					#\newline
				)
			)
		) rules)
	)
))

;;; print-breezesim-rules: print breezesim `target: dependencies' rules.
(define print-breezesim-rules (lambda (rules)
	(let
		((block-names (lambda (blocks ext)
		 	(map (lambda (n) (brz-block-name->path n "balsa" ext)) blocks)
		)))
		(for-each (lambda (rule)
;			(if (not (null? (brz-rule:dependencies rule)))
				(print (brz-block-name->path (brz-rule:target rule) "balsa" "breezesim") ": "
					(build-separated-string (block-names (brz-rule:dependencies rule) "breeze") " ")
					#\newline 
					#\ht "breeze-link "
					(build-separated-string (block-names (append (brz-rule:dependencies rule) (list (brz-rule:target rule))) "") " ")
					#\newline #\newline
				)
;			)
		) rules)
	)
))

;;; print-lard-rules: print lard `target: dependencies' lines and the following lcd command actions.
;;;		uses flat rules to produce flattened library lists
(define print-lard-rules (lambda (rules flat-rules)
	(let
		((block-names (lambda (blocks ext)
		 	(map (lambda (n) (brz-block-name->path n "balsa" ext)) blocks)
		)))
		(for-each (lambda (rule flat-rule)
			(let*
				((target (brz-rule:target rule))
				 (deps (brz-rule:dependencies rule))
				 (flat-deps (brz-rule:dependencies flat-rule))
				 (dep-bcodes
					(foldl (lambda (bcodes block)
						(if (member block brz-library-balsa-blocks)
							bcodes
							(string-append bcodes (brz-block-name->path block "balsa" "") "bcode ")
						)
					) "" deps)
				 )
				 (trimmed-target-name (brz-trim-square-brackets target))
				 (lard-libs (brz-lard-lcd-libs flat-deps))
				 (target-path/name (brz-block-name->path target "balsa" ""))
				)
				(print target-path/name "bcode: ")
				(if (or (string=? balsa-md-simulation-system "interpreter")
					(string=? balsa-md-simulation-system "lard2c"))
					    (print (string-append target-path/name "l ") dep-bcodes #\newline)
					    (print (string-append target-path/name "breeze ") dep-bcodes #\newline)
				)
				(print #\ht "$(LCD) $(LARDLIBPATH) $(LCDOPTS) -mklib " lard-libs
					(string-append target-path/name "l") #\newline)
; Removed by LJ 10/09/2002
;				(if (strchr trimmed-target-name #\.)
;					(for-each (lambda (ext)
;						(print #\ht "mv " (string-append target-path/name ext)
;							" " trimmed-target-name "." ext #\newline)
;					) '("bcode" "dcode"))
;				)
				(newline)
			)
		) rules flat-rules)
	)
))

;;; print-test-fixture: print out the make(1) rules for a single, predefined-lard?generated
;;;		test fixture if predefined-lard? is #t or for a test fixture with a predefined LARD file
;;;		(as (caddr fixture)) if predefined-lard? is #f
(define print-test-fixture (lambda (fixture flat-rules predefined-lard?)
	(let*
		((name (car fixture))
		 (block-name (cadr fixture))
		 (third-name (caddr fixture)) ; proc-name or file-name
		 (breeze-block-name (brz-block-name->path block-name "balsa" "breeze")) ; expanded block name
		 (bcode-block-name (string-append (brz-trim-square-brackets block-name) ".bcode"))
		 (breezelardinterface-block-name (string-append (brz-trim-square-brackets block-name) "_breezelardinterface"))
;		 (breezesim-block-name (string-append (brz-trim-square-brackets block-name) ".breezesim"))
		 (block-rule (find-with-predicate flat-rules (lambda (r) (string=? (brz-rule:target r) block-name))))
		 (test-fixture-file-name (if predefined-lard? (final-string third-name ".l") (string-append "test-" name)))
		 (test-fixture-real-file-name (string-append "test-" name))
		 (testdesc-block-name (string-append "test-" name "_TEMPORARY.testdesc"))
		)	
		(if (not block-rule)
			(error "balsa-md: can't find a block with name `" block-name "'. Are you using filenames/dotted-paths consistently?")
		)
		(if (or (string=? balsa-md-simulation-system "interpreter") (string=? balsa-md-simulation-system "lard2c"))
		    (let()
		      (if (not predefined-lard?)
			      (begin
				      (print test-fixture-file-name ".l: " #\newline)
				      (print #\ht "$(MAKE) " breeze-block-name #\newline)
				      (print #\ht "$(BREEZE2LARD) $(B2LOPTS) $(TESTOPTS_" name ") -t " third-name " -o "
					      test-fixture-file-name " '" block-name "'" #\newline
				      )
				      (newline)
			      )
		      )
		      (print test-fixture-file-name ".bcode: " test-fixture-file-name ".l "
			     bcode-block-name #\newline)
		      ; Print the test harness lcd rule
		      (print #\ht "$(LCD) $(LARDLIBPATH) $(LCDOPTS) " 
			     (if predefined-lard? (string-append "$(TESTOPTS_" name ") ") "")
			     (foldl (lambda (libs block)
				      (if (member block brz-empty-lard-balsa-blocks)
					  libs
					  (string-append libs "-l" (brz-dotted-to-slashed-name (brz-trim-square-brackets block)) " ")
				      )
			     ) "" (append (brz-rule:dependencies block-rule)
					  (list (brz-rule:target block-rule))))
			     test-fixture-file-name ".l" #\newline
		      )
		      (newline)
		      (print "sim-pre-" name ": " test-fixture-file-name ".bcode" #\newline)
		      (newline)
			  (balsa-md-add-to-target-help (string-append "sim-pre-" name) (string-append
				"Prepare the way for simulation of block " block-name " using harness " test-fixture-file-name))

		      (if (string=? balsa-md-simulation-system "interpreter")
			      (print "sim-" name ": sim-pre-" name #\newline #\ht "$(LI) "
				      test-fixture-file-name ".bcode $(SIMOPTS_" name ") $(LARDRUNCMD) --end $(SIMARGS_" name ")" #\newline)
			      (print "sim-" name ": sim-pre-" name #\newline #\ht 
				"./" test-fixture-file-name ".bcode" #\newline)
		      )
			  (balsa-md-add-to-target-help (string-append "sim-" name) (string-append
				"Simulate using test harness " test-fixture-file-name))

		      (newline)
		      (if (string=? balsa-md-simulation-system "interpreter")
			      (print "sim-win-" name ": sim-pre-" name #\newline #\ht "$(LI) "
				      test-fixture-file-name ".bcode $(SIMOPTS_" name ") $(LARDWINCMD) --end $(SIMARGS_" name ")" #\newline)
			      (print "sim-win-" name ": sim-pre-" name #\newline #\ht "$(LI) "
				      "./" test-fixture-file-name ".bcode" #\newline)
		      )
			  (balsa-md-add-to-target-help (string-append "sim-win-" name) (string-append
				"Graphically simulate using test harness " test-fixture-file-name))

		      (newline)
		    )
                    ; breezesim test harness rules:
		    (let()
		      (print test-fixture-real-file-name "_TEMPORARY.testdesc: Makefile" #\newline)
;		      (print #\ht "$(MAKE) " breeze-block-name #\newline)
		      (print #\ht "echo $(TESTOPTS_" name ") > " test-fixture-real-file-name "_TEMPORARY.testdesc" #\newline)
;		      (print #\ht "$(BREEZE2LARD) $(B2LOPTS) $(TESTOPTS_" name ") -t " third-name " -o "
;			     test-fixture-file-name " '" block-name "'" #\newline
;			     )
		      (newline)

		      (print test-fixture-real-file-name "_TEMPORARY.breeze" (if predefined-lard? (string-append " " breezelardinterface-block-name ".l") "") ": " breeze-block-name #\newline)
		      (print #\ht "$(BREEZELINK) $(BREEZELINKOPTS) $(LARDLIBPATH) $(LCDOPTS) " 
			     (if predefined-lard? (string-append "$(TESTOPTS_" name ") -i" breezelardinterface-block-name ".l ") "")
;			     (foldl (lambda (libs block)
;				      (if (member block brz-empty-lard-balsa-blocks)
;					  libs
;					  (string-append libs "-l" (brz-dotted-to-slashed-name (brz-trim-square-brackets block)) " ")
;				      )
;			     ) "" (append (brz-rule:dependencies block-rule)
;					  (list (brz-rule:target block-rule))))
			     "-l" (brz-trim-square-brackets block-name) " "
			     (if predefined-lard? "" (string-append "-p" third-name " ")) "-o" test-fixture-real-file-name "_TEMPORARY.breeze" #\newline
		      )
		      (newline)

		      (if predefined-lard?
			  (let()
			    (print breezelardinterface-block-name ".bcode: " breezelardinterface-block-name ".l" #\newline)
			    (print #\ht "$(LCD) $(LARDLIBPATH) $(LCDOPTS) -mklib " 
			     breezelardinterface-block-name ".l" #\newline)
			    (newline)

			    (print test-fixture-file-name ".bcode: " test-fixture-file-name ".l "
				   breezelardinterface-block-name ".bcode" #\newline)
		      ; Print the test harness lcd rule
			    (print #\ht "$(LCD) $(LARDLIBPATH) $(LCDOPTS) " 
			     (string-append "$(TESTOPTS_" name ") ")
;			     (foldl (lambda (libs block)
;				      (if (member block brz-empty-lard-balsa-blocks)
;					  libs
;					  (string-append libs "-l" (brz-dotted-to-slashed-name (brz-trim-square-brackets block)) " ")
;				      )
;			     ) "" (append (brz-rule:dependencies block-rule)
;					  (list (brz-rule:target block-rule))))
			     "-l" breezelardinterface-block-name " "
			     test-fixture-file-name ".l" #\newline
			     )
			    (newline)
			    )
			  )

		      (print "sim-pre-" name ": " test-fixture-real-file-name "_TEMPORARY.breeze " test-fixture-real-file-name "_TEMPORARY.testdesc")
		      (if predefined-lard?
			  (print " " test-fixture-file-name ".bcode")
		      )
		      (newline)(newline)

		      (print "sim-" name ": sim-pre-" name #\newline)
		      (if predefined-lard?
			  (print #\ht "$(LI) " test-fixture-file-name ".bcode run" #\newline)
			  (print #\ht "$(BREEZESIM) " test-fixture-real-file-name "_TEMPORARY" #\newline)
;			  (print #\ht "$(BREEZESIM) $(BREEZESIMOPTS) " test-fixture-real-file-name "_TEMPORARY" #\newline)
		      )
		      (newline)

		      (print "sim-win-" name ": sim-pre-" name #\newline)
		      (if predefined-lard?
			  (print #\ht "$(LI) " test-fixture-file-name ".bcode windows" #\newline)
			  (print #\ht "$(BREEZESIMCTRL) " test-fixture-real-file-name "_TEMPORARY" #\newline)
;			  (print #\ht "$(BREEZESIM) $(BREEZESIMOPTS) --tracefile=" test-fixture-real-file-name "_TEMPORARY.hhh --gtkwave " test-fixture-real-file-name "_TEMPORARY" #\newline)
;			  (print #\ht "$(BREEZESIM) $(BREEZESIMOPTS) --tracefile=" test-fixture-real-file-name "_TEMPORARY.hhh " test-fixture-real-file-name "_TEMPORARY" #\newline #\ht "$(GTKWAVE) " test-fixture-real-file-name "_TEMPORARY.hhh" #\newline)
		      )
		      (newline)
		    )
		)
	)
))

;;; print-test-fixtures: print out test-fixture and test-fixtures rules
(define print-test-fixtures (lambda (top-blocks top-breeze-files top-bcode-files flat-rules)
	(for-each (lambda (fixture) ; '(name block proc-name)
		(print-test-fixture fixture flat-rules #f)
	) balsa-md-test-fixtures)
	(for-each (lambda (fixture) ; '(name block file-name)
		(print-test-fixture fixture flat-rules #t)
	) balsa-md-lard-test-fixtures)
))

;;; balsa-md-print-help-rule: pretty print usage help
(define balsa-md-print-help-rule (lambda ()
	(let
		((max-rule-name-length (foldl (lambda (len rule)
			(let ((rule-name-length (string-length (car rule))))
				(max rule-name-length len)
			)
		 ) 0 balsa-md-target-help))
		 (max-explanation-length (foldl (lambda (len rule)
			(let ((rule-explanation-length (string-length (cdr rule))))
				(max rule-explanation-length len)
			)
		 ) 0 balsa-md-target-help))
		 (sorted-rules (merge-sort (lambda (l r) (string<=? (car l) (car r))) balsa-md-target-help))
		)
		(print "help:")
		(newline)
		(print #\ht "@echo 'Target " (make-string (- max-rule-name-length 6) #\space) "Meaning'")
		(newline)
		(print #\ht "@echo '" (make-string max-rule-name-length #\-) " "
			(make-string max-explanation-length #\-) "'")
		(newline)
		(for-each (lambda (rule)
			(print #\ht "@echo '" (string-append (car rule)
				(make-string (- max-rule-name-length (string-length (car rule))) #\space)
				" " (cdr rule)) "'")
			(newline)
		) sorted-rules)
		(print #\ht "@echo")
		(newline)
		(newline)
	)
))

;;; print-misc-rules: print rules for make file regeneration and cleaning up generated files.
(define print-misc-rules (lambda (top-blocks all-blocks)
	(let*
		((block->file (lambda (n) (brz-block-name->path n "balsa" "")))
		 (files (map block->file all-blocks))
		 ; dotted-files: files in the current dir. with dotted names (such as .bcode/.dcode)
		 (dotted-files (map (lambda (n) (string-append (brz-trim-square-brackets n) ".")) all-blocks))
		 (top-files (map block->file top-blocks))
		)
		(print "depend:" #\newline)
		(print #\ht "balsa-md " balsa-md-command-line-string #\newline)
		(newline)
		(print "tidy:" #\newline)
		(for-each (lambda (file-type)
			(print #\ht "$(RM)" (build-string files " " file-type) #\newline)
		) '("breeze" "l" "ps" "bcode" "dcode" "c" "h" "opt.c" "opt.o" "callContext.breeze"))
; LJ 10/09/2002
;		(for-each (lambda (file-type)
;			(print #\ht "$(RM)" (build-string dotted-files " " file-type) #\newline)
;		) '("bcode" "dcode" "c" "h" "opt.o"))
		; automagically generated test harness files' bcodes
		(print #\ht "$(RM)")
		(for-each (lambda (file-type)
			(for-each (lambda (fixture)
				(print " test-" (car fixture) file-type)
			) balsa-md-test-fixtures)
		) '(".bcode" ".c" ".h" ".opt.c" ".opt.o" "_TEMPORARY.testdesc" "_TEMPORARY.breeze" "_TEMPORARY.hhh"))
		; hand-written LARD .bcode files
		(for-each (lambda (file-type)
			(for-each (lambda (fixture)
				(let*
					((lard-file-name (caddr fixture))
					 (ext-file-name (string-append (final-string lard-file-name ".l") file-type))
					)
					(print " " ext-file-name)
				)
			) balsa-md-lard-test-fixtures)
		) '(".bcode" ".c" ".h" ".opt.c" ".opt.o"))
		(for-each (lambda (file-type)
			(for-each (lambda (fixture)
				(let*
					((lard-file-name (caddr fixture))
					 (ext-file-name (string-append (final-string lard-file-name ".l") file-type))
					)
					(print " test-" (car fixture) file-type)
				)
			) balsa-md-lard-test-fixtures)
		) '("_TEMPORARY.testdesc" "_TEMPORARY.breeze" "_TEMPORARY.hhh" ))
		(for-each (lambda (file-type)
			(for-each (lambda (fixture)
				(print " " (brz-trim-square-brackets (cadr fixture)) file-type)
			) balsa-md-lard-test-fixtures)
		) '("_breezelardinterface.bcode" "_breezelardinterface.dcode" "_breezelardinterface.l"))
		(print #\newline)
		; Remove implementation generated files
		(for-each (lambda (impl)
			(let*
				((impl-name (car impl))
				 (list-file (string-append "impl-" impl-name ".lst"))
				)
				(print #\ht "if test -s " list-file "; then \\" #\newline)
				(print #\ht "cat " list-file " | while read file; do $(RM) $$file; done; \\" #\newline)
				(print #\ht "$(RM) " list-file "; \\" #\newline)
				(print #\ht "fi" #\newline)
				(balsa-md-add-to-target-help (string-append "impl-" impl-name) (string-append
					"Implement block " (cadr impl) " using rules " impl-name))
			)
		) balsa-md-implementations)
		(newline)
		(balsa-md-add-to-target-help "tidy" "Remove auto-generated files (except test harnesses)")
		(balsa-md-add-to-target-help "clean" "Remove all auto-generated files")
		(balsa-md-add-to-target-help "cost" "Estimate Breeze block costs")
		(balsa-md-add-to-target-help "ps" "Generate PostScript graphs for a Breeze files")
		(print "clean: tidy" #\newline)
		; automagically generated test harness files
		(for-each (lambda (fixture)
			(print #\ht "$(RM) test-" (car fixture) ".l" #\newline)
		) balsa-md-test-fixtures)
		(newline)
		(print "ps:" (build-string top-files " " "ps"))
		(newline)
		(newline)
		(print "cost:" (build-string top-files " " "breeze"))
		(newline)
		(for-each (lambda (top-file)
			(print #\ht "breeze-cost " top-file "breeze")
			(newline)
		) top-files)
		(newline)
		(balsa-md-print-help-rule)
	)
))

;;; print-implementation: print balsa-md rules for a single implementation
(define print-implementation (lambda (impl)
	(let*
		((name (car impl))
		 (block-name (cadr impl))
		 (breeze-block-name (brz-block-name->path block-name "balsa" "breeze")) ; expanded block name
		 (impl-rule-name (string-append "impl-" name))
		)
		(print impl-rule-name " " impl-rule-name ".log " impl-rule-name ".lst: "
			breeze-block-name #\newline)
		(print #\ht "BALSATECH='$(IMPLTECH_" name ")'; export BALSATECH; \\" #\newline)
		(print #\ht "$(BALSANETLIST) $(BALSANETLISTOPTS) $(IMPLPRE_" name ") $(IMPLOPTS_" name ") '"
			block-name "'" #\newline)
		(newline)
	)
))

;;; balsa-md-{no-banner,...}: command line switches
(define balsa-md-no-banner #f)
(define balsa-md-makefile-name "Makefile")
(define balsa-md-test-fixtures '()) ; list of (name block-name proc-name) triples
; (name block-name file-name) triples for predefined lard test fixtures
(define balsa-md-lard-test-fixtures '())
(define balsa-md-command-line-string #f)
(define balsa-md-defined-variables '())
(define balsa-md-undefined-variables '())
(define balsa-md-debug #f)
(define balsa-md-project-name "project")
(define balsa-md-files '())
(define balsa-md-import-path balsa-search-path)
(define balsa-md-implementations '()) ; list of (name block-name)
(define balsa-md-simulation-system "interpreter")

(define balsa-md-printed-banner #f)

;;; balsa-md-print-banner: print the balsa-md banner
(define balsa-md-print-banner (lambda ()
	(if (not balsa-md-printed-banner)
		(make-program-banner "balsa-md" "balsa-md: Balsa Makefile generator"
			"1997-2002, The University of Manchester")
	)
	(set! balsa-md-printed-banner #t)
))

;;; balsa-md-usage: command usage
(define balsa-md-usage (lambda ()
	(balsa-md-print-banner)
	(error
		"version " balsa-version #\newline
		"usage: balsa-md {<switch>}* {<block/file-name>}+" #\newline #\newline
		"switches: -o <file-name>     - Name of target makefile (--output-file)" #\newline
		"          -h or -?           - Display this message (--help)" #\newline
		"          -b                 - Don't print the balsa-md banner (--no-banner)" #\newline
		"          -f <name> <block-name> <proc-name>" #\newline
		"                             - Make a named test harness generating rule (and" #\newline
		"                               simulation rules) for proc. proc-name. (--test-fixture)" #\newline
		"          -T <base-name>     - Use <base-name> as the base of the test harness" #\newline
		"                               file name (--test-fixture-file)" #\newline 
		"          -U <variable>      - Don't define the named variable in the generated file (--undefine)" #\newline
		"          -D <var> <value>   - Define the variable <var> with value <value> (--define)" #\newline
		"          -p <directory>     - Read the Project file out of the given directory and use" #\newline
		"                               that as the basis of the Makefile (--project)" #\newline
		"          -v <block>         - Mark the named block as having being precompiled, i.e. don't" #\newline
		"                               spit out rules about compiling it (--visited)" #\newline
		"          -g                 - Pass debugging flags to breeze2lard and balsa-lcd (--debug)" #\newline
		"          -I <directory>     - Add named directory to the import path for balsa-c and" #\newline
		"                               dependency analysis (--import)" #\newline
		"          -i <name> <block-name> - Add an implementation (a run of balsa-netlist) to" #\newline
		"                               the Makefile called impl-<name> for block <block-name>" #\newline
		"                               (--implementation)" #\newline
		"          -s <interpreter|lard2c|breezesim>" #\newline
		"                             - Choose the simulation system (--simulation-system)" #\newline
	)
))

;;; balsa-md-add-test-fixture: add a test fixture defn. to the global balsa-md-test-fixtures
;;;		list.  Canonicalise the given block name
(define balsa-md-add-test-fixture (lambda (test-name block-name proc-name)
	(cond
		((or (assoc test-name balsa-md-test-fixtures)
			(assoc test-name balsa-md-lard-test-fixtures))
			(error "balsa-md: test fixture `" test-name "' already defined"))
		(else
			(set! balsa-md-test-fixtures
				(cons
					(list
						test-name
						(brz-canonicalise-block-name block-name)
						proc-name
					)
					balsa-md-test-fixtures
				)
			)
		)
	)
))

;;; balsa-md-add-lard-test-fixture: add a predefined LARD file to the global balsa-md-lard-test-fixtures
;;;		list.  Canonicalise the given block name
(define balsa-md-add-lard-test-fixture (lambda (test-name block-name file-name)
	(cond
		((not (file-exists? file-name))
			(error "balsa-md: LARD test harness file `" file-name "' doesn't exist"))
		((or (assoc test-name balsa-md-test-fixtures)
			(assoc test-name balsa-md-lard-test-fixtures))
			(error "balsa-md: test fixture `" test-name "' already defined"))
		(else
			(set! balsa-md-lard-test-fixtures
				(cons
					(list
						test-name
						(brz-canonicalise-block-name block-name)
						file-name
					)
					balsa-md-lard-test-fixtures
				)
			)
		)
	)
))

;;; balsa-md-add-implementation: add a scheduled run of balsa-netlist under the
;;;		rule impl-`impl-name' to a list of such runs.  This run of balsa-netlist will
;;;		be on block `block-name'.  Canonicalise the given block name
(define balsa-md-add-implementation (lambda (impl-name block-name)
	(cond
		((assoc impl-name balsa-md-implementations)
			(error "balsa-md: implementation `" impl-name "' already defined"))
		(else
			(set! balsa-md-implementations
				(cons
					(list
						impl-name
						(brz-canonicalise-block-name block-name)
					)
					balsa-md-implementations
				)
			)
			; generate log file impl-<name>.log
			(balsa-md-define-variable
				(string-append "IMPLPRE_" impl-name)
				(string-append "-l impl-" impl-name ".lst -L impl-" impl-name ".log")
			)
		)
	)
))

;;; balsa-md-define-variable: add a definition for a variable in balsa-md-defined-variable
(define balsa-md-define-variable (lambda (name value)
	(set! balsa-md-defined-variables (cons (list name value) balsa-md-defined-variables))
))

;;; balsa-md-set-simulation-system: check for a valid argument and set balsa-md-simulation-system
(define valid-simulation-systems '("interpreter" "lard2c" "breezesim" "development_breezesim_lard2c"))
(define balsa-md-set-simulation-system (lambda (name)
	(if (member name valid-simulation-systems)
		(set! balsa-md-simulation-system name)
		(error "balsa-md: invalid simulation system. Valid values:" valid-simulation-systems)
	)

	(cond
		((or (string=? name "interpreter") (string=? name "breezesim"))
			(balsa-md-define-variable "LCD" "balsa-lcd"))
		((or (string=? name "lard2c") (string=? name "development_breezesim_lard2c"))
			(balsa-md-define-variable "LCD" "balsa-l2cd"))
	)
))

;;; balsa-md-target-help: pairs of (target-name . explanation) for each target made in this
;;;		Makefile dumped as the make target `help'
(define balsa-md-target-help '())

;;; balsa-md-add-to-target-help: add a single target to balsa-md-target-help
(define balsa-md-add-to-target-help (lambda (target help-string)
	(set! balsa-md-target-help (cons (cons target help-string) balsa-md-target-help))
))

;;; balsa-md-command-line-rules: command-line-args action rules
(define balsa-md-command-line-rules `(
	(#\f "test-fixture" 3 ,(lambda (args)
		(balsa-md-add-test-fixture (car args) (cadr args) (caddr args))
	))
	(#\L "lard-test-fixture" 3 ,(lambda (args)
		(balsa-md-add-lard-test-fixture (car args) (cadr args) (caddr args))
	))
	(#\o "output-file" 1 ,(lambda (args) (set! balsa-md-makefile-name (car args))))
	(#\b "no-banner" 0 ,(lambda (args) (set! balsa-md-no-banner #t)))
	(#\U "undefine" 1 ,(lambda (args) 
		(set! balsa-md-undefined-variables (cons (car args) balsa-md-undefined-variables))
	))
	(#\D "define" 2 ,(lambda (args) 
		(balsa-md-define-variable (car args) (cadr args))
	))
	(#\p "project" 1 ,(lambda (args) (balsa-md-read-project-file (car args))))
	(#\v "visited" 1 ,(lambda (args)
		(set! brz-library-balsa-blocks (merge-sort string<=? (cons (car args) brz-library-balsa-blocks)))
	))
	(#\g "debug" 0 ,(lambda (args) (set! balsa-md-debug #t)))
	(#\I "import" 1 ,(lambda (args) (set! balsa-md-import-path (append balsa-md-import-path (list (car args))))))
	(#\i "implementation" 2 ,(lambda (args)
		(balsa-md-add-implementation (car args) (cadr args))
	))
	(#\s "simulation-system" 1 ,(lambda (args) 
		(balsa-md-set-simulation-system (car args))
	))
	(#\h "help" 0 ,(lambda (args) (balsa-md-usage)))
	(#\? "help" 0 ,(lambda (args) (balsa-md-usage)))
))

;;; balsa-md-parse-command-line: parse switches from the given command line list, set
;;;		the balsa-md-... globals and return the tail of the command line.
(define balsa-md-parse-command-line (lambda (args)
	(set! balsa-md-command-line-string
		(build-separated-string (map bourne-escape-command-line-arg args) " "))
	(if (null? args)
		(balsa-md-usage)
		(parse-command-line "balsa-md" balsa-md-command-line-rules balsa-md-usage args)
	)
))

;; balsa-md-read-project-file:test: read a test fixture desc. from the project file element test
(define balsa-md-read-project-file:test (lambda (file test)
	(let*
		((syntax-error (lambda () (error "balsa-md-read-project-file:test: error in Project file `"
			directory "/Project'")
		 ))
		 (test-name (cadr test))
		 (proc-name (caddr test))
		 (fixture (list test-name file proc-name))
		 ; parse-port: parse a port entry into a breeze2lard command string
		 (parse-port (lambda (port)
			(case (car port)
				((input-port)
				    (string-append
					(if (eqv? (caddr port) 'file)
						(string-append "-f " (cadr port) " " (cadddr port))
						(string-append "-v " (cadr port) " \"" (cadddr port) "\"")
					)
					(if (>= (length port) 5) (string-append " \"\\\"" (caddr (cddr port)) "\\\"\"") "")
					(if (>= (length port) 6) (string-append " \"\\\"" (cadddr (cddr port)) "\\\"\"") "")
				    )
				)
				((output-port)
					(if (eqv? (caddr port) 'file)
						(string-append "-f " (cadr port) " " (cadddr port)
							       (if (>= (length port) 5) (string-append " \"\\\"" (caddr (cddr port))) "\\\"\"")
							       (if (>= (length port) 6) (string-append " \"\\\"" (cadddr (cddr port))) "\\\"\"")
						)
						""
					)
				)
				((memory-component)
					(string-append "-m " (cadr port) " " (caddr port) " " (cadddr port) " " (caddr (cddr port)))
				)
				(else "")
			)
		 ))
		)
		(for-each (lambda (elem)
			(if (headed-list? elem)
				(case (car elem)
					((ports)
						(balsa-md-define-variable
							(string-append "TESTOPTS_" test-name)
							(build-separated-string (map parse-port (cdr elem)) " ")
						)
					)
				)
				(syntax-error)
			)
		) (cdddr test))
		fixture
	)
))

;; balsa-md-read-project-file:lard-test-harness: parse a lard-test-harness out of the project file
(define balsa-md-read-project-file:lard-test-harness (lambda (file test)
	(let*
		((syntax-error (lambda () (error "balsa-md-read-project-file:lard-test-harness: error in Project file `"
			directory "/Project'")
		 ))
		 (test-name (cadr test))
		 (lard-file-name (caddr test))
		 (fixture (list test-name file lard-file-name))
		 (set-variable (lambda (elem prefix)
			(if (/= 2 (length elem))
				(syntax-error)
				(balsa-md-define-variable
					(string-append prefix "_" test-name)
					(cadr elem)
				)
			)
		 ))
		)
		(for-each (lambda (elem)
			(if (headed-list? elem)
				(case (car elem)
					((simulation-options) ; build options for the test harness
						(set-variable elem "SIMOPTS")
					)
					((simulation-arguments) ; simulation run arguments
						(set-variable elem "SIMARGS")
					)
					((build-options) ; test harness build options
						(set-variable elem "TESTOPTS")
					)
				)
				(syntax-error)
			)
		) (cdddr test))
		fixture
	)
))

;; balsa-md-read-project-file:implementation: read an implementation desc. from the project file element impl
(define balsa-md-read-project-file:implementation (lambda (file impl)
	(let*
		((syntax-error (lambda () (error "balsa-md-read-project-file:implementation: error in Project file `"
			directory "/Project'")
		 ))
		 (impl-name (cadr impl))
		 (implementation (list impl-name file))
		)
		(for-each (lambda (elem)
			(if (headed-list? elem)
				(case (car elem)
					((technology)
						(if (/= 2 (length elem))
							(syntax-error)
							(balsa-md-define-variable
								(string-append "IMPLTECH_" impl-name)
								(cadr elem)
							)
						)
					)
					((balsa-netlist-options)
						(if (/= 2 (length elem))
							(syntax-error)
							(balsa-md-define-variable
								(string-append "IMPLOPTS_" impl-name)
								(cadr elem)
							)
						)
					)
				)
				(syntax-error)
			)
		) (cddr impl))
		implementation
	)
))

;;; balsa-md-read-project-file:file: read a file description from the project file element file
;;;		and update the global variables
(define balsa-md-read-project-file:file (lambda (file)
	(let
		((syntax-error (lambda () (error "balsa-md-read-project-file:file: error in Project file `"
			directory "/Project'")
		 ))
		 (add-file (lambda (filename)
			(set! balsa-md-files (cons (brz-canonicalise-block-name filename) balsa-md-files))
		 ))
		)
		(if (string? file)
			(if (not (find-filename file "balsa" #t balsa-md-import-path))
				(error "balsa-md-read-project-file: invalid block name `" (cadr file) "'")
				(add-file file)
			)
			(if (headed-list? file 'top-level)
				(if (find-filename (cadr file) "balsa" #t balsa-md-import-path)
					(begin
						(add-file (cadr file))
						(for-each (lambda (file-elem)
							(if (headed-list? file-elem)
								(case (car file-elem)
									((test) ; test fixture desc.
										(let
											((test (balsa-md-read-project-file:test
												(cadr file) file-elem)))
											(if test
												(apply balsa-md-add-test-fixture test)
											)
										)
									)
									((lard-test-harness) ; test fixture desc.
										(let
											((test (balsa-md-read-project-file:lard-test-harness
												(cadr file) file-elem)))
											(if test
												(apply balsa-md-add-lard-test-fixture test)
											)
										)
									)
									((implementation) ; balsa-netlist implementation
										(let
											((impl (balsa-md-read-project-file:implementation
												(cadr file) file-elem)))
											(if impl
												(apply balsa-md-add-implementation impl)
											)
										)
									)
								)
								(syntax-error)
							)
						) (cddr file)) 
					)
					(error "balsa-md-read-project-file: invalid block name `" (cadr file) "'") 
				)
				(syntax-error)
			)
		)
	)
))

;;; balsa-md-read-project-file: read the file Project from the given directory
;;;		and fill the global variables from that file
(define balsa-md-read-project-file (lambda (directory)
	(let*
		((project (car (get-file (string-append directory "/Project"))))
		 (syntax-error (lambda () (error "balsa-md-read-project-file: error in Project file `"
			directory "/Project'")
		 ))
		)
		(chdir directory) ; change to that directory
		(if (headed-list? project 'balsa-project)
			(for-each (lambda (elem)
				(if (headed-list? elem)
					(case (car elem)
						((name) (set! balsa-md-project-name (cadr elem)))
						; directory, ignore
						((import-path)
							(balsa-md-define-variable "BALSAIMPORTPATH" (brz-make-balsa-import-path (cdr elem)))
							(set! balsa-md-import-path (append balsa-search-path (cdr elem)))
						)
						((files) (for-each balsa-md-read-project-file:file (cdr elem)))
						((technology)
							(print-err "balsa-md: use of (technology ...) node at top of Project file is deprecated" #\newline)
						)
						((simulation-system)
							(balsa-md-set-simulation-system (cadr elem))
						)
						((defines)
							(for-each (lambda (elem)
								(if (/= 3 (length elem)) (syntax-error))
								(balsa-md-define-variable (cadr elem) (caddr elem))
							) (cdr elem))
						)
					)
					(syntax-error)
				)
			) (cdr project))
			(syntax-error)
		)
	)
))

;;; brz-balsa-md: make a makefile for the top Balsa blocks `top-blocks'
(define balsa-md (lambda top-blocks
	; (if (null? top-blocks) (balsa-md-usage))
	(let*
		((top-blocks-w/o-ext
			(map (lambda (block)
				(let ((trimmed (final-string block ".balsa")))
					(if (strchr block #\/)
						(error "balsa-md: `" block "' use a dotted-path for a Balsa block in a different directory")
					)
					(brz-canonicalise-block-name (if trimmed trimmed block))
				)
			) top-blocks)
		 )
		 (rules (apply brz-make-top-rule-set top-blocks-w/o-ext))
		 (filtered-rules (map (lambda (rule)
			(cons (brz-rule:target rule)
				(filter (lambda (e) (not (member e brz-library-balsa-blocks))) (brz-rule:dependencies rule))
			)
		 ) rules))
		 (flat-rules (brz-flatten-rule-set rules))
		 (block-names (lambda (blocks ext)
		 	(map (lambda (n) (brz-block-name->path n "balsa" ext)) blocks)
		 ))
		 (top-breeze-files (block-names top-blocks-w/o-ext "breeze"))
		 (top-bcode-files (block-names top-blocks-w/o-ext "bcode"))
		)
		(with-output-to-file balsa-md-makefile-name (lambda ()
			(print-make-header)
			(print "### Must use a Bourne shell" #\newline)
			(print "SHELL = /bin/sh" #\newline)
			(newline)
			(print-make-definitions)
			(newline)
			(print "all: " (build-string top-breeze-files "" " "))
			(newline)
			(newline)
			(print "### Balsa rules" #\newline)
			(print-balsa-rules filtered-rules)
			(newline)
			(if (or (string=? balsa-md-simulation-system "interpreter") (string=? balsa-md-simulation-system "lard2c"))
			    ( let()
			      (print "### LARD rules" #\newline)
			      (print-lard-rules rules flat-rules)
			    )
;			    ( let()
;			      (print "### Breezesim rules" #\newline)
;			      (print-breezesim-rules filtered-rules)
;			      (newline)
;			    )
			)
			(print "### Test harness rules" #\newline)
			(print-test-fixtures top-blocks-w/o-ext top-breeze-files top-bcode-files flat-rules)
			(print "### Implementation rules" #\newline)
			(for-each (lambda (impl) (print-implementation impl)) balsa-md-implementations)
			(print "### Miscellaneous rules" #\newline)
			(print-misc-rules top-blocks-w/o-ext (foldl (lambda (blocks rule)
				(union string<=? blocks (merge-sort string<=? rule))
			) '() filtered-rules))
		))
	)
))
