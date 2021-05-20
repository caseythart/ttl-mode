;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Create the Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode ttl-mode fundamental-mode "ttl mode"
  "ttl mode is for creating and editing .ttl files."
  ;; get ttl-mode variable bindings
  (get-ttl-mode-variable-bindings)
  
  ;; syntax table stuff
  (ttl-mode-syntax-settings)
  
  ;; colorizing stuff
  ;;  (ttl-syntax-highlighting)
  (set-font-lock-defaults)

  ;; indenting stuff
  (setq tab-width 8)
  ;;NOTE start new block after period
  ;;go to start of line without end punctuation

  ;; key bindings
  (set-ttl-mode-key-bindings)

  )

;; define and function to grab variables

(defvar *base-prefix* ":"
  "base-prefix is the default prefix for a ttl-file, used in a variety of ttl-mode functions. We will assume the base prefix is the first prefix declared in a .ttl file. Defaults to ':' if no declarations are made.")

(defvar *defined-prefix-list* nil
  "defined-prefix-list is a list of strings, where each member is a prefix defined in a given ttl file")

(defvar *defined-prefix-list-regex* ":"
  "defined-prefix-list-regex is a regex generated out of the list of strings, defined-prefix-list")

(defvar *defined-prefix-list-sans-colon* nil
  "defined-prefix-list-sans-colon is the prefixes in defined-prefix-list minus the colons at the end")

(defun get-ttl-mode-variable-bindings ()
  "Finds bindings for the wide-scope variables used by ttl-mode.el."
  (get-prefix-list)			; get defined-prefix-list binding
  (get-base-prefix)			; get base-prefix binding
  (get-prefix-list-regex)
  (get-prefix-list-sans-colon *defined-prefix-list*)
  )

(defun get-base-prefix ()
  "This function gets the first declared prefix in a turtle file. We assume this is the 'base prefix', and is the prefix that folks will predominantly use when creating new terms. If no prefixes are declared, ':' is set as the default prefix."
  (setq *base-prefix*
	(if *defined-prefix-list* (car (last *defined-prefix-list*)) ":")))

(defun get-prefix-list ()
  "This function should only be used in a buffer containing a ttl file, though perhaps other ontology files will behave similarly. The function evaluates to a list of strings, one for each prefix defined in the file. This function assumes that '@prefix ' will precede, and ':' will follow, any defined prefix. These declarations are usually in the header of a file, but this need not be the case."
  (save-excursion
    (setq *defined-prefix-list* nil)
    (goto-char 0)
    (while (search-forward "@prefix" nil t nil)
      (forward-char 1)
      (push (concat (thing-at-point 'word 'no-properties) ":") *defined-prefix-list*))))

(defun get-prefix-list-regex ()
  "This function creates a regexp that can be used for coloring defined prefixes."
  (setq *defined-prefix-list-regex*
	(if *defined-prefix-list* (regexp-opt *defined-prefix-list*) (list "\\(?:owl\\|rdfs?\\|skos\\)"))))

(defun get-prefix-list-sans-colon (old-list)
  "This function takes a list of strings and evaluates to a list with the same items, except all colon suffixes are removed."
    (setq *defined-prefix-list-sans-colon*
	  (mapcar #'(lambda (item) (string-remove-suffix ":" item)) old-list))) 

;; colorizing
(defun set-font-lock-defaults ()
    (progn (setq ttl-mode-font-lock-keywords
		 (let* ((prefix-keywords *defined-prefix-list-sans-colon*)
			(warning-words '("TODO" "FIXME"))                                 ;; add to list if desired
			(prefix-keywords-regexp (regexp-opt prefix-keywords))
			(subject-keywords-regexp  (concat "^" prefix-keywords-regexp "\\([[:word:]_-]+\\)\\>"))
			(warning-words-regexp (regexp-opt warning-words 'words)))
		   `(
		     (,subject-keywords-regexp . font-lock-keyword-face)              ;; subjects
		     (,"\\^\\^[^,;.]+\\|@en" . font-lock-comment-face)                   ;; literal types
		     (," a \\|:\\([[:word:]_-]+\\)\\>\\|<.*>" . font-lock-constant-face) ;; resource names
		     (,prefix-keywords-regexp . font-lock-type-face)                     ;; defined prefixes
		     (,"@prefix\\|@base" . font-lock-doc-face)                           ;; prefix declaration
		     (,warning-words-regexp . font-lock-warning-face)                    ;; warnings
		     (,";\\|\\." . font-lock-builtin-face)                               ;; line end punctuation
		     )))
	   (setq font-lock-defaults '((ttl-mode-font-lock-keywords)))))


;; syntax
(defun ttl-mode-syntax-settings ()
  (setq comment-start "#")                               ; var for comment-region, etc.
  (modify-syntax-entry ?# "< b" ttl-mode-syntax-table)   ; '#' starts comments
  (modify-syntax-entry ?\n "> b" ttl-mode-syntax-table)  ; \n ends comments
  (modify-syntax-entry ?_ "w" ttl-mode-syntax-table)     ; '_' is part of a word
  )

;; key bindings
(defun set-ttl-mode-key-bindings ()      
  (define-key ttl-mode-map (kbd "C-c C-c f") 'create-class)                      ;; create-class and variants
  (define-key ttl-mode-map (kbd "C-c C-c q") 'create-class-quick)
  (define-key ttl-mode-map (kbd "C-c C-c n") 'create-class-nil)
  (define-key ttl-mode-map (kbd "C-c C-p") 'create-property)                     ;; create-property
  (define-key ttl-mode-map (kbd "C-c C-o f") 'create-object-property)            ;;create-object-property and variants
  (define-key ttl-mode-map (kbd "C-c C-o q") 'create-object-property-quick)
  (define-key ttl-mode-map (kbd "C-c C-o n") 'create-object-property-nil)
  (define-key ttl-mode-map (kbd "C-c C-a f") 'create-annotation-property)        ;;create-annotation-property and variants
  (define-key ttl-mode-map (kbd "C-c C-a q") 'create-annotation-property-quick)
  (define-key ttl-mode-map (kbd "C-c C-a n") 'create-annotation-property-nil)
  (define-key ttl-mode-map (kbd "C-c C-d f") 'create-datatype-property)            ;;create-datatype-property and variants
  (define-key ttl-mode-map (kbd "C-c C-d q") 'create-datatype-property-quick)
  (define-key ttl-mode-map (kbd "C-c C-d n") 'create-datatype-property-nil)
  (define-key ttl-mode-map (kbd "C-c C-i f") 'create-individual)                   ;; create-individual and variants
  (define-key ttl-mode-map (kbd "C-c C-i q") 'create-object-property-quick)
  (define-key ttl-mode-map (kbd "C-c C-i n") 'create-object-property-nil)
  (define-key ttl-mode-map (kbd "C-c C-r") 'create-rule)                         ;; create-rule
  (define-key ttl-mode-map (kbd "C-c C-s i") 'create-instance)                   ;; create-(sub)instance
  (define-key ttl-mode-map (kbd "C-c C-s c") 'create-subclass)                   ;; create-subclass
  (define-key ttl-mode-map (kbd "C-c C-s p") 'create-subproperty)                ;; create-subproperty
  (define-key ttl-mode-map (kbd "C-c C-j s") 'jump-to-section)                   ;; jump to section
  (define-key ttl-mode-map (kbd "C-c C-j r") 'jump-to-resource)                  ;; jump to resource
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Resource Creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; create thing
(defun owl-create (type)
  "When called, prompts user to select the type of thing they want to create."
  (interactive "sChoose the type of things you wish to create: (c)lass, (p)roperty, (i)ndividual, (r)ule.")
  (case type
	("c" (call-interactively 'create-class))
	("p" (call-interactively 'create-property))
        ("i" (call-interactively 'owl-individual))
	("r" (call-interactively 'owl-rule))
	(otherwise (message "Invalid type selected. You must enter 'c', 'p', 'i', or 'r'."))))

;;; create class TODO: things like TransferEvent are not lexifying properly in default
(defun create-class (class superclass lexical)
  "Creates a new class."
  (interactive "sClass name: \nsSubclass of (defaults to owl:Thing): \nsLexical (defaults to just the name): ")
  (let*
      ((class-with-base-prefix (concatenate 'string *base-prefix* class))
       (default-superclass "owl:Thing")
       (superclass (default-string-if-empty-or-nil superclass default-superclass))
       (default-lexical (downcase class))
       (lexical (default-string-if-empty-or-nil lexical default-lexical)))
    (insert class-with-base-prefix " a owl:Class ;
	rdfs:label \""class) (camelcase-to-sentence-case) (insert "\"^^xsd:string ;
        skos:prefLabel \""lexical"\"@en ;
	rdfs:subClassOf "superclass" ;
	rdfs:comment \"\"\"" class-with-base-prefix " is the class of all \"\"\"^^xsd:string ;
        "*base-prefix*"exampleSubclass \"\";
        "*base-prefix*"exampleInstance \"\";
.")
    (search-backward "all")
    (forward-char 4)
    (message (concatenate 'string "Created " class-with-base-prefix "! Complete the comment, and then add in example subclasses and instances below."))))

(defun create-class-quick (input)
  "Calls create-class that takes a single argument, which is just the ist of strings of all the arguments."
  (interactive "x(\"class\" \"superclass\" \"lexical\"): ")
  (let ((class (car input))
	(superclass (car (cdr input)))
	(lexical (car (cdr (cdr input)))))
  (create-class class superclass lexical)))

(defun create-class-nil ()
  "Calls create-class with default or nil arguments so no user interaction is required."
  (interactive)
  (create-class "" "" ""))

;;; create property
(defun create-property (type)
  "Prompts user to select the type of property to create, and then calls the appropriate function to create said property."
  (interactive "sEnter Type ('o' = Object, 'd' = Datatype, 'a' = Annotation): ")
  (case type
	("o" (call-interactively 'create-object-property))
	("d" (call-interactively 'create-datatype-property))
	("a" (call-interactively 'create-annotation-property))
	(otherwise (message "Invalid property type selected. You must enter 'o', 'd', or 'a'."))))

; object property
(defun create-object-property (property domain range lexical superproperty)
  "Creates an object property."
  (interactive "sProperty Name (e.g. 'hasPart', 'performsActFreely'): \nsDomain (defaults to owl:Thing if left blank): \nsRange (defaults to owl:Thing if left blank): \nsEnglish Translation (e.g. :father may be 'is the father of'): \nsSubproperty of (leave blank if none known): ")
  (write-property-details-for-type property "owl:ObjectProperty" domain range lexical superproperty))

(defun create-object-property-quick (input)
  "Calls create-object-property that takes a single argument, which is just the list of strings of all the arguments."
  (interactive "x(\"property\" \"domain\" \"range\" \"lexical\" \"superproperty\"): ")
  (let ((property (nth 1 input))
	(domain (nth 2 input))
	(range (nth 3 input))
	(lexical (nth 4 input))
	(superproperty (nth 5 input)))
    (create-object-property property domain range lexical superproperty)))

(defun create-object-property-nil ()
  "Calls create-object-property with default or nil arguments so no user intearction is required."
  (interactive)
  (create-object-property "" "" "" "" ""))

; datatype property
(defun create-datatype-property (property domain range lexical superproperty)
  "Creates a datatype property."
  (interactive "sProperty Name (e.g. 'hasLabel', 'numberOfBedrooms'): \nsDomain (defaults to owl:Thing if left blank): \nsRange (defaults to xsd:string if left blank): \nsEnglish Translation (e.g. :name may be 'is named'): \nsSubproperty of (leave blank if none known): ")
  (write-property-details-for-type property "owl:DatatypeProperty" domain range lexical superproperty))

(defun create-datatype-property-quick ()
  "Calls create-datatype-property that takes a single argument, which is just the ist of strings of all the arguments."
  (interactive "x(\"property\" \"domain\" \"range\" \"lexical\" \"superproperty\"): ")
  (let ((property (nth 1 input))
	(domain (nth 2 input))
	(range (nth 3 input))
	(lexical (nth 4 input))
	(superproperty (nth 5 input)))
    (create-datatype-property property domain range lexical superproperty)))

(defun create-datatype-property-nil ()
  "Calls create-datatype-property with default or nil arguments so no user intearction is required."  
  (interactive)
  (create-datatype-property "" "" "" "" ""))

; annotation property
(defun create-annotation-property (property domain range lexical superproperty)
  (interactive "sProperty Name (e.g. 'isOfType', 'exampleValue'): \nsDomain (defaults to owl:Thing if left blank): \nsRange (defaults to owl:Thing if left blank): \nsEnglish Translation (e.g. :exampleTriple may be 'can produce triples like the following:'): \nsSubproperty of (leave blank if none known): ")
  (write-property-details-for-type property "owl:AnnotationProperty" domain range lexical superproperty))

(defun create-annotation-property-quick ()
  "Calls create-annotation-property that takes a single argument, which is just the ist of strings of all the arguments."
  (interactive "x(\"property\" \"domain\" \"range\" \"lexical\" \"superproperty\"): ")
  (let ((property (nth 1 input))
	(domain (nth 2 input))
	(range (nth 3 input))
	(lexical (nth 4 input))
	(superproperty (nth 5 input)))
    (create-annotation-property property domain range lexical superproperty)))

(defun create-annotation-property-nil ()
  "Calls create-annotation-property with default or nil arguments so no user intearction is required."  
  (interactive)
  (create-annotation-property "" "" "" "" ""))

;; create individual (see create instance, below)
(defun create-individual (name class lexical)
  (interactive "sIndividual name: \nsInstance of (defaults to owl:Thing): \nsLexical (defaults to just the lower-cased name): ")
  (let*
      ((default-class "owl:Thing")
       (class (default-string-if-empty-or-nil class default-class))
       (default-lexical (downcase name))
       (lexical (default-string-if-empty-or-nil lexical default-lexical)))
    (insert *base-prefix* name " a " class " ;
	rdfs:label \""name) (camelcase-to-sentence-case) (insert "\"^^xsd:string ;
        skos:prefLabel \""lexical"\"@en ;
	rdfs:comment \"\"\""*base-prefix* name" is an instance of " class ".\"\"\"^^xsd:string ;
.")
    (message (concat "Created " *base-prefix* name "."))))

(defun create-individual-quick ()
  "Calls create-individual that takes a single argument, which is just the ist of strings of all the arguments."
  (interactive "x(\"name\" \"class\" \"lexical\": ")
  (let ((name (nth 1 input))
	(class (nth 2 input))
	(lexical (nth 3 input)))
    (create-annotation-property name class lexical)))

(defun create-individual-nil ()
  "Calls create-individual with default or nil arguments so no user intearction is required."  
  (interactive)
  (create-individual "" "" ""))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Derivative Resource Creation 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO SECTION
;; create subclass
(defun create-subclass (class lexical)
    (interactive "sClass name: \nsLexical (defaults to just the name): ")

    (create-class class superclass lexical)
  )

;; create subproperty
(defun create-subproperty ()
  )

;; create instance
(defun create-instance ()
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Rule Creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *rule-syntax* "stardog"
  "rule-syntax is set to stardog by default, but users will want to customize this to whatever syntax their engine(s) use.")

;; create rule
(defun create-rule ()
  (interactive)
  (case *rule-syntax*
	("stardog" (create-stardog-rule))
	("swrl" (create-swrl-rule))
	(otheriwse (message "*rule-syntax* is not set to a supported value."))))

;; Stardog Syntax
(defun create-stardog-rule ()
  (interactive)
  (insert "#\nIF {\n\n}\nTHEN {\n\n}"))

;; SWRL Syntax
(defun create-swrl-rule ()
  (interactive)
  (message "To be supported in the future."))

;; Property Path
(defun create-property-path (derived-property chain-links)
  (interactive "sDerived Property (include prefix): \nsChain (space separated, include prefixes): ")
  (insert derived-property " owl:propertyChainAxiom (" chain-links " )."))

(defun create-property-path-nil ()
  (interactive)
  (create-property-path "" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Component Creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; owl restriction
(defun ttl-create-restriction (property restriction-type restriction-value)
  (interactive "sonProperty: \nsRestriction type (e.g. someValuesFrom): \nsValue: ")
  (insert "[a owl:Restriction ;\n\t owl:onProperty " property" ;\n\t owl:" restriction-type " " restriction-value" ;]"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; File Setup or Manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; create-ttl-file
(defun create-ttl-file ()
  (interactive)
  ;; make the header
  (create-ttl-header)
  ;; define the ontology
  (call-interactively 'define-ontology-resource)
  ;; make the sections
  (add-section-headers)
  ;; call ttl-mode
  (ttl-mode)
  )


(defun create-ttl-header ()
  (beginning-of-buffer)
  (call-interactively 'get-and-insert-prefix))

(defun get-and-insert-prefix (prefix)
  (interactive "sPrefix (excluding ':'): ")
  ;; TODO: add in a 'do you want the standard prefixes" question: owl, rdf, rdfs, xsd, xml, skos
  (if (equal prefix "")
      (if (= (point) (point-min))
	  (progn (insert "@prefix : <http://TODO> .\n")
	       (call-interactively 'get-and-insert-prefix)))	  ; print the empty prefix if it's the first line
    (progn (insert "@prefix " prefix ": <http://TODO> .\n")       ; print the given prefix given and repeat
	   (call-interactively 'get-and-insert-prefix)))) 

(defun define-ontology-resource (comment)
  (interactive "sProvide a comment for this ontology: ")
  (insert "\n<SHOULD-MATCH-LINE-ONE-PREFIX-URL> a owl:Ontology;\n\trdfs:commment \""comment"\" ;\n."))

(defun add-section-headers ()
  (end-of-buffer)
  (create-section-header "Classes")
  (create-section-header "Properties")
  (create-section-header "\t\tObject Properties")
  (create-section-header "\t\tDatatype Properties")
  (create-section-header "\t\tAnnotation Properties")
  (create-section-header "Individuals")
  (create-section-header "Example Data"))

(defun create-section-header (title)
  (insert "\n#################################################################\n#    " title "\n#################################################################\n\n"))

;; get-defined-resources
(defvar defined-resource-list nil
  "The list of all resources that appear as subjects of triples in a given ttl file.")

(defun get-defined-resources ()
  (beginning-of-buffer)
  )


;; jump-to
(defun jump-to-section ()
  (interactive)
  (let ((char (read-char "(C)lasses;  (P)roperties: (O)bject, (D)atatype, (A)nnotation; (I)ndividuals; (E)xample Data")))
    (case char
	  ((or ?c ?C) (move-to-section "Classes"))
	  ((or ?p ?P) (move-to-section "Properties"))
	  ((or ?o ?O) (move-to-section "Object Properties"))
	  ((or ?d ?D) (move-to-section "Datatype Properties"))
	  ((or ?a ?A) (move-to-section "Annotation Properties"))
	  ((or ?i ?I) (move-to-section "Individuals"))
	  ((or ?e ?E) (move-to-section "Example Data"))
	  )))

(defun move-to-section (section-title)
  (beginning-of-buffer)
  (if (re-search-forward (concat "^#*\n#.*" section-title  "\n#*"))
      (forward-char 1)
    (message "That section was not found.")))

;; insert-resource-of-type
(defun insert-resource-of-type (resource type)
  (interactive)
  ;; find and copy resource
  ;; go to appropriate spot
  ;; insert copied resource
  )
   

;; rename resource
(defun rename-resource (old-name new-name)
  (interactive "sOld Name: \nsNew Name: ")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Supporting Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun camelcase-to-sentence-case ()
  "un-camelcase the word at point, replacing uppercase chars with
the lowercase version preceded by an underscore.

The first char, if capitalized (eg, PascalCase) is just
downcased, no preceding underscore.
"
  (interactive)
  (save-excursion
    (if (equal (thing-at-point 'word 'no-properties) nil) nil
      (let ((bounds (bounds-of-thing-at-point 'word)))
	(replace-regexp "\\([A-Z]\\)" " \\1" nil
			(1+ (car bounds)) (cdr bounds))
	(downcase-region (car bounds) (cdr bounds))))))

(defun delete-prefix (prefixed-term)
  "take a term with a prefix, e.g. owl:Thing, and then return it without the prefix, e.g. Thing."
  (replace-regexp-in-string "\\S-*?:" "" prefixed-term))


(defun default-string-if-empty-or-nil (string default)
  "This function returns the string value if it's non-empty, otherwise it returns the default."
  (if (or (equal string "") (equal string nil)) default string))
    
(defun alphabetize-list (list-of-strings)
  (sort list-of-strings 'string-lessp))




;; Workbench

(defun write-property-details-for-type (property type domain range lexical superproperty)
  (let* ((property-with-base-prefix (concatenate 'string *base-prefix* property))
	 (default-domain "owl:Thing")
	 (domain (default-string-if-empty-or-nil domain default-domain))
	 (default-range "owl:Thing")
	 (range (default-string-if-empty-or-nil range default-range)))
    (insert property-with-base-prefix " a " type " ;
	rdfs:label \""property)
    (camelcase-to-sentence-case)
    (insert "\"^^xsd:string ;
        skos:prefLabel \""lexical"\"@en ;
	")
    (if (equal superproperty "") nil (insert "rdfs:subPropertyOf "superproperty" ;
        "))
    (insert  "rdfs:domain "domain" ;
        rdfs:range "range" ;
	"
	(rdfs-comment-for-property-type type property-with-base-prefix domain range lexical)
	*base-prefix*"exampleTriple \"\";
.")
    (search-backward "(")
    (forward-char 2)
    (message (concatenate 'string "Created " property-with-base-prefix ". Create your example, and add it to the example triple below."))))


(defun rdfs-comment-for-property-type (type property-with-base-prefix domain range lexical)
  (case type
	("owl:ObjectProperty" "rdfs:comment \"\"\"(:"(delete-prefix domain)"1 "property-with-base-prefix" :"(delete-prefix range)"1) means that "(delete-prefix domain)"1 "lexical" "(delete-prefix range)"1. For example, (: "property-with-base-prefix" :).\"\"\"^^xsd:string ;
        ")
	("owl:DatatypeProperty" "rdfs:comment \"\"\"(:"(delete-prefix domain)"1 "*base-prefix* property" \\\""(upcase (delete-prefix range))"\\\""range") means that "(delete-prefix domain)"1 "lexical" "(upcase (delete-prefix range))". For example, (: "*base-prefix*" \"\"^^"range").\"\"\"^^xsd:string ;
        ")
	("owl:AnnotationProperty" "rdfs:comment \"\"\"(:"(delete-prefix domain)"1 "*base-prefix* property" :"(delete-prefix range)"1) means that "(delete-prefix domain)"1 "lexical" "(delete-prefix range)"1. For example, (: "*base-prefix*" :).\"\"\"^^xsd:string ;
        ")
	(otherwise "No property type specified.")))
