;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Create the Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode ttl-mode fundamental-mode "ttl mode"
  "ttl mode is for creating and editing .ttl files."
  ;; syntax table stuff
  (set-syntax-table ttl-mode-syntax-table)
  
  ;; colorizing stuff
  (ttl-syntax-highlighting)

  ;; indenting stuff
  (setq tab-width 8)
  
  )

;; colorizing
(defun ttl-syntax-highlighting ()
  "Adds keywords for syntax highlighting to the appropriate faces."
  (font-lock-add-keywords nil '(("@prefix\\|@base" . 'font-lock-type-face))) ; parts of the header
  (font-lock-add-keywords nil '(("\\S-*?:" . 'prefix))) ; prefixes (excluding those in datatypes). KEEP THIS ABOVE 'literal-type
  (font-lock-add-keywords nil '(("<.*>" . 'resource-fullname))) ; full uris, including the side carrots
  (font-lock-add-keywords nil '((":\\([[:word:]_-]+\\)\\>" . 'resource-shortname))) ; the short name for resources; the value after the colon
  (font-lock-add-keywords nil '(("^:\\([a-z0-9_]+\\)" . 'subject-term))) ; highlights prefix and shortname for resources that start lines.
  (font-lock-add-keywords nil '(("\\^\\^[^,;.]+\\|@en" . 'literal-type))) ; highlights literal types, like '^^xsd:string'. KEEP THIS BELOW 'prefix, else the prefix will override the datatype.
  )

;; faces, defined and then set to colors. To change colors, simply replace the hex values after :foreground.
(defface prefix  nil
  "face for prefixes in ttl-mode")

(face-spec-set 'prefix
	       '((t :foreground "#ablefed"))
	       'face-defface-spec)

(defface literal-type  nil 
  "face for literal values in ttl-mode")

(face-spec-set 'literal-type
	       '((t (:foreground "#ablefed")))
	       'face-defface-spec)

(defface subject-term  nil
  "face for resources used as the subject in ttl-mode")

(face-spec-set 'subject-term
	       '((t (:foreground "#cfbae1"
				 :weight bold)))
	       'face-defface-spec)

(defface resource-shortname  nil
  "face for resources used in ttl-mode")

(face-spec-set 'resource-shortname
	       '((t (:foreground "#f8c630")))
	       'face-defface-spec)

(defface resource-fullname  nil
  "face for resources used in ttl-mode")

(face-spec-set 'resource-fullname
	       '((t (:foreground "#a4def9")))
	       'face-defface-spec)

;; syntax

(defvar ttl-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table) ; '_' is part of a word
    (modify-syntax-entry ?# "< b" table) ; '#' starts comments
    (modify-syntax-entry ?\n "> b" table) ; \n ends comments
    table)
  "Syntax table for ttl-mode")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Resource Creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; create class
(defun owl-class (prefix class superclass lexical)
  "Creates a new class."
  (interactive "sPrefix: \nsClass name: \nsSubclass off (defaults to owl:Thing): \nsLexical (defaults to just the name): ")
  (let*
      ((default-superclass "owl:Thing")
       (superclass (return-default-if-empty superclass default-superclass))
       (default-lexical class)
       (lexical (return-default-if-empty lexical default-lexical)))
    (insert prefix ":" class " a owl:Class ;
	rdfs:label \""class) (camelcase-to-sentence-case) (insert "\"^^xsd:string ;
        skos:prefLabel \""lexical"\"@en ;
	rdfs:subClassOf "superclass" ;
	rdfs:comment \"\"\""class" is the class of all \"\"\"^^xsd:string ;
        olive:exampleSubclass \"\";
        olive:exampleInstance \"\";
.")
    (search-backward "all")
    (forward-char 4)
    (message (concat "Created " prefix ":" class "! Complete the comment, and then add in example subclasses and instances below."))))


;;; create property
(defun owl-property (type)
  "Prompts user to select the type of property to create, and then calls the appropriate function to create said property."
  (interactive "sEnter Type ('o' = Object, 'd' = Datatype, 'a' = Annotation): ")
  (if (equal type "o") (call-interactively 'owl-create-object-property)
    (if (equal type "d") (call-interactively 'owl-create-datatype-property)
      (if (equal type "a") (call-interactively 'owl-create-annotation-property)
	(message "Invalid property type selected. You must enter 'o', 'd', or 'a'.")))))

; object property
(defun owl-create-object-property (prefix property domain range lexical superproperty)
  "Creates an object property."
  (interactive "sPrefix: \nsProperty Name (e.g. 'hasPart', 'performsActFreely'): \nsDomain (defaults to owl:Thing if left blank): \nsRange (defaults to owl:Thing if left blank): \nsEnglish Translation (e.g. :father may be 'is the father of'): \nsSubproperty of (leave blank if none known): ")
  (let*
      ((default-domain "owl:Thing")
       (domain (return-default-if-empty domain default-domain))
       (default-range "owl:Thing")
       (range (return-default-if-empty range default-range)))
    (insert prefix ":" property " a owl:ObjectProperty ;
	rdfs:label \""property) (camelcase-to-sentence-case) (insert "\"^^xsd:string ;
        skos:prefLabel \""lexical"\"@en ;
	") (if (equal superproperty "") nil (insert "rdfs:subPropertyOf "superproperty" ;
        ")) (insert  "rdfs:domain "domain" ;
        rdfs:range "range" ;
	rdfs:comment \"\"\"(:"(delete-prefix domain)"1 "prefix":"property" :"(delete-prefix range)"1) means that "(delete-prefix domain)"1 "lexical" "(delete-prefix range)"1. For example, (: "prefix": :).\"\"\"^^xsd:string ;
        olive:exampleTriple \"\";
.")
    (search-backward "(")
    (forward-char 2)
    (message (concat "Created " prefix ":" property "! Create your example, and add it to the example triple below."))))

; datatype property
(defun owl-create-datatype-property (prefix property domain range lexical superproperty)
  "Creates a datatype property."
  (interactive "sPrefix: \nsProperty Name (e.g. 'hasLabel', 'numberOfBedrooms'): \nsDomain (defaults to owl:Thing if left blank): \nsRange (defaults to xsd:string if left blank): \nsEnglish Translation (e.g. :name may be 'is named'): \nsSubproperty of (leave blank if none known): ")
  (let*
      ((default-domain "owl:Thing")
       (domain (return-default-if-empty domain default-domain))
       (default-range "xsd:string")
       (range (return-default-if-empty range default-range)))
    (insert prefix ":" property " a owl:DatatypeProperty ;
	rdfs:label \""property) (camelcase-to-sentence-case) (insert "\"^^xsd:string ;
        skos:prefLabel \""lexical"\"@en ;
	") (if (equal superproperty "") nil (insert "rdfs:subPropertyOf "superproperty" ;
        ")) (insert  "rdfs:domain "domain" ;
        rdfs:range "range" ;
	rdfs:comment \"\"\"(:"(delete-prefix domain)"1 "prefix":"property" \\\""(upcase (delete-prefix range))"\\\""range") means that "(delete-prefix domain)"1 "lexical" "(upcase (delete-prefix range))". For example, (: "prefix": \"\"^^"range").\"\"\"^^xsd:string ;
        olive:exampleTriple \"\";
.")
    (search-backward "(")
    (forward-char 2)
    (message (concat "Created " prefix ":" property "! Create your example, and add it to the example triple below."))))

; annotation property
(defun owl-create-annotation-property (prefix property domain range lexical superproperty)
  (interactive "sPrefix: \nsProperty Name (e.g. 'isOfType', 'exampleValue'): \nsDomain (defaults to owl:Thing if left blank): \nsRange (defaults to owl:Thing if left blank): \nsEnglish Translation (e.g. :exampleTriple may be 'can produce triples like the following:'): \nsSubproperty of (leave blank if none known): ")
  (let*
      ((default-domain "owl:Thing")
       (domain (return-default-if-empty domain default-domain))
       (default-range "owl:Thing")
       (range (return-default-if-empty range default-range)))
    (insert prefix ":" property " a owl:AnnotationProperty ;
	rdfs:label \""property) (camelcase-to-sentence-case) (insert "\"^^xsd:string ;
        skos:prefLabel \""lexical"\"@en ;
	") (if (equal superproperty "") nil (insert "rdfs:subPropertyOf "superproperty" ;
        ")) (insert  "rdfs:domain "domain" ;
        rdfs:range "range" ;
	rdfs:comment \"\"\"(:"(delete-prefix domain)"1 "prefix":"property" :"(delete-prefix range)"1) means that "(delete-prefix domain)"1 "lexical" "(delete-prefix range)"1. For example, (: "prefix": :).\"\"\"^^xsd:string ;
        olive:exampleTriple \"\";
.")
    (search-backward "(")
    (forward-char 2)
    (message (concat "Created " prefix ":" property "! Create your example, and add it to the example triple below."))))


;; create individual (see create instance, below)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Derivative Resource Creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; create subclass


;; create instance


;; create subproperty





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Rule Creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Property Path


;; Stardog Syntax


;; SWRL Syntax


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
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (replace-regexp "\\([A-Z]\\)" " \\1" nil
                      (1+ (car bounds)) (cdr bounds))
      (downcase-region (car bounds) (cdr bounds)))))

(defun delete-prefix (prefixed-term)
  "take a term with a prefix, e.g. owl:Thing, and then return it without the prefix, e.g. Thing."
  (replace-regexp-in-string "\\S-*?:" "" prefixed-term))


(defun return-default-if-empty (string default)
  "This function returns the string value if it's non-empty, otherwise it returns the default."
  (if
      (equal string "")
      (symbol-value 'default)
    (symbol-value 'string)))
    
