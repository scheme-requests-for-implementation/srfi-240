#!r6rs

;; Copyright (C) Marc Nieper-Wißkirchen (2022).  All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(library (srfi :237 records)
  (export define-record-type
	  fields
	  mutable
	  immutable
	  parent
	  protocol
	  sealed
	  opaque
	  nongenerative
	  parent-rtd
	  record-type-descriptor
	  record-constructor-descriptor
	  make-record-type-descriptor
	  record-type-descriptor?
	  make-record-descriptor
	  make-record-constructor-descriptor
	  record-descriptor-rtd
	  record-descriptor?
	  record-constructor-descriptor?
	  record-constructor
	  record-predicate
	  record-accessor
	  record-mutator
	  record?
	  record-rtd
	  record-type-name
	  record-type-parent
	  record-type-uid
	  record-type-generative?
	  record-type-sealed?
	  record-type-opaque?
	  record-type-field-names
	  record-field-mutable?)
  (import (rnrs base (6))
	  (rnrs syntax-case (6))
	  (rnrs lists (6))
	  (rnrs control (6))
	  (only (rnrs records syntactic (6))
		fields
		mutable
		immutable
		parent
		protocol
		sealed
		opaque
		nongenerative
		parent-rtd)
	  (prefix (rnrs (6)) rnrs:)
	  (srfi :213))

  (rnrs:define-record-type rd
    (nongenerative) (opaque #t) (sealed #t)
    (fields rtd cd))

  (define record-type-descriptor?
    (lambda (obj)
      (or (rnrs:record-type-descriptor? obj)
	  (rd? obj))))

  (define record-descriptor?
    (lambda (obj)
      (rd? obj)))

  (define record-constructor-descriptor?
    (lambda (obj)
      (record-descriptor? obj)))

  (define rnrs:rtd
    (lambda (rtd)
      (unless (record-type-descriptor? rtd)
	(assertion-violation #f "not a record-type descriptor" rtd))
      (if (rnrs:record-type-descriptor? rtd)
	  rtd
	  (rd-rtd rtd))))

  (define rnrs:cd
    (lambda (rd)
      (unless (record-descriptor? rd)
	(assertion-violation #f "not a record descriptor" rd))
      (rd-cd rd)))

  (define make-record-type-descriptor
    (lambda (name parent uid sealed? opaque? fields)
      (rnrs:make-record-type-descriptor name (rnrs:rtd parent) uid sealed? opaque? fields)))

  (define make-record-descriptor
    (case-lambda
      [(rtd parent-descriptor protocol)
       (make-rd (rnrs:rtd rtd)
		(rnrs:make-record-constructor-descriptor (rnrs:rtd rtd) (rnrs:cd parent-descriptor) protocol))]
      [(name parent uid sealed? opaque? fields protocol)
       (make-record-descriptor (make-record-type-descriptor name parent uid sealed? opaque? fields)
			       parent protocol)]))

  (define make-record-constructor-descriptor
    (lambda (rtd parent-descriptor protocol)
      (make-record-descriptor rtd parent-descriptor protocol)))

  (define record-descriptor-rtd
    (lambda (rd)
      (define who 'record-descriptor-rtd)
      (unless (record-descriptor? rd)
	(assertion-violation who "not a record descriptor" rd))
      (rd-rtd rd)))

  (define record-constructor
    (lambda (rd)
      (define who 'record-constructor)
      (unless (record-descriptor? rd)
	(assertion-violation who "not a record descriptor" rd))
      (rnrs:record-constructor (rd-cd rd))))

  (define record-predicate
    (lambda (rtd)
      (rnrs:record-predicate (rnrs:rtd rtd))))

  (define record-accessor
    (lambda (rtd k)
      (rnrs:record-accessor (rnrs:rtd rtd) k)))

  (define record-mutator
    (lambda (rtd k)
      (rnrs:record-accessor (rnrs:rtd rtd) k)))

  (define record?
    (lambda (obj)
      (rnrs:record? obj)))

  (define record-rtd
    (lambda (record)
      (rnrs:record-rtd record)))

  (define record-type-name
    (lambda (rtd)
      (rnrs:record-type-name (rnrs:rtd rtd))))

  (define record-type-parent
    (lambda (rtd)
      (rnrs:record-type-parent (rnrs:rtd rtd))))

  (define record-type-uid
    (lambda (rtd)
      (rnrs:record-type-uid (rnrs:rtd rtd))))

  (define record-type-generative?
    (lambda (rtd)
      (rnrs:record-type-generative? (rnrs:rtd rtd))))

  (define record-type-sealed?
    (lambda (rtd)
      (rnrs:record-type-sealed? (rnrs:rtd rtd))))

  (define record-type-opaque?
    (lambda (rtd)
      (rnrs:record-type-opaque? (rnrs:rtd rtd))))

  (define record-type-field-names
    (lambda (rtd)
      (rnrs:record-type-field-names (rnrs:rtd rtd))))

  (define record-field-mutable?
    (lambda (rtd k)
      (rnrs:record-field-mutable? (rnrs:rtd rtd) k)))

  (define-syntax define-record-type
    (lambda (stx)
      (define who 'define-record-type)

      (define distinct-identifiers?
        (lambda (id*)
          (let f ((id* id*))
            (or (null? id*)
                (and (not (exists
                           (lambda (id)
                             (bound-identifier=? (car id*) id))
                           (cdr id*)))
                     (f (cdr id*)))))))
      (define gen-field-spec
        (lambda (field)
          (syntax-case field ()
            [(field-name accessor-name)
             #'(immutable field-name accessor-name)]
            [(field-name accessor-name mutator-name)
             #'(mutable field-name accessor-name mutator-name)]
            [_
             (syntax-violation who
                               "invalid field spec"
                               stx
                               field)])))
      (define update-name-spec
	(lambda (k spec)
	  (syntax-case spec ()
	    [name
	     (identifier? #'name)
	     (let ([name (syntax->datum #'name)])
	       (list spec
		     (list (datum->syntax #'* name)
			   (datum->syntax k (string->symbol (string-append "make-" (symbol->string name))))
			   (datum->syntax k (string->symbol (string-append (symbol->string name) "?"))))))]
	    [(name constructor-name predicate-name)
	     (and (identifier? #'name)
		  (identifier? #'constructor-name)
		  (identifier? #'predicate-name))
	     (list #'name
		   (list (datum->syntax #'* (syntax->datum #'name))
			 #'constructor-name
			 #'predicate-name))]
	    [_
	     (syntax-violation who "invalid name spec" stx spec)])))
      (lambda (lookup)
	(define update-record-clause
	  (lambda (k prefix clause)
	    (define update-field-spec
	      (lambda (field-spec)
		(syntax-case field-spec (immutable mutable)
		  [(immutable field-name)
		   (identifier? #'field-name)
		   (let ([name (symbol->string (syntax->datum #'field-name))])
		     (with-syntax ([accessor-name
				    (datum->syntax k (string->symbol (string-append prefix name)))])
		       #'(immutable field-name accessor-name)))]
		  [(mutable field-name)
		   (identifier? #'field-name)
		   (let ([name (symbol->string (syntax->datum #'field-name))])
		     (with-syntax ([accessor-name
				    (datum->syntax k (string->symbol (string-append prefix "-" name)))]
				   [mutator-name
				    (datum->syntax k (string->symbol (string-append prefix "-" name "-set!")))])
		       #'(mutable field-name accessor-name mutator-name)))]
		  [field-name
		   (identifier? #'field-name)
		   (let ([name (symbol->string (syntax->datum #'field-name))])
		     (with-syntax ([accessor-name
				    (datum->syntax k (string->symbol (string-append prefix "-" name)))])
		       #'(immutable field-name accessor-name)))]
		  [_ field-spec])))
	    (syntax-case clause (fields parent parent-rtd)
	      [(fields field-spec ...)
	       (with-syntax ([(field-spec ...) (map update-field-spec #'(field-spec ...))])
		 #'(fields field-spec ...))]
	      [(parent name)
	       (identifier? #'name)
	       (cond
		[(lookup #'name #'rnrs:record-name) =>
		 (lambda (record-name)
		   (with-syntax ([record-name record-name])
		     #'(parent record-name)))]
		[else
		 #'(parent-rtd (rnrs:rtd name) (rnrs:cd name))])]
	      [(parent-rtd rtd-expr cd-expr)
	       #'(parent-rtd (rnrs:rtd rtd-expr) (rnrs:cd cd-expr))]
	      [_
	       clause])))
	(syntax-case stx ()
          [(_ name (constructor-name field-name ...) pred field ...)
           (and (identifier? #'name)
		(identifier? #'constructor-name)
		(for-all identifier? #'(field-name ...))
		(identifier? #'pred)
		(distinct-identifiers? #'(field-name ...)))
           (let* ([name* #'(field-name ...)]
                  [tmp* (generate-temporaries name*)]
                  [field-spec* (map gen-field-spec #'(field ...))])
             (define gen-init
               (lambda (spec)
		 (or (exists
                      (lambda (name tmp)
			(and (bound-identifier=? name (cadr spec))
                             tmp))
                      name* tmp*)
                     #'#f)))
             (for-each
              (lambda (name)
		(unless (exists
			 (lambda (spec)
                           (bound-identifier=? name (cadr spec)))
			 field-spec*)
                  (syntax-violation who
                                    "undefined field name"
                                    stx
                                    name)))
              name*)
             (unless (distinct-identifiers? (map cadr field-spec*))
               (syntax-violation who
				 "multiple field specs with the same name"
				 stx))
             (with-syntax ([(tmp ...) tmp*]
                           [(init ...) (map gen-init field-spec*)]
                           [(spec ...) field-spec*])
               #'(define-record-type (name constructor-name pred)
                   (fields spec ...)
                   (protocol
                    (lambda (p)
                      (lambda (tmp ...)
			(p init ...)))))))]
          [(k name-spec record-clause ...)
	   (with-syntax ([(record-name name-spec) (update-name-spec #'k #'name-spec)])
	     (define prefix (symbol->string (syntax->datum #'record-name)))
	     (with-syntax ([(record-clause ...) (map (lambda (clause)
						       (update-record-clause #'k prefix clause))
						     #'(record-clause ...))])
	       #'(define-record-type-aux record-name name-spec record-clause ...)))]
          [_
           (syntax-violation who "invalid record-type definition" stx)]))))

  (define rnrs:record-name)

  (define-syntax define-record-type-aux
    (syntax-rules ()
      [(_ record-name (tmp-name constructor-name predicate-name) record-clause ...)
       (begin
	 (rnrs:define-record-type (tmp-name constructor-name predicate-name) record-clause ...)
	 (define rtd (rnrs:record-type-descriptor tmp-name))
	 (define cd (rnrs:record-constructor-descriptor tmp-name))
	 (define rd (make-rd rtd cd))
	 (define-syntax record-name
	   (lambda (stx)
	     (syntax-case stx ()
	       [k
		(identifier? #'k)
		#'rd]
	       [_
		(syntax-violation 'record-name "invalid use of record name" stx)])))
	 (define-property record-name rnrs:record-name #'tmp-name))]))

  (define-syntax record-type-descriptor
    (lambda (stx)
      (define who 'record-type-descriptor)
      (syntax-case stx ()
	[(_ record-name)
	 (identifier? #'record-name)
	 #'(record-descriptor-rtd record-name)]
	[_
	 (syntax-violation who "invalid syntax" stx)])))

  (define-syntax record-constructor-descriptor
    (lambda (stx)
      (define who 'record-constructor-descriptor)
      (syntax-case stx ()
	[(_ record-name)
	 (identifier? #'record-name)
	 #'(values record-name)]
	[_
	 (syntax-violation who "invalid syntax" stx)])))

  )


;; Local Variables:
;; mode: scheme
;; End:
