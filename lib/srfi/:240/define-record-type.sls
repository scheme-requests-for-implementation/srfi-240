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

(library (srfi :240 define-record-type)
  (export define-record-type
	  fields
	  mutable
	  immutable
	  parent
	  protocol
	  sealed
	  opaque
	  nongenerative
          generative
	  parent-rtd)
  (import (rnrs base (6))
	  (rnrs syntax-case (6))
	  (rnrs lists (6))
	  (rnrs control (6))
	  (prefix (rnrs (6)) rnrs:)
	  (rename (srfi :237)
		  (define-record-type :237:define-record-type)))

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
		     spec
		     (datum->syntax k (string->symbol (string-append "make-" (symbol->string name))))
		     (datum->syntax k (string->symbol (string-append (symbol->string name) "?")))))]
            [(rtd-name record-name)
	     (and (identifier? #'rtd-name)
                  (identifier? #'record-name))
	     (let ([name (syntax->datum #'rtd-name)])
	       (list #'rtd-name
		     #'record-name
		     (datum->syntax k (string->symbol (string-append "make-" (symbol->string name))))
		     (datum->syntax k (string->symbol (string-append (symbol->string name) "?")))))]
	    [(name constructor-name predicate-name)
	     (and (identifier? #'name)
		  (identifier? #'constructor-name)
		  (identifier? #'predicate-name))
	     (list #'name
		   #'name
		   #'constructor-name
		   #'predicate-name)]
            [(rtd-name record-name constructor-name predicate-name)
             (and (identifier? #'rtd-name)
                  (identifier? #'record-name)
                  (identifier? #'constructor-name)
                  (identifier? #'predicate-name))
             (list #'rtd-name
		   #'record-name
                   #'constructor-name
                   #'predicate-name)]
	    [_
	     (syntax-violation who "invalid name spec" stx spec)])))
      (define update-record-clauses
	(lambda (k prefix clauses)
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
          (let f ([clauses clauses] [transformed '()] [gen #f])
            (if (null? clauses)
                transformed
                (let ([clause (car clauses)] [clauses (cdr clauses)])
                  (define g
                    (lambda (clause)
                      (f clauses (cons clause transformed) gen)))
                  (syntax-case clause (fields parent parent-rtd nongenerative generative)
	            [(fields field-spec ...)
	             (with-syntax ([(field-spec ...) (map update-field-spec #'(field-spec ...))])
		       (g #'((fields field-spec ...))))]
	            [clause
	             (g #'(clause))]))))))
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
	 (with-syntax ([(record-name . name-spec) (update-name-spec #'k #'name-spec)])
	   (define prefix (symbol->string (syntax->datum #'record-name)))
	   (with-syntax ([(record-clause ...)
                          (update-record-clauses #'k prefix #'(record-clause ...))])
	     #'(:237:define-record-type (record-name . name-spec) record-clause ...)))]
        [_
         (syntax-violation who "invalid record-type definition" stx)])))

  )


;; Local Variables:
;; mode: scheme
;; End:
