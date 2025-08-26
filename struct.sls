#!chezscheme
(library (struct struct)
    (export define-type
            type-sizeof)
    (import (chezscheme))

(define name->offset)
(define name->size)

;;native or normal calls?
(meta define type->setcall 
    (lambda (type instance offset value k)
        (case type
            ('u8
            #`(bytevector-u8-set! #,instance #,offset #,value))
            ('s8
            #`(bytevector-s8-set! #,instance #,offset #,value))
            ('f32
             #`(bytevector-ieee-single-set! #,instance #,offset #,value (native-endianness)))
            ('f64
             #`(bytevector-ieee-double-set! #,instance #,offset #,value (native-endianness)))
            ('char 
             #`(bytevector-u8-set! #,instance #,offset (char->integer #,value)))
            ('bool 
             #`(bytevector-u8-set! #,instance #,offset (if #,value 1 0)))
            (else 
                #`(#,(datum->syntax k
                        (string->symbol
                            (string-append "bytevector-" (symbol->string type) "-set!")))
                    #,instance
                    #,offset
                    #,value
                    (native-endianness))))))

(meta define type->getcall 
    (lambda (type instance offset k)
        (case type
            ('u8
            #`(bytevector-u8-ref #,instance #,offset))
            ('s8
            #`(bytevector-s8-ref #,instance #,offset))
            ('f32
             #`(bytevector-ieee-single-ref #,instance #,offset (native-endianness)))
            ('f64
             #`(bytevector-ieee-double-ref #,instance #,offset (native-endianness)))
            ('char 
             #`(integer->char (bytevector-u8-ref #,instance #,offset)))
            ('bool 
             #`(if (not (= 0 (bytevector-u8-ref #,instance #,offset))) #t #f))
            (else 
                #`(#,(datum->syntax k
                        (string->symbol
                            (string-append "bytevector-" (symbol->string type) "-ref")))
                    #,instance
                    #,offset
                    (native-endianness))))))

(meta define struct-offsets
    (lambda (c t)
      (if (null? t)
          '()
          (let ([n (car t)])
            (cons (list (car n) c (caddr n)) (struct-offsets (fx+ c (cadr n)) (cdr t)))))))

(meta define union-offsets
    (lambda (t)
        (map (lambda (n) (list (car n) 0 (caddr n))) t)))

(meta define array-offsets
    (lambda (t-size t)
        (map (lambda (n) (list (car n) (list t-size) (caddr n) #'array)) t)))

(meta define type->size
    (lambda (tree type)
      (syntax-case type ()
        [((struct name (t n r ...) rest ...) more ...)
         (eq? (syntax->datum #'struct) 'struct)
            (let* ([pre-tree (type->size '() #'((t n r ...) rest ...))]
                    [size (apply fx+ (map cadr pre-tree))]
                    [new-tree (append (list #'name size) (list (struct-offsets 0 pre-tree)))])
                    (if (null? (syntax->datum #'(more ...)))
                        (append tree (list new-tree))
                        (type->size (append tree (list new-tree)) #'(more ...))))]
        [((union name (t n r ...) rest ...) more ...)
         (eq? (syntax->datum #'union) 'union)
            (let* ([pre-tree (type->size '() #'((t n r ...) rest ...))]
                    [size (apply fxmax (map cadr pre-tree))]
                    [new-tree (append (list #'name size) (list (union-offsets pre-tree)))])
                    (if (null? (syntax->datum #'(more ...)))
                        (append tree (list new-tree))
                        (type->size (append tree (list new-tree)) #'(more ...))))]
        [((array name arr-size (t n r ...)) more ...)
         (eq? (syntax->datum #'array) 'array)
         (let* ([pre-tree (type->size '() #'((t n r ...)))]
                [t-size (cadar pre-tree)]
                [size (fx* t-size (syntax->datum #'arr-size))]
                [new-tree (append (list #'name size) (list (array-offsets t-size pre-tree)))])
                (if (null? (syntax->datum #'(more ...)))
                    (append tree (list new-tree))
                    (type->size (append tree (list new-tree)) #'(more ...))))]
        [((t n r ...) rest ...)
         (if (null? (syntax->datum #'(rest ...)))
                (let* ([sym (syntax->datum #'t)]
                       [s (if (or (eq? sym 'bool) (eq? sym 'char))
                              1
                              (fx/ (string->number (list->string (cdr (string->list (symbol->string sym))))) 8))])
                    (append tree (list (list #'n s #'t))))
                 (let* ([sym (syntax->datum #'t)]
                        [s (if (or (eq? sym 'bool) (eq? sym 'char))
                              1
                              (fx/ (string->number (list->string (cdr (string->list (symbol->string sym))))) 8))])
                    (type->size
                        (append tree (list (list #'n s #'t)))
                        #'(rest ...))))])))

(define-syntax type-sizeof
  (lambda (x)
    (lambda (r)
      (syntax-case x ()
        [(_ id)
         #`'#,(datum->syntax #'* (r #'id #'name->size))]))))
                
(meta define field-details
      (lambda (tree p idxs)
        (syntax-case p ()
        [(k rest ...)
        (let* ([array-idx? (or (number? (syntax->datum #'k)) (= (length (car tree)) 4))]
               [child (if array-idx?
                          (car tree)
                          (assq (syntax->datum #'k) tree))]
               [new-idxs (if array-idx?
                            (append idxs (list #'k))
                            idxs)])
          (if (null? #'(rest ...))
              (cons (cdr child) new-idxs)
              (field-details (caddr child) #'(rest ...) new-idxs)))])))

(meta define sum-paths
    (lambda (sum tree)
      (if (symbol? tree)
          tree
          (fold-right cons '() 
            (map (lambda (n) 
                    (let ([offset (cadr n)])
                        (cond 
                            ((and (number? offset) (number? sum))
                             (let ([new-offset (fx+ offset sum)])
                                (append 
                                    (list (car n) new-offset (sum-paths new-offset (caddr n)))
                                    (cdddr n))))
                            
                            ((and (list? offset) (list? sum)) ;'(some-size) & '((sizes ...) rel-offsets ...)
                             (let ([new-offset (cons (append (car sum) offset) (cdr sum))])
                                (append 
                                    (list (car n) new-offset (sum-paths new-offset (caddr n)))
                                    (cdddr n))))
                             
                            ((list? sum) ; 10 & '((sizes ...) rel-offsets ...)
                             (let ([new-offset (list (car sum) (fx+ offset (cadr sum)))])
                                (append 
                                   (list (car n) new-offset (sum-paths new-offset (caddr n)))
                                   (cdddr n))))
                                
                            (else ; '(some-size) & 10
                             (let ([new-offset (list offset sum)])
                                (append 
                                    (list (car n) new-offset (sum-paths new-offset (caddr n)))
                                    (cdddr n)))))))
                tree)))))

(define-syntax define-type
    (lambda (stx)
        (syntax-case stx ()
        [(k (t n r ...))
         (let* ([walk (type->size '() #'((t n r ...)))]
                [size (cadar walk)]
                [paths (caddar walk)])
                (with-syntax ([name (caar walk)])
               #`(begin
                    (define-syntax name
                        (lambda (stx)
                            (syntax-violation 'name "invalid use of keyword" stx)))
                    (define-property name name->offset (sum-paths 0 (syntax->datum (syntax #,paths)))) ;;hack to fix raw symbol output error for now, we can also get rid of this completely now
                    (define-property name name->size #,size)
                    
                    (define-syntax #,(datum->syntax #'k
                        (string->symbol 
                            (string-append 
                                (symbol->string 
                                    (syntax->datum #'name))
                                    "-get")))
                        (lambda (stx)
                            (lambda (lookup)
                                (syntax-case stx ()
                                [(_ path instance)
                                 (let* ([tree (lookup #'name #'name->offset)]
                                        [fd (field-details tree #'path '())]
                                        [details (car fd)]
                                        [array-idxs (cdr fd)]
                                        [offset (car details)])
                                        (with-syntax ([offset (if (number? offset) 
                                                                   offset
                                                                   (cons #'fx+
                                                                        (cons (cadr offset)
                                                                         (map (lambda (a b) #`(fx* #,a #,b))
                                                                              (car offset)
                                                                              array-idxs))))])
                                            (type->getcall (cadr details) #'instance #'offset #'k)))]
                                [(_ idx path instance)
                                 (let* ([tree (lookup #'name #'name->offset)]
                                        [fd (field-details tree #'path '())]
                                        [details (car fd)]
                                        [array-idxs (cdr fd)]
                                        [offset (car details)])
                                        (with-syntax ([offset (if (number? offset) 
                                                                   offset
                                                                   (cons #'fx+
                                                                        (cons (cadr offset)
                                                                         (map (lambda (a b) #`(fx* #,a #,b))
                                                                              (car offset)
                                                                              array-idxs))))])
                                            (type->getcall (cadr details) #'instance #'(fx+ idx offset) #'k)))]))))
                                       
                    (define-syntax #,(datum->syntax #'k
                        (string->symbol 
                            (string-append 
                                (symbol->string 
                                    (syntax->datum #'name))
                                    "-set!")))
                        (lambda (stx)
                            (lambda (lookup)
                                (syntax-case stx ()
                                [(_ path instance value)
                                 (let* ([tree (lookup #'name #'name->offset)]
                                        [fd (field-details tree #'path '())]
                                        [details (car fd)]
                                        [array-idxs (cdr fd)]
                                        [offset (car details)])
                                        (with-syntax ([offset (if (number? offset) 
                                                                   offset
                                                                   (cons #'fx+
                                                                        (cons (cadr offset)
                                                                         (map (lambda (a b) #`(fx* #,a #,b))
                                                                              (car offset)
                                                                              array-idxs))))])
                                             (type->setcall (cadr details) #'instance #'offset #'value #'k)))]
                                [(_ idx path instance value)
                                 (let* ([tree (lookup #'name #'name->offset)]
                                        [fd (field-details tree #'path '())]
                                        [details (car fd)]
                                        [array-idxs (cdr fd)]
                                        [offset (car details)])
                                        (with-syntax ([offset (if (number? offset) 
                                                                   offset
                                                                   (cons #'fx+
                                                                        (cons (cadr offset)
                                                                         (map (lambda (a b) #`(fx* #,a #,b))
                                                                              (car offset)
                                                                              array-idxs))))])
                                             (type->setcall (cadr details) #'instance #`(fx+ idx offset) #'value #'k)))]))))
                                             
                    (define-syntax #,(datum->syntax #'k
                        (string->symbol 
                            (string-append 
                                (symbol->string 
                                    (syntax->datum #'name))
                                    "-make")))
                        (lambda (stx)
                            (syntax-case stx ()
                            [(_ n)
                            #`(make-bytevector (fx* n #,#,size) 0)]))))))]))))

#|
(define-syntax get-property 
 (lambda (x)
    (lambda (r)
      (syntax-case x ()
        [(_ id key)
         #`'#,(datum->syntax #'* (r #'id #'key))]))))
|#