#!chezscheme
(library (struct struct)
    (export define-type
            type-sizeof)
    (import (chezscheme))

(define name->offset)
(define name->size)

;;native or normal calls?
(meta define type->setcall 
    (lambda (type k)
        (cond 
            ((eq? type 'u8)
            #'bytevector-u8-set!)
            ((eq? type 's8)
            #'bytevector-s8-set!)
            (else 
            (datum->syntax k
                (string->symbol
                    (string-append "bytevector-" (symbol->string type) "-set!")))))))

;;native or normal calls?
(meta define type->getcall 
    (lambda (type k)
        (cond 
            ((eq? type 'u8)
            #'bytevector-u8-ref)
            ((eq? type 's8)
            #'bytevector-s8-ref)
            (else 
            (datum->syntax k
                (string->symbol
                    (string-append "bytevector-" (symbol->string type) "-ref")))))))

(meta define type->size
    (lambda (count tree type)
      (syntax-case type ()
        [((struct name (t n r ...) rest ...) more ...)
         (eq? (syntax->datum #'struct) 'struct)
         (let* ([p (type->size count (list #'name) #'((t n r ...) rest ...))]
                [size (car p)]
                [new-tree (cdr p)])
                (if (null? (syntax->datum #'(more ...)))
                    (cons size (append tree (list new-tree)))
                    (type->size size (append tree (list new-tree)) #'(more ...))))]
        [((t n r ...) rest ...)
         (if (null? (syntax->datum #'(rest ...)))
            (cons
                (fx+ count 
                     (string->number (list->string (cdr (string->list (symbol->string (syntax->datum #'t)))))))
                (append tree (list (list #'n (fx/ count 8) #'t))))
            (type->size 
                (fx+ count
                    (string->number (list->string (cdr (string->list (symbol->string (syntax->datum #'t)))))))
                (append tree (list (list #'n (fx/ count 8) #'t)))
                #'(rest ...)))])))

(define-syntax type-sizeof
  (lambda (x)
    (lambda (r)
      (syntax-case x ()
        [(_ id)
         #`'#,(datum->syntax #'* (r #'id #'name->size))]))))

(meta define field-details
    (lambda (tree path)
      (let* ([k (car path)]
             [child (filter (lambda (p) (eq? (car p) k)) tree)]) ;;can error check if child is length 1
        (if (null? (cdr path))
            (cdar child)
            (field-details (cdar child) (cdr path))))))

(define-syntax define-type
    (lambda (stx)
        (syntax-case stx ()
        [(k (t name r ...))
         (let* ([walk (type->size 0 '() #'((t name r ...)))]
                [size (fx/ (car walk) 8)]
                [paths (cdadr walk)])
               #`(begin
                    (define-syntax name
                        (lambda (stx)
                            (syntax-violation 'name "invalid use of keyword" stx)))
                    (define-property name name->offset (syntax->datum (syntax #,paths))) ;;hack to fix raw symbol output error for now
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
                                        [p (syntax->datum #'path)]
                                        [details (field-details tree p)]
                                        [offset (car details)]                                        
                                        [get-call (type->getcall (cadr details) #'k)])
                                       #`(#,get-call instance #,offset (native-endianness)))]))))
                                       
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
                                        [p (syntax->datum #'path)]
                                        [details (field-details tree p)]
                                        [offset (car details)]                                        
                                        [set-call (type->setcall (cadr details) #'k)])
                                       #`(#,set-call instance #,offset value (native-endianness)))]))))))]))))

#|
(mys-get (in y) s)

(define-type 
      (struct mys 
          (u8 x)
          (struct in (u8 y) (u16 t))
          (u16 z)
          (struct anotha (u32 zz))))))

((x 0) (in (y 8) (z 8)))

(define-syntax #,(datum->syntax #'k
                    (string->symbol 
                        (string-append 
                            (symbol->string 
                                (syntax->datum #'name))
                            "-size")))
                (lambda (_) #',size))

(define-syntax get-property 
 (lambda (x)
    (lambda (r)
      (syntax-case x ()
        [(_ id key)
         #`'#,(datum->syntax #'* (r #'id #'key))]))))
|#