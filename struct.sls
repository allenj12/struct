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
            #'(bytevector-u8-set!))
            ((eq? type 's8)
            #'(bytevector-s8-set!))
            (else 
            (datum->syntax k
                (list 
                    (string->symbol
                        (string-append "bytevector-" (symbol->string type) "-set!"))
                '(native-endianness)))))))

;;native or normal calls?
(meta define type->getcall 
    (lambda (type k)
        (cond 
            ((eq? type 'u8)
            #'(bytevector-u8-ref))
            ((eq? type 's8)
            #'(bytevector-s8-ref))
            (else 
            (datum->syntax k
                (list 
                    (string->symbol
                        (string-append "bytevector-" (symbol->string type) "-ref"))
                '(native-endianness)))))))

(meta define struct-offsets
    (lambda (c t)
      (if (null? t)
          '()
          (let ([n (car t)]) 
            (cons (list (car n) c (caddr n)) (struct-offsets (fx+ c (cadr n)) (cdr t)))))))

(meta define union-offsets
    (lambda (t)
        (map (lambda (n) (list (car n) 0 (caddr n))) t)))

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
        [((t n r ...) rest ...)
         (if (null? (syntax->datum #'(rest ...)))
                (append tree (list (list #'n (fx/ (string->number (list->string (cdr (string->list (symbol->string (syntax->datum #'t)))))) 8) #'t)))
            (type->size
                (append tree (list (list #'n (fx/ (string->number (list->string (cdr (string->list (symbol->string (syntax->datum #'t)))))) 8) #'t)))
                #'(rest ...)))])))

(define-syntax type-sizeof
  (lambda (x)
    (lambda (r)
      (syntax-case x ()
        [(_ id)
         #`'#,(datum->syntax #'* (r #'id #'name->size))]))))

(meta define field-details
      (lambda (tree path)
        (let* ([k (car path)]                              ;;cdr to skip struct size
               [child (assq k tree)]) ;;can error check if child is length 1
          (if (null? (cdr path))
              (cdr child)
              (field-details (caddr child) (cdr path))))))

(meta define sum-paths
    (lambda (sum tree)
      (if (symbol? tree)
          tree
          (fold-right cons '() 
            (map (lambda (n) 
                   (let ([new-offset (fx+ (cadr n) sum)])
                     (list (car n) new-offset (sum-paths new-offset (caddr n))))) tree)))))

(define-syntax define-type
    (lambda (stx)
        (syntax-case stx ()
        [(k (t name r ...))
         (let* ([walk (type->size '() #'((t name r ...)))]
                [size (cadar walk)]
                [paths (caddar walk)])
               #`(begin
                    (define-syntax name
                        (lambda (stx)
                            (syntax-violation 'name "invalid use of keyword" stx)))
                    (define-property name name->offset (sum-paths 0 (syntax->datum (syntax #,paths)))) ;;hack to fix raw symbol output error for now
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
                                        [offset (car details)])
                                        (with-syntax ([get-call (type->getcall (cadr details) #'k)])
                                            (syntax-case #'get-call ()
                                            [(call rest)
                                             #`(call instance #,offset rest)]
                                            [(call)
                                             #`(call instance #,offset)])))]
                                [(_ idx path instance)
                                 (let* ([tree (lookup #'name #'name->offset)]
                                        [p (syntax->datum #'path)]
                                        [details (field-details tree p)]
                                        [offset (car details)])
                                        (with-syntax ([get-call (type->getcall (cadr details) #'k)])
                                            (syntax-case #'get-call ()
                                            [(call rest)
                                             (if (= offset 0)
                                                #`(call instance idx rest)
                                                #`(call instance (fx+ idx #,offset) rest))]
                                            [(call)
                                             (if (= offset 0)
                                                #`(call instance idx)
                                                #`(call instance (fx+ idx #,offset)))])))]))))
                                       
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
                                        [offset (car details)])
                                        (with-syntax ([set-call (type->setcall (cadr details) #'k)])
                                            (syntax-case #'set-call ()
                                            [(call rest)
                                             #`(call instance #,offset value rest)]
                                            [(call)
                                             #`(call instance #,offset value)])))]
                                [(_ idx path instance value)
                                 (let* ([tree (lookup #'name #'name->offset)]
                                        [p (syntax->datum #'path)]
                                        [details (field-details tree p)]
                                        [offset (car details)])
                                        (with-syntax ([set-call (type->setcall (cadr details) #'k)])
                                            (syntax-case #'set-call ()
                                            [(call rest)
                                            (if (= offset 0)
                                                #`(call instance idx value rest)
                                                #`(call instance (fx+ idx #,offset) value rest))]
                                            [(call)
                                             (if (= offset 0)
                                                #`(call instance idx value)
                                                #`(call instance (fx+ idx #,offset) value))])))]))))
                                             
                    (define-syntax #,(datum->syntax #'k
                        (string->symbol 
                            (string-append 
                                (symbol->string 
                                    (syntax->datum #'name))
                                    "-make")))
                        (lambda (stx)
                            (syntax-case stx ()
                            [(_ n)
                            #`(make-bytevector (fx* n #,#,size) 0)])))))]))))

#|
(define-type 
      (struct mys 
          (u8 x)
          (struct in (u8 y) (u16 t))
          (u16 z)
          (struct anotha (u32 zz))))))

(define-syntax get-property 
 (lambda (x)
    (lambda (r)
      (syntax-case x ()
        [(_ id key)
         #`'#,(datum->syntax #'* (r #'id #'key))]))))
|#