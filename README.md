# STRUCT
implements structs in bytevectors. Currently handles uints/ints/chars/bools/float32/float64 with structs/unions/arrays.

## Example

A complex type to show nesting works etc...
```scheme
> (import (struct struct))
> (define-type
    (struct a
      (array b 3
        (struct c
          (u8 f) 
          (array inner 2
            (union z 
              (u32 a) 
              (u16 b)))))
      (array d 1 (u16 e))))
```

Can get the size with this macro
```scheme
> (type-sizeof a)
29
```

A helper to make bytevectors with the appropriate size
```scheme
> (define v (a-make 2))
> v
#vu8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
     0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)

```

getters and setters...
```scheme
> (a-set! (b 2 f) v 5)
> (a-get (b 2 f) v)
5
> ;; optional index parameter (default is 0) to select from the multiple elements in the one bytevector
  ;; uses TOTAL byte index for now, to make looping slightly faster by avoiding uneeded multiplications
  (a-get (fx* (type-sizeof a) 1) (b 2 f) v)
0
```

nested arrays and unions of course work here
```scheme
  (a-set! (b 1 inner 0 a) v (fx1- (expt 2 32)))
> (a-get (b 1 inner 0 a) v)
4294967295
> (a-set! (b 1 inner 0 b) v 0)
> (a-get (b 1 inner 0 a) v)
4294901760
```

you can also refer to other types
```scheme
> (define-type (struct circle (u32 radius)))
> (define-type (struct triangle (u32 height) (u32 base)))
> (define-type
    (struct shape
      (u8 type)
      (union shapes circle triangle)))

> (define v (shape-make 1))
> (shape-set! (shapes triangle height) v (fx1- (expt 2 32)))
```
