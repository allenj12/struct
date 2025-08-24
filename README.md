# STRUCT
implements structs in bytevectors, still early in development and only handles structs and unions (no arrays yet).

## Example

```
(import (struct struct))
(define-type 
    (struct mys 
        (u8 a)
        (struct in (u8 b) (u16 c))
        (u16 d)
        (struct inner' (u32 e))))

(define v (make-bytevector (type-sizeof mys) 0))

(mys-set! (in b) v 3)

(mys-get (in b) v) => 3
```
