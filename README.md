PuddingX
========

A forth like language.

Samples
=======

fizzbuzz
--------

```
: succ 1 + ;
: 2drop drop drop ;
: mod=0 mod 0 == ;
: fb' dup 15 mod=0 if "fizzbuzz" . else dup 5 mod=0 if "buzz" . else dup 3 mod=0 if "fizz" . else dup . then then then ;
: fb ;
: fb 2dup >= if fb' succ fb else 2drop then ;
100 1 fb
```
