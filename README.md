# WIP: An attempt at a native implementation of scrypt in scala

Based on the spec at http://tools.ietf.org/html/draft-josefsson-scrypt-kdf-01

Note: Implementing any hashing algorithm in Scala is a dumb idea. This is
because scala is very slow. To get the fastest implementation usable in
your scala project, you'll want to use the java implementation: 
https://github.com/wg/scrypt

