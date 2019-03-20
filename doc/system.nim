#
#
#            Nim's Runtime Library
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# ----------------------------------------------------------------------------
# ** NOTE **
# This file serves for creating system.html documentation.
# The original file is lib/system.nim.
# ----------------------------------------------------------------------------

##[
The compiler depends on the System module to work properly and the System
module depends on the compiler. Most of the routines listed here use
special compiler magic.

Each module implicitly imports the System module; it must not be listed
explicitly. Because of this there cannot be a user-defined module named
``system``.

System module
=============

System module imports several separate modules, and their documentation
is in separate files:
* `iterators <iterators.html>`_
* `assertions <assertions.html>`_
* `dollars <dollars.html>`_
* `io <io.html>`_
* `widestr <widestr.html>`_


Here is a short overview of the most commonly used functions from the
`system` module. Function names in the tables below are clickable and
will take you to the full documentation of the function.

The amount of available functions is much larger. Use the table of contents
on the left-hand side and/or `Ctrl+F` to navigate through this module.


Strings and characters
----------------------

=============================     =======================================
Proc                              Usage
=============================     =======================================
`len(s)<#len,string>`_            Return the length of a string
`chr(i)<#chr,range[]>`_           Convert an `int` in the range `0..255`
                                  to a character
`ord(c)<#ord,T>`_                 Return `int` value of a character
`a & b<#&,string,string>`_        Concatenate two strings
`$<dollars.html>`_                Convert various types to string
=============================     =======================================

**See also:**
* `strutils module<strutils.html>`_ for common string functions
* `strformat module<strformat.html>`_ for string interpolation and formatting
* `unicode module<unicode.html>`_ for Unicode UTF-8 handling
* `strscans<strscans.html>`_ for ``scanf`` and ``scanp`` macros, which offer
  easier substring extraction than regular expressions
* `strtabs module<strtabs.html>`_ for efficient hash tables
  (dictionaries, in some programming languages) mapping from strings to strings
* `ropes module<ropes.html>`_ for rope data type, which can represent very
  long strings efficiently



Seqs
----

========================================   ==========================================
Proc                                       Usage
========================================   ==========================================
`newSeq<#newSeq>`_                         Create a new sequence of a given length
`newSeqOfCap<#newSeqOfCap,Natural>`_       Create a new sequence with zero length
                                           and a given capacity
`setLen<#setLen,seq[T][T],Natural>`_       Set the length of a sequence
`@<#@,array[IDX,T]>`_                      Turn an array into a sequence
`add<#add,seq[T][T],T>`_                   Add an item to the sequence
`insert<#insert,seq[T][T],T>`_             Insert an item at a specific position
`delete<#delete,seq[T][T],Natural>`_       Delete an item while preserving the
                                           order of elements (`O(n)` operation)
`del<#del,seq[T][T],Natural>`_             `O(1)` removal, doesn't preserve the order
`pop<#pop,seq[T][T]>`_                     Remove and return last item of a sequence
`x & y<#&,seq[T][T],seq[T][T]>`_           Concatenate two sequences
`x[a..b]<#[],openArray[T],HSlice[U,V]>`_   Slice of a seqence (both ends included)
========================================   ==========================================

**See also:**
* `sequtils module<collections/sequtils.html>`_ for operations on container
  types (including strings)
* `json module<json.html>`_ for a structure which allows heterogeneous members
* `lists module<lists.html>`_ for linked lists



Sets
----

Built-in bit sets.

===============================     ======================================
Proc                                Usage
===============================     ======================================
`incl<#incl,set[T],T>`_             Include element `y` in the set `x`
`excl<#excl,set[T],T>`_             Exclude element `y` from the set `x`
`card<#card,set[T]>`_               Return the cardinality of the set,
                                    i.e. the number of elements
`a * b<#*,set[T],set[T]>`_          Intersection
`a + b<#+,set[T],set[T]>`_          Union
`a - b<#-,set[T],set[T]>`_          Difference
`contains<#contains,set[T],T>`_     Check if an element is in the set
[a < b](#<,set[T],set[T])           Check if `a` is a subset of `b`
===============================     ======================================

**See also:**
* `sets module <sets.html>`_ for hash sets
* `intsets module <intsets.html>`_ for efficient int sets



Numbers
-------

==============================    ==================================     =====================
Proc                              Usage                                  Also known as
                                                                         (in other languages)
==============================    ==================================     =====================
`div<#div,int,int>`_              Integer division                       `//`
`mod<#mod,int,int>`_              Integer modulo (remainder)             `%`
`shl<#shl,int,SomeInteger>`_      Shift left                             `<<`
`shr<#shr,int,SomeInteger>`_      Shift right                            `>>`
`ashr<#ashr,int,SomeInteger>`_    Arithmetic shift right
`and<#and,int,int>`_              Bitwise `and`                          `&`
`or<#or,int,int>`_                Bitwise `or`                           `|`
`xor<#xor,int,int>`_              Bitwise `xor`                          `^`
`not<#not,int>`_                  Bitwise `not` (complement)             `~`
`toInt<#toInt,float>`_            Convert floating-point number
                                  into an `int`
`toFloat<#toFloat,int>`_          Convert an integer into a `float`
==============================    ==================================     =====================

**See also:**
* `math module<math.html>`_ for mathematical operations like trigonometric
  functions, logarithms, square and cubic roots, etc.
* `complex module<complex.html>`_ for operations on complex numbers
* `rationals module<rationals.html>`_ for rational numbers



Ordinals
--------

`Ordinal type <#Ordinal>`_ includes integer, bool, character, and enumeration
types, as well as their subtypes.

=====================     =======================================
Proc                      Usage
=====================     =======================================
`succ<#succ,T,int>`_      Successor of the value
`pred<#pred,T,int>`_      Predecessor of the value
`inc<#inc,T,int>`_        Increment the ordinal
`dec<#dec,T,int>`_        Decrement the ordinal
`high<#high,T>`_          Return the highest possible value
`low<#low,T>`_            Return the lowest possible value
`ord<#ord,T>`_            Return `int` value of an ordinal value
=====================     =======================================



Misc
----

=============================================  ============================================
Proc                                           Usage
=============================================  ============================================
`is<#is,T,S>`_                                 Check if two arguments are of the same type
`isnot<#isnot.t,untyped,untyped>`_             Negated version of `is`
`!=<#!%3D.t,untyped,untyped>`_                 Not equals
`addr<#addr,T>`_                               Take the address of a memory location
`T and F<#and,bool,bool>`_                     Boolean `and`
`T or F<#or,bool,bool>`_                       Boolean `or`
`T xor F<#xor,bool,bool>`_                     Boolean `xor` (exclusive or)
`not T<#not,bool>`_                            Boolean `not`
`a .. b<#..,T,U>`_                             Binary slice that constructs an interval
                                               `[a, b]`
[a ..< b](#..<.t,untyped,untyped)              Interval `[a, b>` (excluded upper bound)
[runnableExamples](#runnableExamples,untyped)  Create testable documentation
=============================================  ============================================
]##



type
  int* {.magic: Int.}         ## Default integer type; bitwidth depends on
                              ## architecture, but is always the same as a pointer.
  int8* {.magic: Int8.}       ## Signed 8 bit integer type.
  int16* {.magic: Int16.}     ## Signed 16 bit integer type.
  int32* {.magic: Int32.}     ## Signed 32 bit integer type.
  int64* {.magic: Int64.}     ## Signed 64 bit integer type.
  uint* {.magic: UInt.}       ## Unsigned default integer type.
  uint8* {.magic: UInt8.}     ## Unsigned 8 bit integer type.
  uint16* {.magic: UInt16.}   ## Unsigned 16 bit integer type.
  uint32* {.magic: UInt32.}   ## Unsigned 32 bit integer type.
  uint64* {.magic: UInt64.}   ## Unsigned 64 bit integer type.
  float* {.magic: Float.}     ## Default floating point type.
  float32* {.magic: Float32.} ## 32 bit floating point type.
  float64* {.magic: Float.}   ## 64 bit floating point type.
# 'float64' is now an alias to 'float'; this solves many problems

  SomeSignedInt* = int|int8|int16|int32|int64
    ## Type class matching all signed integer types.
  SomeUnsignedInt* = uint|uint8|uint16|uint32|uint64
    ## Type class matching all unsigned integer types.
  SomeInteger* = SomeSignedInt|SomeUnsignedInt
    ## Type class matching all integer types.
  SomeFloat* = float|float32|float64
    ## Type class matching all floating point number types.
  SomeNumber* = SomeInteger|SomeFloat
    ## Type class matching all number types.

type # we need to start a new type section here, so that ``0`` can have a type
  bool* {.magic: Bool.} = enum ## Built-in boolean type.
    false = 0, true = 1
  Ordinal*[T] {.magic: Ordinal.} ## Generic ordinal type. Includes integer,
                                 ## bool, character, and enumeration types
                                 ## as well as their subtypes. Note `uint`
                                 ## and `uint64` are not ordinal types for
                                 ## implementation reasons.
  SomeOrdinal* = int|int8|int16|int32|int64|bool|enum|uint8|uint16|uint32
    ## Type class matching all ordinal types; however this includes enums with
    ## holes.

  char* {.magic: Char.}         ## Built-in 8 bit character type (unsigned).
  string* {.magic: String.}     ## Built-in string type.
  cstring* {.magic: Cstring.}   ## Built-in cstring (*compatible string*) type.
  pointer* {.magic: Pointer.}   ## Built-in pointer type, use the ``addr``
                                ## operator to get a pointer to a variable.
  typedesc* {.magic: TypeDesc.} ## Meta type to denote a type description.

const
  on* = true    ## Alias for ``true``.
  off* = false  ## Alias for ``false``.

{.push warning[GcMem]: off, warning[Uninit]: off.}
{.push hints: off.}

proc `or`*(a, b: typedesc): typedesc {.magic: "TypeTrait", noSideEffect.}
  ## Constructs an `or` meta class.

proc `and`*(a, b: typedesc): typedesc {.magic: "TypeTrait", noSideEffect.}
  ## Constructs an `and` meta class.

proc `not`*(a: typedesc): typedesc {.magic: "TypeTrait", noSideEffect.}
  ## Constructs an `not` meta class.

proc `|`*(a, b: typedesc): typedesc = discard

type
  `ptr`*[T] {.magic: Pointer.}   ## Built-in generic untraced pointer type.
  `ref`*[T] {.magic: Pointer.}   ## Built-in generic traced pointer type.
  void* {.magic: "VoidType".}    ## Meta type to denote the absence of any type.
  auto* {.magic: Expr.}          ## Meta type for automatic type determination.
  any* = distinct auto           ## Meta type for any supported type.
  untyped* {.magic: Expr.}       ## Meta type to denote an expression that
                                 ## is not resolved (for templates).
  typed* {.magic: Stmt.}         ## Meta type to denote an expression that
                                 ## is resolved (for templates).



# -----------------------------------------------------------------------------
# arrays
# -----------------------------------------------------------------------------

proc high*[I, T](x: array[I, T]): I {.magic: "High", noSideEffect.} =
  ## Returns the highest possible index of an array `x`.
  ##
  ## See also:
  ## * `low(array) <#low,array[I,T]>`_
  ##
  ## .. code-block:: Nim
  ##  var arr = [1, 2, 3, 4, 5, 6, 7]
  ##  high(arr) # => 6
  ##  for i in low(arr)..high(arr):
  ##    echo arr[i]
  discard

proc high*[I, T](x: typeDesc[array[I, T]]): I {.magic: "High", noSideEffect.} =
  ## Returns the highest possible index of an array type.
  ##
  ## See also:
  ## * `low(typedesc[array]) <#low,typedesc[array[I,T]]>`_
  ##
  ## .. code-block:: Nim
  ##  high(array[7, int]) # => 6
  discard

proc low*[I, T](x: array[I, T]): I {.magic: "Low", noSideEffect.} =
  ## Returns the lowest possible index of an array `x`.
  ##
  ## See also:
  ## * `high(array) <#high,array[I,T]>`_
  ##
  ## .. code-block:: Nim
  ##  var arr = [1, 2, 3, 4, 5, 6, 7]
  ##  low(arr) # => 0
  ##  for i in low(arr)..high(arr):
  ##    echo arr[i]
  discard

proc low*[I, T](x: typeDesc[array[I, T]]): I {.magic: "Low", noSideEffect.} =
  ## Returns the lowest possible index of an array type.
  ##
  ## See also:
  ## * `high(typedesc[array]) <#high,typedesc[array[I,T]]>`_
  ##
  ## .. code-block:: Nim
  ##  low(array[7, int]) # => 0
  discard

proc len*(x: (type array)|array): int {.magic: "LengthArray", noSideEffect.} =
  ## Returns the length of an array or an array type.
  ## This is roughly the same as ``high(T)-low(T)+1``.
  ##
  ## .. code-block:: Nim
  ##   var arr = [1, 1, 1, 1, 1]
  ##   echo len(arr) # => 5
  ##   echo len(array[3..8, int]) # => 6
  discard

proc `==`*[I, T](x, y: array[I, T]): bool = discard

proc `[]`*[Idx, T, U, V](a: array[Idx, T], x: HSlice[U, V]): seq[T] =
  ## Slice operation for arrays.
  ## Returns the inclusive range `[a[x.a], a[x.b]]`:
  ##
  ## .. code-block:: Nim
  ##    var a = [1, 2, 3, 4]
  ##    assert a[0..2] == @[1, 2, 3]
  discard

proc `[]=`*[Idx, T, U, V](a: var array[Idx, T], x: HSlice[U, V], b: openArray[T]) =
  ## Slice assignment for arrays.
  ##
  ## .. code-block:: Nim
  ##   var a = [10, 20, 30, 40, 50]
  ##   a[1..2] = @[99, 88]
  ##   assert a == [10, 99, 88, 40, 50]
  discard

proc `[]`*[Idx, T](a: array[Idx, T]; i: BackwardsIndex): T {.inline.} =
  discard

proc `[]`*[Idx, T](a: var array[Idx, T]; i: BackwardsIndex): var T {.inline.} =
  discard

proc `[]=`*[Idx, T](a: var array[Idx, T]; i: BackwardsIndex; x: T) {.inline.} =
  discard



# -----------------------------------------------------------------------------
# bool
# -----------------------------------------------------------------------------

proc `not`*(x: bool): bool {.magic: "Not", noSideEffect.} =
  ## Boolean not; returns true if ``x == false``.
  discard

proc `and`*(x, y: bool): bool {.magic: "And", noSideEffect.} =
  ## Boolean ``and``; returns true if ``x == y == true`` (if both arguments
  ## are true).
  ##
  ## Evaluation is lazy: if ``x`` is false, ``y`` will not even be evaluated.
  discard

proc `or`*(x, y: bool): bool {.magic: "Or", noSideEffect.} =
  ## Boolean ``or``; returns true if ``not (not x and not y)`` (if any of
  ## the arguments is true).
  ##
  ## Evaluation is lazy: if ``x`` is true, ``y`` will not even be evaluated.
  discard

proc `xor`*(x, y: bool): bool {.magic: "Xor", noSideEffect.} =
  ## Boolean `exclusive or`; returns true if ``x != y`` (if either argument
  ## is true while the other is false).
  discard

proc `==`*(x, y: bool): bool {.magic: "EqB", noSideEffect.} =
  ## Checks for equality between two `bool` variables.
  discard

proc `<=`*(x, y: bool): bool {.magic: "LeB", noSideEffect.}

proc `<`*(x, y: bool): bool {.magic: "LtB", noSideEffect.}




# -----------------------------------------------------------------------------
# integers
# -----------------------------------------------------------------------------

proc `+`*(x: int): int {.magic: "UnaryPlusI", noSideEffect.}
  ## Unary `+` operator for an integer. Has no effect.

proc `-`*(x: int): int {.magic: "UnaryMinusI", noSideEffect.}
  ## Unary `-` operator for an integer. Negates `x`.

proc `+`*(x, y: int): int {.magic: "AddI", noSideEffect.}
  ## Binary `+` operator for an integer.

proc `-`*(x, y: int): int {.magic: "SubI", noSideEffect.}
  ## Binary `-` operator for an integer.

proc `*`*(x, y: int): int {.magic: "MulI", noSideEffect.}
  ## Binary `*` operator for an integer.

proc `/`*(x, y: int): float {.inline, noSideEffect.} =
  ## Division of intergers that results in a float.
  ##
  ## See also:
  ## * `div <#div,int,int>`_
  ## * `mod <#mod,int,int>`_
  ##
  ## .. code-block:: Nim
  ##   echo 7 / 5 # => 1.4
  discard

proc `+=`*[T: SomeInteger](x: var T, y: T) {.
  magic: "Inc", noSideEffect.}
  ## Increments an integer.

proc `-=`*[T: SomeInteger](x: var T, y: T) {.
  magic: "Dec", noSideEffect.}
  ## Decrements an integer.

proc `*=`*[T: SomeInteger](x: var T, y: T) {.
  inline, noSideEffect.} =
  ## Binary `*=` operator for integers.
  discard

proc `div`*(x, y: int): int {.magic: "DivI", noSideEffect.}
  ## Computes the integer division.
  ##
  ## This is roughly the same as ``trunc(x/y)``.
  ##
  ## .. code-block:: Nim
  ##   ( 1 div  2) ==  0
  ##   ( 2 div  2) ==  1
  ##   ( 3 div  2) ==  1
  ##   ( 7 div  3) ==  2
  ##   (-7 div  3) == -2
  ##   ( 7 div -3) == -2
  ##   (-7 div -3) ==  2

proc `mod`*(x, y: int): int {.magic: "ModI", noSideEffect.}
  ## Computes the integer modulo operation (remainder).
  ##
  ## This is the same as ``x - (x div y) * y``.
  ##
  ## .. code-block:: Nim
  ##   ( 7 mod  5) ==  2
  ##   (-7 mod  5) == -2
  ##   ( 7 mod -5) ==  2
  ##   (-7 mod -5) == -2

proc `shr`*(x: int, y: SomeInteger): int {.magic: "ShrI", noSideEffect.}
  ## Computes the `shift right` operation of `x` and `y`, filling
  ## vacant bit positions with zeros.
  ##
  ## **Note**: `Operator precedence <manual.html#syntax-precedence>`_
  ## is different than in *C*.
  ##
  ## See also:
  ## * `ashr proc <#ashr,int,SomeInteger>`_ for arithmetic shift right
  ##
  ## .. code-block:: Nim
  ##   0b0001_0000'i8 shr 2 == 0b0000_0100'i8
  ##   0b1000_0000'i8 shr 8 == 0b0000_0000'i8
  ##   0b0000_0001'i8 shr 1 == 0b0000_0000'i8

proc `shl`*(x: int, y: SomeInteger): int {.magic: "ShlI", noSideEffect.}
  ## Computes the `shift left` operation of `x` and `y`.
  ##
  ## **Note**: `Operator precedence <manual.html#syntax-precedence>`_
  ## is different than in *C*.
  ##
  ## .. code-block:: Nim
  ##  1'i32 shl 4 == 0x0000_0010
  ##  1'i64 shl 4 == 0x0000_0000_0000_0010

proc ashr*(x: int, y: SomeInteger): int {.magic: "AshrI", noSideEffect.}
  ## Shifts right by pushing copies of the leftmost bit in from the left,
  ## and let the rightmost bits fall off.
  ##
  ## See also:
  ## * `shr proc <#shr,int,SomeInteger>`_
  ##
  ## .. code-block:: Nim
  ##   0b0001_0000'i8 shr 2 == 0b0000_0100'i8
  ##   0b1000_0000'i8 shr 8 == 0b1111_1111'i8
  ##   0b1000_0000'i8 shr 1 == 0b1100_0000'i8

proc `not`*(x: int): int {.magic: "BitnotI", noSideEffect.}
  ## Computes the `bitwise complement` of the integer `x`.
  ##
  ## .. code-block:: Nim
  ##   var
  ##     a = 0'u8
  ##     b = 0'i8
  ##     c = 1000'u16
  ##     d = 1000'i16
  ##
  ##   echo not a # => 255
  ##   echo not b # => -1
  ##   echo not c # => 64535
  ##   echo not d # => -1001

proc `and`*(x, y: int): int {.magic: "BitandI", noSideEffect.}
  ## Computes the `bitwise and` of numbers `x` and `y`.
  ##
  ## .. code-block:: Nim
  ##   (0b0011 and 0b0101) == 0b0001
  ##   (0b0111 and 0b1100) == 0b0100

proc `or`*(x, y: int): int {.magic: "BitorI", noSideEffect.}
  ## Computes the `bitwise or` of numbers `x` and `y`.
  ##
  ## .. code-block:: Nim
  ##   (0b0011 or 0b0101) == 0b0111
  ##   (0b0111 or 0b1100) == 0b1111

proc `xor`*(x, y: int): int {.magic: "BitxorI", noSideEffect.}
  ## Computes the `bitwise xor` of numbers `x` and `y`.
  ##
  ## .. code-block:: Nim
  ##   (0b0011 xor 0b0101) == 0b0110
  ##   (0b0111 xor 0b1100) == 0b1011

proc `==`*(x, y: int): bool {.magic: "EqI", noSideEffect.}
  ## Compares two integers for equality.

proc `<=`*(x, y: int): bool {.magic: "LeI", noSideEffect.}
  ## Returns true if `x` is less than or equal to `y`.

proc `<`*(x, y: int): bool {.magic: "LtI", noSideEffect.}
  ## Returns true if `x` is less than `y`.

proc min*(x, y: int): int {.magic: "MinI", noSideEffect.} =
  ## The minimum value of two integers.
  discard

proc max*(x, y: int): int {.magic: "MaxI", noSideEffect.} =
  ## The maximum value of two integers.
  discard

proc `+%`*(x, y: int64): int64 {.magic: "AddU", noSideEffect.}
  ## Treats `x` and `y` as unsigned and adds them.
  ##
  ## The result is truncated to fit into the result.
  ## This implements modulo arithmetic. No overflow errors are possible.

proc `-%`*(x, y: int64): int64 {.magic: "SubU", noSideEffect.}
  ## Treats `x` and `y` as unsigned and subtracts them.
  ##
  ## The result is truncated to fit into the result.
  ## This implements modulo arithmetic. No overflow errors are possible.

proc `*%`*(x, y: int64): int64 {.magic: "MulU", noSideEffect.}
  ## Treats `x` and `y` as unsigned and multiplies them.
  ##
  ## The result is truncated to fit into the result.
  ## This implements modulo arithmetic. No overflow errors are possible.

proc `/%`*(x, y: int64): int64 {.magic: "DivU", noSideEffect.}
  ## Treats `x` and `y` as unsigned and divides them.
  ##
  ## The result is truncated to fit into the result.
  ## This implements modulo arithmetic. No overflow errors are possible.

proc `%%`*(x, y: int64): int64 {.magic: "ModU", noSideEffect.}
  ## Treats `x` and `y` as unsigned and compute the modulo of `x` and `y`.
  ##
  ## The result is truncated to fit into the result.
  ## This implements modulo arithmetic. No overflow errors are possible.

proc `<=%`*(x, y: int64): bool {.magic: "LeU64", noSideEffect.}
  ## Treats `x` and `y` as unsigned and compares them.
  ## Returns true if ``unsigned(x) <= unsigned(y)``.

proc `<%`*(x, y: int64): bool {.magic: "LtU64", noSideEffect.}
  ## Treats `x` and `y` as unsigned and compares them.
  ## Returns true if ``unsigned(x) < unsigned(y)``.

template `>=%`*(x, y: untyped): untyped = y <=% x
  ## Treats `x` and `y` as unsigned and compares them.
  ## Returns true if ``unsigned(x) >= unsigned(y)``.

template `>%`*(x, y: untyped): untyped = y <% x
  ## Treats `x` and `y` as unsigned and compares them.
  ## Returns true if ``unsigned(x) > unsigned(y)``.

proc toInt*(f: float): int {.
  magic: "ToInt", noSideEffect, importc: "toInt".}
  ## Converts a floating point number `f` into an ``int``.
  ##
  ## Conversion rounds `f` half away from 0, see
  ## `Round half away from zero
  ## <https://en.wikipedia.org/wiki/Rounding#Round_half_away_from_zero>`_.
  ##
  ## Note that some floating point numbers (e.g. infinity or even 1e19)
  ## cannot be accurately converted.
  ##
  ## .. code-block:: Nim
  ##   doAssert toInt(0.49) == 0
  ##   doAssert toInt(0.5) == 1
  ##   doAssert toInt(-0.5) == -1 # rounding is symmetrical

proc toBiggestInt*(f: BiggestFloat): BiggestInt {.
  magic: "ToBiggestInt", noSideEffect, importc: "toBiggestInt".} =
  ## Same as `toInt <#toInt,float>`_ but for ``BiggestFloat`` to ``BiggestInt``.
  discard



# -----------------------------------------------------------------------------
# floats
# -----------------------------------------------------------------------------

proc `+`*(x: float): float {.magic: "UnaryPlusF64", noSideEffect.}
proc `-`*(x: float): float {.magic: "UnaryMinusF64", noSideEffect.}
proc `+`*(x, y: float): float {.magic: "AddF64", noSideEffect.}
proc `-`*(x, y: float): float {.magic: "SubF64", noSideEffect.}
proc `*`*(x, y: float): float {.magic: "MulF64", noSideEffect.}
proc `/`*(x, y: float): float {.magic: "DivF64", noSideEffect.}

proc `+=`*[T: float|float32|float64] (x: var T, y: T) {.
  inline, noSideEffect.} =
  ## Increments in place a floating point number.
  discard

proc `-=`*[T: float|float32|float64] (x: var T, y: T) {.
  inline, noSideEffect.} =
  ## Decrements in place a floating point number.
  discard

proc `*=`*[T: float|float32|float64] (x: var T, y: T) {.
  inline, noSideEffect.} =
  ## Multiplies in place a floating point number.
  discard

proc `/=`*(x: var float64, y: float64) {.inline, noSideEffect.} =
  ## Divides in place a floating point number.
  discard

proc `==`*(x, y: float): bool {.magic: "EqF64", noSideEffect.}
proc `<=`*(x, y: float): bool {.magic: "LeF64", noSideEffect.}
proc `<`*(x, y: float): bool {.magic: "LtF64", noSideEffect.}

proc abs*(x: float): float {.magic: "AbsF64", noSideEffect.} =
  discard

proc min*(x, y: float): float {.magic: "MinF64", noSideEffect.} =
  discard

proc max*(x, y: float): float {.magic: "MaxF64", noSideEffect.} =
  discard

proc min*[T](x, y: T): T {.inline.} =
  discard

proc max*[T](x, y: T): T {.inline.} =
  discard

proc high*(T: typedesc[SomeFloat]): T = Inf
proc low*(T: typedesc[SomeFloat]): T = NegInf

proc clamp*[T](x, a, b: T): T =
  ## Limits the value ``x`` within the interval [a, b].
  ##
  ## .. code-block:: Nim
  ##   assert((1.4).clamp(0.0, 1.0) == 1.0)
  ##   assert((0.5).clamp(0.0, 1.0) == 0.5)
  discard

proc toFloat*(i: int): float {.
  magic: "ToFloat", noSideEffect, importc: "toFloat".}
  ## Converts an integer `i` into a ``float``.
  ##
  ## If the conversion fails, `ValueError` is raised.
  ## However, on most platforms the conversion cannot fail.
  ##
  ## .. code-block:: Nim
  ##   let
  ##     a = 2
  ##     b = 3.7
  ##
  ##   echo a.toFloat + b # => 5.7

proc toBiggestFloat*(i: BiggestInt): BiggestFloat {.
  magic: "ToBiggestFloat", noSideEffect, importc: "toBiggestFloat".}
  ## Same as `toFloat <#toFloat,int>`_ but for ``BiggestInt`` to ``BiggestFloat``.




# -----------------------------------------------------------------------------
# ordinals & enums
# -----------------------------------------------------------------------------

proc `==`*[Enum: enum](x, y: Enum): bool {.magic: "EqEnum", noSideEffect.} =
  ## Checks whether values within the *same enum* have the same underlying value.
  ##
  ## .. code-block:: Nim
  ##  type
  ##    Enum1 = enum
  ##      Field1 = 3, Field2
  ##    Enum2 = enum
  ##      Place1, Place2 = 3
  ##  var
  ##    e1 = Field1
  ##    e2 = Enum1(Place2)
  ##  echo (e1 == e2) # true
  ##  echo (e1 == Place2) # raises error
  discard

proc `<=`*[Enum: enum](x, y: Enum): bool {.magic: "LeEnum", noSideEffect.} =
  discard

proc `<`*[Enum: enum](x, y: Enum): bool {.magic: "LtEnum", noSideEffect.}

proc high*[T: Ordinal](x: T): T {.magic: "High", noSideEffect.} =
  ## Returns the highest possible value of an ordinal value `x`.
  ##
  ## As a special semantic rule, `x` may also be a type identifier.
  ##
  ## See also:
  ## * `low(T) <#low,T>`_
  ##
  ## .. code-block:: Nim
  ##  high(2) # => 9223372036854775807
  discard

proc high*[T: Ordinal|enum](x: typeDesc[T]): T {.magic: "High", noSideEffect.} =
  ## Returns the highest possible value of an ordinal or enum type.
  ##
  ## ``high(int)`` is Nim's way of writing `INT_MAX`:idx: or `MAX_INT`:idx:.
  ##
  ## See also:
  ## * `low(typedesc) <#low,typedesc[T]>`_
  ##
  ## .. code-block:: Nim
  ##  high(int) # => 9223372036854775807
  discard

proc low*[T](x: T): T {.magic: "Low", noSideEffect.} =
  ## Returns the lowest possible value of an ordinal value `x`. As a special
  ## semantic rule, `x` may also be a type identifier.
  ##
  ## See also:
  ## * `high(T) <#high,T>`_
  ##
  ## .. code-block:: Nim
  ##  low(2) # => -9223372036854775808
  discard

proc low*[T: Ordinal|enum](x: typeDesc[T]): T {.magic: "Low", noSideEffect.} =
  ## Returns the lowest possible value of an ordinal or enum type.
  ##
  ## ``low(int)`` is Nim's way of writing `INT_MIN`:idx: or `MIN_INT`:idx:.
  ##
  ## See also:
  ## * `high(typedesc) <#high,typedesc[T]>`_
  ##
  ## .. code-block:: Nim
  ##  low(int) # => -9223372036854775808
  discard

proc succ*[T: Ordinal](x: T, y = 1): T {.magic: "Succ", noSideEffect.} =
  ## Returns the ``y``-th successor (default: 1) of the value ``x``.
  ## ``T`` has to be an `ordinal type <#Ordinal>`_.
  ##
  ## If such a value does not exist, ``OverflowError`` is raised
  ## or a compile time error occurs.
  ##
  ## .. code-block:: Nim
  ##   let x = 5
  ##   echo succ(5)    # => 6
  ##   echo succ(5, 3) # => 8
  discard

proc pred*[T: Ordinal](x: T, y = 1): T {.magic: "Pred", noSideEffect.} =
  ## Returns the ``y``-th predecessor (default: 1) of the value ``x``.
  ## ``T`` has to be an `ordinal type <#Ordinal>`_.
  ##
  ## If such a value does not exist, ``OverflowError`` is raised
  ## or a compile time error occurs.
  ##
  ## .. code-block:: Nim
  ##   let x = 5
  ##   echo pred(5)    # => 4
  ##   echo pred(5, 3) # => 2
  discard

proc inc*[T: Ordinal|uint|uint64](x: var T, y = 1) {.magic: "Inc", noSideEffect.} =
  ## Increments the ordinal ``x`` by ``y``.
  ##
  ## If such a value does not exist, ``OverflowError`` is raised or a compile
  ## time error occurs. This is a short notation for: ``x = succ(x, y)``.
  ##
  ## .. code-block:: Nim
  ##  var i = 2
  ##  inc(i)    # i <- 3
  ##  inc(i, 3) # i <- 6
  discard

proc dec*[T: Ordinal|uint|uint64](x: var T, y = 1) {.magic: "Dec", noSideEffect.} =
  ## Decrements the ordinal ``x`` by ``y``.
  ##
  ## If such a value does not exist, ``OverflowError`` is raised or a compile
  ## time error occurs. This is a short notation for: ``x = pred(x, y)``.
  ##
  ## .. code-block:: Nim
  ##  var i = 2
  ##  dec(i)    # i <- 1
  ##  dec(i, 3) # i <- -2
  discard

proc ord*[T: Ordinal|enum](x: T): int {.magic: "Ord", noSideEffect.} =
  ## Returns the internal `int` value of an ordinal value ``x``.
  ##
  ## .. code-block:: Nim
  ##   echo ord('A') # => 65
  ##   echo ord('a') # => 97
  discard

proc len*[U: Ordinal; V: Ordinal](x: HSlice[U, V]): int {.noSideEffect, inline.} =
  ## Length of ordinal slice. When x.b < x.a returns zero length.
  ##
  ## .. code-block:: Nim
  ##   assert((0..5).len == 6)
  ##   assert((5..2).len == 0)
  discard





# -----------------------------------------------------------------------------
# sets
# -----------------------------------------------------------------------------

proc `==`*[T](x, y: set[T]): bool {.magic: "EqSet", noSideEffect.} =
  ## Checks for equality between two variables of type `set`.
  ##
  ## .. code-block:: Nim
  ##  var a = {1, 2, 2, 3} # duplication in sets is ignored
  ##  var b = {1, 2, 3}
  ##  echo (a == b) # true
  discard

proc `<=`*[T](x, y: set[T]): bool {.magic: "LeSet", noSideEffect.} =
  ## Returns true if `x` is a subset of `y`.
  ##
  ## A subset `x` has all of its members in `y` and `y` doesn't necessarily
  ## have more members than `x`. That is, `x` can be equal to `y`.
  ##
  ## .. code-block:: Nim
  ##   let
  ##     a = {3, 5}
  ##     b = {1, 3, 5, 7}
  ##     c = {2}
  ##   assert a <= b
  ##   assert a <= a
  ##   assert (a <= c) == false
  discard

proc `<`*[T](x, y: set[T]): bool {.magic: "LtSet", noSideEffect.}
  ## Returns true if `x` is a strict or proper subset of `y`.
  ##
  ## A strict or proper subset `x` has all of its members in `y` but `y` has
  ## more elements than `y`.
  ##
  ## .. code-block:: Nim
  ##   let
  ##     a = {3, 5}
  ##     b = {1, 3, 5, 7}
  ##     c = {2}
  ##   assert a < b
  ##   assert (a < a) == false
  ##   assert (a < c) == false

# set routines:
proc incl*[T](x: var set[T], y: T) {.magic: "Incl", noSideEffect.} =
  ## Includes element ``y`` in the set ``x``.
  ##
  ## This is the same as ``x = x + {y}``, but it might be more efficient.
  ##
  ## .. code-block:: Nim
  ##   var a = {1, 3, 5}
  ##   a.incl(2) # a <- {1, 2, 3, 5}
  ##   a.incl(4) # a <- {1, 2, 3, 4, 5}
  discard

template incl*[T](x: var set[T], y: set[T]) = discard
  ## Includes the set ``y`` in the set ``x``.
  ##
  ## .. code-block:: Nim
  ##   var a = {1, 3, 5, 7}
  ##   var b = {4, 5, 6}
  ##   a.incl(b)  # a <- {1, 3, 4, 5, 6, 7}

proc excl*[T](x: var set[T], y: T) {.magic: "Excl", noSideEffect.} =
  ## Excludes element ``y`` from the set ``x``.
  ##
  ## This is the same as ``x = x - {y}``, but it might be more efficient.
  ##
  ## .. code-block:: Nim
  ##   var b = {2, 3, 5, 6, 12, 545}
  ##   b.excl(5)  # b <- {2, 3, 6, 12, 545}
  discard

template excl*[T](x: var set[T], y: set[T]) = discard
  ## Excludes the set ``y`` from the set ``x``.
  ##
  ## .. code-block:: Nim
  ##   var a = {1, 3, 5, 7}
  ##   var b = {3, 4, 5}
  ##   a.excl(b)  # a <- {1, 7}

proc card*[T](x: set[T]): int {.magic: "Card", noSideEffect.} =
  ## Returns the cardinality of the set ``x``, i.e. the number of elements
  ## in the set.
  ##
  ## .. code-block:: Nim
  ##   var a = {1, 3, 5, 7}
  ##   echo card(a) # => 4
  discard

# set operators
proc `*`*[T](x, y: set[T]): set[T] {.magic: "MulSet", noSideEffect.} =
  ## This operator computes the intersection of two sets.
  ##
  ## .. code-block:: Nim
  ##   let
  ##     a = {1, 2, 3}
  ##     b = {2, 3, 4}
  ##   echo a * b # => {2, 3}
  discard

proc `+`*[T](x, y: set[T]): set[T] {.magic: "PlusSet", noSideEffect.} =
  ## This operator computes the union of two sets.
  ##
  ## .. code-block:: Nim
  ##   let
  ##     a = {1, 2, 3}
  ##     b = {2, 3, 4}
  ##   echo a + b # => {1, 2, 3, 4}
  discard

proc `-`*[T](x, y: set[T]): set[T] {.magic: "MinusSet", noSideEffect.} =
  ## This operator computes the diference of two sets.
  ##
  ## .. code-block:: Nim
  ##   let
  ##     a = {1, 2, 3}
  ##     b = {2, 3, 4}
  ##   echo a - b # => {1}
  discard

proc contains*[T](x: set[T], y: T): bool {.magic: "InSet", noSideEffect.} =
  ## One should overload this proc if one wants to overload the ``in`` operator.
  ##
  ## The parameters are in reverse order! ``a in b`` is a template for
  ## ``contains(b, a)``.
  ## This is because the unification algorithm that Nim uses for overload
  ## resolution works from left to right.
  ## But for the ``in`` operator that would be the wrong direction for this
  ## piece of code:
  ##
  ## .. code-block:: Nim
  ##   var s: set[range['a'..'z']] = {'a'..'c'}
  ##   assert s.contains('c')
  ##   assert 'b' in s
  ##
  ## If ``in`` had been declared as ``[T](elem: T, s: set[T])`` then ``T`` would
  ## have been bound to ``char``. But ``s`` is not compatible to type
  ## ``set[char]``! The solution is to bind ``T`` to ``range['a'..'z']``. This
  ## is achieved by reversing the parameters for ``contains``; ``in`` then
  ## passes its arguments in reverse order.
  discard




# -----------------------------------------------------------------------------
# seqs
# -----------------------------------------------------------------------------

proc newSeq*[T](s: var seq[T], len: Natural) {.magic: "NewSeq", noSideEffect.} =
  ## Creates a new sequence of type ``seq[T]`` with length ``len``.
  ##
  ## This is equivalent to ``s = @[]; setlen(s, len)``, but more
  ## efficient since no reallocation is needed.
  ##
  ## Note that the sequence will be filled with zeroed entries.
  ## After the creation of the sequence you should assign entries to
  ## the sequence instead of adding them. Example:
  ##
  ## .. code-block:: Nim
  ##   var inputStrings : seq[string]
  ##   newSeq(inputStrings, 3)
  ##   assert len(inputStrings) == 3
  ##   inputStrings[0] = "The fourth"
  ##   inputStrings[1] = "assignment"
  ##   inputStrings[2] = "would crash"
  ##   #inputStrings[3] = "out of bounds"
  discard

proc newSeq*[T](len = 0.Natural): seq[T] =
  ## Creates a new sequence of type ``seq[T]`` with length ``len``.
  ##
  ## Note that the sequence will be filled with zeroed entries.
  ## After the creation of the sequence you should assign entries to
  ## the sequence instead of adding them.
  ##
  ## See also:
  ## * `newSeqOfCap <#newSeqOfCap,Natural>`_
  ## * `newSeqUninitialized <#newSeqUninitialized,Natural>`_
  ##
  ## .. code-block:: Nim
  ##   var inputStrings = newSeq[string](3)
  ##   assert len(inputStrings) == 3
  ##   inputStrings[0] = "The fourth"
  ##   inputStrings[1] = "assignment"
  ##   inputStrings[2] = "would crash"
  ##   #inputStrings[3] = "out of bounds"
  discard

proc `@`* [IDX, T](a: array[IDX, T]): seq[T] {.
  magic: "ArrToSeq", nosideeffect.} =
  ## Turns an array into a sequence.
  ##
  ## This most often useful for constructing
  ## sequences with the array constructor: ``@[1, 2, 3]`` has the type
  ## ``seq[int]``, while ``[1, 2, 3]`` has the type ``array[0..2, int]``.
  ##
  ## .. code-block:: Nim
  ##   let
  ##     a = [1, 3, 5]
  ##     b = "foo"
  ##
  ##   echo @a # => @[1, 3, 5]
  ##   echo @b # => @['f', 'o', 'o']
  discard

proc `@`*[T](a: openArray[T]): seq[T] =
  ## Turns an `openarray <#openArray>`_ into a sequence.
  ##
  ## This is not as efficient as turning a fixed length array into a sequence
  ## as it always copies every element of `a`.
  discard

proc newSeqOfCap*[T](cap: Natural): seq[T] {.magic: "NewSeqOfCap", noSideEffect.} =
  ## Creates a new sequence of type ``seq[T]`` with length zero and capacity
  ## ``cap``.
  ##
  ## .. code-block:: Nim
  ##   var x = newSeqOfCap[int](5)
  ##   assert len(x) == 0
  ##   x.add(10)
  ##   assert len(x) == 1
  discard

proc newSeqUninitialized*[T: SomeNumber](len: Natural): seq[T] =
  ## Creates a new sequence of type ``seq[T]`` with length ``len``.
  ##
  ## Only available for numbers types. Note that the sequence will be
  ## uninitialized. After the creation of the sequence you should assign
  ## entries to the sequence instead of adding them.
  ##
  ## This is not available for JS backend.
  ##
  ## .. code-block:: Nim
  ##   var x = newSeqUninitialized[int](3)
  ##   assert len(x) == 3
  ##   x[0] = 10
  discard

proc len*[TOpenArray: openArray|varargs](x: TOpenArray): int {.
  magic: "LengthOpenArray", noSideEffect.} =
  ## Returns the length of an openarray.
  ##
  ## .. code-block:: Nim
  ##   var s = [1, 1, 1, 1, 1]
  ##   echo len(s) # => 5
  discard

proc len*[T](x: seq[T]): int {.magic: "LengthSeq", noSideEffect.} =
  ## Returns the length of a sequence.
  ##
  ## .. code-block:: Nim
  ##   var s = @[1, 1, 1, 1, 1]
  ##   echo len(s) # => 5
  discard

proc setLen*[T](s: var seq[T], newlen: Natural) {.
  magic: "SetLengthSeq", noSideEffect.} =
  ## Sets the length of seq `s` to `newlen`. ``T`` may be any sequence type.
  ##
  ## If the current length is greater than the new length,
  ## ``s`` will be truncated.
  ##
  ## .. code-block:: Nim
  ##   var x = @[10, 20]
  ##   x.setLen(5)
  ##   x[4] = 50
  ##   assert x == @[10, 20, 0, 0, 50]
  ##   x.setLen(1)
  ##   assert x == @[10]
  discard

proc high*[T](x: openArray[T]): int {.magic: "High", noSideEffect.} =
  ## Returns the highest possible index of a sequence `x`.
  ##
  ## See also:
  ## * `low(openArray) <#low,openArray[T]>`_
  ##
  ## .. code-block:: Nim
  ##  var s = @[1, 2, 3, 4, 5, 6, 7]
  ##  high(s) # => 6
  ##  for i in low(s)..high(s):
  ##    echo s[i]
  discard

proc low*[T](x: openArray[T]): int {.magic: "Low", noSideEffect.} =
  ## Returns the lowest possible index of a sequence `x`.
  ##
  ## See also:
  ## * `high(openArray) <#high,openArray[T]>`_
  ##
  ## .. code-block:: Nim
  ##  var s = @[1, 2, 3, 4, 5, 6, 7]
  ##  low(s) # => 0
  ##  for i in low(s)..high(s):
  ##    echo s[i]
  discard

proc add*[T](x: var seq[T], y: T) {.magic: "AppendSeqElem", noSideEffect.} =
  ## Generic proc for adding a data item `y` to a container `x`.
  ##
  ## For containers that have an order, `add` means *append*. New generic
  ## containers should also call their adding proc `add` for consistency.
  ## Generic code becomes much easier to write if the Nim naming scheme is
  ## respected.
  discard

proc add*[T](x: var seq[T], y: openArray[T]) {.noSideEffect.} =
  ## Generic proc for adding a container `y` to a container `x`.
  ##
  ## For containers that have an order, `add` means *append*. New generic
  ## containers should also call their adding proc `add` for consistency.
  ## Generic code becomes much easier to write if the Nim naming scheme is
  ## respected.
  ##
  ## See also:
  ## * `& proc <#&,seq[T][T],seq[T][T]>`_
  ##
  ## .. code-block:: Nim
  ##   var s: seq[string] = @["test2","test2"]
  ##   s.add("test") # s <- @[test2, test2, test]
  discard

proc insert*[T](x: var seq[T], item: T, i = 0.Natural) {.noSideEffect.} =
  ## Inserts `item` into `x` at position `i`.
  ##
  ## .. code-block:: Nim
  ##  var i = @[1, 3, 5]
  ##  i.insert(99, 0) # i <- @[99, 1, 3, 5]
  discard

proc `&`*[T](x, y: seq[T]): seq[T] {.noSideEffect.} =
  ## Concatenates two sequences.
  ##
  ## Requires copying of the sequences.
  ##
  ## See also:
  ## * `add(var seq[T], openArray[T]) <#add,seq[T][T],openArray[T]>`_
  ##
  ## .. code-block:: Nim
  ##   assert(@[1, 2, 3, 4] & @[5, 6] == @[1, 2, 3, 4, 5, 6])
  discard

proc `&`*[T](x: seq[T], y: T): seq[T] {.noSideEffect.} =
  ## Appends element y to the end of the sequence.
  ##
  ## Requires copying of the sequence.
  ##
  ## See also:
  ## * `add(var seq[T], T) <#add,seq[T][T],T>`_
  ##
  ## .. code-block:: Nim
  ##   assert(@[1, 2, 3] & 4 == @[1, 2, 3, 4])
  discard

proc `&`*[T](x: T, y: seq[T]): seq[T] {.noSideEffect.} =
  ## Prepends the element x to the beginning of the sequence.
  ##
  ## Requires copying of the sequence.
  ##
  ## .. code-block:: Nim
  ##   assert(1 & @[2, 3, 4] == @[1, 2, 3, 4])
  discard

proc del*[T](x: var seq[T], i: Natural) {.noSideEffect.} =
  ## Deletes the item at index `i` by putting ``x[high(x)]`` into position `i`.
  ##
  ## This is an `O(1)` operation.
  ##
  ## See also:
  ## * `delete <#delete,seq[T][T],Natural>`_ for preserving the order
  ##
  ## .. code-block:: Nim
  ##  var i = @[1, 2, 3, 4, 5]
  ##  i.del(2) # => @[1, 2, 5, 4]
  discard

proc delete*[T](x: var seq[T], i: Natural) {.noSideEffect.} =
  ## Deletes the item at index `i` by moving all ``x[i+1..]`` items by one position.
  ##
  ## This is an `O(n)` operation.
  ##
  ## See also:
  ## * `del <#delete,seq[T][T],Natural>`_ for O(1) operation
  ##
  ## .. code-block:: Nim
  ##  var i = @[1, 2, 3, 4, 5]
  ##  i.delete(2) # => @[1, 2, 4, 5]
  discard

proc pop*[T](s: var seq[T]): T {.inline, noSideEffect.} =
  ## Returns the last item of `s` and decreases ``s.len`` by one. This treats
  ## `s` as a stack and implements the common *pop* operation.
  ##
  ## .. code-block:: Nim
  ##   var a = @[1, 3, 5, 7]
  ##   let b = pop(a)
  ##   assert b == 7
  ##   assert a == @[1, 3, 5]
  discard

proc min*[T](x: openArray[T]): T =
  ## The minimum value of `x`. ``T`` needs to have a ``<`` operator.
  discard

proc max*[T](x: openArray[T]): T =
  ## The maximum value of `x`. ``T`` needs to have a ``<`` operator.
  discard

proc `==`*[T](x, y: openarray[T]): bool = discard

proc `==`*[T](x, y: seq[T]): bool {.noSideEffect.} =
  ## Generic equals operator for sequences: relies on a equals operator for
  ## the element type `T`.
  discard

proc contains*[T](a: openArray[T], item: T): bool {.inline.} =
  ## Returns true if `item` is in `a` or false if not found. This is a shortcut
  ## for ``find(a, item) >= 0``.
  ##
  ## This allows the `in` operator: `a.contains(item)` is the same as
  ## `item in a`.
  ##
  ## .. code-block:: Nim
  ##   var a = @[1, 3, 5]
  ##   assert a.contains(5)
  ##   assert 3 in a
  ##   assert 99 notin a
  discard

proc `[]`*[T, U, V](s: openArray[T], x: HSlice[U, V]): seq[T] =
  ## Slice operation for sequences.
  ## Returns the inclusive range `[s[x.a], s[x.b]]`:
  ##
  ## .. code-block:: Nim
  ##    var s = @[1, 2, 3, 4]
  ##    assert s[0..2] == @[1, 2, 3]
  discard

proc `[]=`*[T, U, V](s: var seq[T], x: HSlice[U, V], b: openArray[T]) =
  ## Slice assignment for sequences.
  ##
  ## If ``b.len`` is not exactly the number of elements that are referred to
  ## by `x`, a `splice`:idx: is performed.
  ##
  ## .. code-block:: Nim
  ##   var s = @"abcdefgh"
  ##   s[1 .. ^2] = @"xyz"
  ##   assert s == @"axyzh"

proc `[]`*[T](s: openArray[T]; i: BackwardsIndex): T {.inline.} =
  discard

proc `[]`*[T](s: var openArray[T]; i: BackwardsIndex): var T {.inline.} =
  discard

proc `[]=`*[T](s: var openArray[T]; i: BackwardsIndex; x: T) {.inline.} =
  discard

proc shallow*[T](s: var seq[T]) {.noSideEffect, inline.} =
  ## Marks a sequence `s` as `shallow`:idx:. Subsequent assignments will not
  ## perform deep copies of `s`.
  ##
  ## This is only useful for optimization purposes.
  discard

when not defined(js):
  proc toOpenArray*[T](x: seq[T]; first, last: int): openarray[T] {.
    magic: "Slice".}
  proc toOpenArray*[T](x: openarray[T]; first, last: int): openarray[T] {.
    magic: "Slice".}
  proc toOpenArray*[T](x: ptr UncheckedArray[T]; first, last: int): openarray[T] {.
    magic: "Slice".}
  proc toOpenArray*[I, T](x: array[I, T]; first, last: I): openarray[T] {.
    magic: "Slice".}
  proc toOpenArray*(x: string; first, last: int): openarray[char] {.
    magic: "Slice".}
  proc toOpenArrayByte*(x: string; first, last: int): openarray[byte] {.
    magic: "Slice".}



# -----------------------------------------------------------------------------
# strings & chars
# -----------------------------------------------------------------------------


# chars

proc `==`*(x, y: char): bool {.magic: "EqCh", noSideEffect.} =
  ## Checks for equality between two `char` variables.
  discard

proc `<=`*(x, y: char): bool {.magic: "LeCh", noSideEffect.} =
  ## Compares two chars and returns true if `x` is lexicographically
  ## before `y` (uppercase letters come before lowercase letters).
  ##
  ## .. code-block:: Nim
  ##     let
  ##       a = 'a'
  ##       b = 'b'
  ##       c = 'Z'
  ##     assert a <= b
  ##     assert a <= a
  ##     assert (a <= c) == false
  discard

proc `<`*(x, y: char): bool {.magic: "LtCh", noSideEffect.}
  ## Compares two chars and returns true if `x` is lexicographically
  ## before `y` (uppercase letters come before lowercase letters).
  ##
  ## .. code-block:: Nim
  ##     let
  ##       a = 'a'
  ##       b = 'b'
  ##       c = 'Z'
  ##     assert a < b
  ##     assert (a < a) == false
  ##     assert (a < c) == false

proc chr*(u: range[0..255]): char {.magic: "Chr", noSideEffect.} =
  ## Converts an `int` in the range `0..255` to a character.
  ##
  ## .. code-block:: Nim
  ##   echo chr(65) # => A
  ##   echo chr(97) # => a
  discard


# strings

proc `==`*(x, y: string): bool {.magic: "EqStr", noSideEffect.} =
  ## Checks for equality between two `string` variables.
  discard

proc `<=`*(x, y: string): bool {.magic: "LeStr", noSideEffect.} =
  ## Compares two strings and returns true if `x` is lexicographically
  ## before `y` (uppercase letters come before lowercase letters).
  ##
  ## .. code-block:: Nim
  ##     let
  ##       a = "abc"
  ##       b = "abd"
  ##       c = "ZZZ"
  ##     assert a <= b
  ##     assert a <= a
  ##     assert (a <= c) == false
  discard

proc `<`*(x, y: string): bool {.magic: "LtStr", noSideEffect.}
  ## Compares two strings and returns true if `x` is lexicographically
  ## before `y` (uppercase letters come before lowercase letters).
  ##
  ## .. code-block:: Nim
  ##     let
  ##       a = "abc"
  ##       b = "abd"
  ##       c = "ZZZ"
  ##     assert a < b
  ##     assert (a < a) == false
  ##     assert (a < c) == false

proc len*(x: string): int {.magic: "LengthStr", noSideEffect.} =
  ## Returns the length of a string.
  ##
  ## .. code-block:: Nim
  ##   var str = "Hello world!"
  ##   echo len(str) # => 12
  discard

proc cmp*(x, y: string): int {.noSideEffect, procvar.} =
  ## Compare proc for strings. More efficient than the generic version.
  ##
  ## **Note**: The precise result values depend on the used C runtime library and
  ## can differ between operating systems!
  discard

proc `&`*(x: string, y: char): string {.
  magic: "ConStrStr", noSideEffect, merge.}
  ## Concatenates `x` with `y`.
  ##
  ## .. code-block:: Nim
  ##   assert("ab" & 'c' == "abc")

proc `&`*(x, y: char): string {.
  magic: "ConStrStr", noSideEffect, merge.}
  ## Concatenates characters `x` and `y` into a string.
  ##
  ## .. code-block:: Nim
  ##   assert('a' & 'b' == "ab")

proc `&`*(x, y: string): string {.
  magic: "ConStrStr", noSideEffect, merge.}
  ## Concatenates strings `x` and `y`.
  ##
  ## .. code-block:: Nim
  ##   assert("ab" & "cd" == "abcd")

proc `&`*(x: char, y: string): string {.
  magic: "ConStrStr", noSideEffect, merge.}
  ## Concatenates `x` with `y`.
  ##
  ## .. code-block:: Nim
  ##   assert('a' & "bc" == "abc")

proc add*(x: var string, y: char) {.magic: "AppendStrCh", noSideEffect.}
  ## Appends `y` to `x` in place.
  ##
  ## .. code-block:: Nim
  ##   var tmp = ""
  ##   tmp.add('a')
  ##   tmp.add('b')
  ##   assert(tmp == "ab")

proc add*(x: var string, y: string) {.magic: "AppendStrStr", noSideEffect.}
  ## Concatenates `x` and `y` in place.
  ##
  ## .. code-block:: Nim
  ##   var tmp = ""
  ##   tmp.add("ab")
  ##   tmp.add("cd")
  ##   assert(tmp == "abcd")

proc `&=`*(x: var string, y: string) {.magic: "AppendStrStr", noSideEffect.}
  ## Appends in place to a string.
  ##
  ## .. code-block:: Nim
  ##   var a = "abc"
  ##   a &= "de" # a <- "abcde"

proc setLen*(s: var string, newlen: Natural) {.
  magic: "SetLengthStr", noSideEffect.}
  ## Sets the length of string `s` to `newlen`.
  ##
  ## If the current length is greater than the new length,
  ## ``s`` will be truncated.
  ##
  ## .. code-block:: Nim
  ##  var myS = "Nim is great!!"
  ##  myS.setLen(3) # myS <- "Nim"
  ##  echo myS, " is fantastic!!"

proc substr*(s: string, first, last: int): string =
  ## Copies a slice of `s` into a new string and returns this new
  ## string.
  ##
  ## The bounds `first` and `last` denote the indices of
  ## the first and last characters that shall be copied. If ``last``
  ## is omitted, it is treated as ``high(s)``. If ``last >= s.len``, ``s.len``
  ## is used instead: This means ``substr`` can also be used to `cut`:idx:
  ## or `limit`:idx: a string's length.
  ##
  ## .. code-block:: Nim
  ##   let a = "abcdefgh"
  ##   assert a.substr(2, 5) == "cdef"
  ##   assert a.substr(2) == "cdefgh"
  ##   assert a.substr(5, 99) == "fgh"

proc substr*(s: string, first = 0): string = discard

proc high*(x: string): int {.magic: "High", noSideEffect.} =
  ## Returns the highest possible index of a string `x`.
  ##
  ## See also:
  ## * `low(string) <#low,string>`_
  ##
  ## .. code-block:: Nim
  ##  var str = "Hello world!"
  ##  high(str) # => 11
  discard

proc low*(x: string): int {.magic: "Low", noSideEffect.} =
  ## Returns the lowest possible index of a string `x`.
  ##
  ## See also:
  ## * `high(string) <#high,string>`_
  ##
  ## .. code-block:: Nim
  ##  var str = "Hello world!"
  ##  low(str) # => 0
  discard


template `[]`*(s: string; i: int): char = discard
template `[]=`*(s: string; i: int; val: char) = discard

proc `[]`*[T, U](s: string, x: HSlice[T, U]): string {.inline.} =
  ## Slice operation for strings.
  ## Returns the inclusive range `[s[x.a], s[x.b]]`:
  ##
  ## .. code-block:: Nim
  ##    var s = "abcdef"
  ##    assert s[1..3] == "bcd"
  discard

proc `[]=`*[T, U](s: var string, x: HSlice[T, U], b: string) =
  ## Slice assignment for strings.
  ##
  ## If ``b.len`` is not exactly the number of elements that are referred to
  ## by `x`, a `splice`:idx: is performed:
  ##
  ## .. code-block:: Nim
  ##   var s = "abcdefgh"
  ##   s[1 .. ^2] = "xyz"
  ##   assert s == "axyzh"
  discard

proc `[]`*(s: string; i: BackwardsIndex): char {.inline.} = discard

proc `[]=`*(s: var string; i: BackwardsIndex; x: char) {.inline.} =
  discard

proc insert*(x: var string, item: string, i = 0.Natural) {.noSideEffect.} =
  ## Inserts `item` into `x` at position `i`.
  ##
  ## .. code-block:: Nim
  ##   var a = "abc"
  ##   a.insert("zz", 0) # a <- "zzabc"
  discard

proc addEscapedChar*(s: var string, c: char) {.noSideEffect, inline.} =
  ## Adds a char to string `s` and applies the following escaping:
  ##
  ## * replaces any ``\`` by ``\\``
  ## * replaces any ``'`` by ``\'``
  ## * replaces any ``"`` by ``\"``
  ## * replaces any ``\a`` by ``\\a``
  ## * replaces any ``\b`` by ``\\b``
  ## * replaces any ``\t`` by ``\\t``
  ## * replaces any ``\n`` by ``\\n``
  ## * replaces any ``\v`` by ``\\v``
  ## * replaces any ``\f`` by ``\\f``
  ## * replaces any ``\c`` by ``\\c``
  ## * replaces any ``\e`` by ``\\e``
  ## * replaces any other character not in the set ``{'\21..'\126'}
  ##   by ``\xHH`` where ``HH`` is its hexadecimal value.
  ##
  ## The procedure has been designed so that its output is usable for many
  ## diferent common syntaxes.
  ##
  ## **Warning**: This is **not correct** for producing Ansi C code!
  discard

proc addQuoted*[T](s: var string, x: T) =
  ## Appends `x` to string `s` in place, applying quoting and escaping
  ## if `x` is a string or char.
  ##
  ## See `addEscapedChar <#addEscapedChar,string,char>`_
  ## for the escaping scheme. When `x` is a string, characters in the
  ## range ``{\128..\255}`` are never escaped so that multibyte UTF-8
  ## characters are untouched (note that this behavior is diferent from
  ## ``addEscapedChar``).
  ##
  ## The Nim standard library uses this function on the elements of
  ## collections when producing a string representation of a collection.
  ## It is recommended to use this function as well for user-side collections.
  ## Users may overload `addQuoted` for custom (string-like) types if
  ## they want to implement a customized element representation.
  ##
  ## .. code-block:: Nim
  ##   var tmp = ""
  ##   tmp.addQuoted(1)
  ##   tmp.add(", ")
  ##   tmp.addQuoted("string")
  ##   tmp.add(", ")
  ##   tmp.addQuoted('c')
  ##   assert(tmp == """1, "string", 'c'""")
  discard

proc shallow*(s: var string) {.noSideEffect, inline.} =
  ## Marks a string `s` as `shallow`:idx:. Subsequent assignments will not
  ## perform deep copies of `s`.
  ##
  ## This is only useful for optimization purposes.
  ##
  ## **Note:** This proc is not available for JS backend and nimscript.
  discard

proc newString*(len: Natural): string {.
  magic: "NewString", importc: "mnewString", noSideEffect.}
  ## Returns a new string of length ``len`` but with uninitialized
  ## content. One needs to fill the string character after character
  ## with the index operator ``s[i]``.
  ##
  ## This procedure exists only for optimization purposes;
  ## the same effect can be achieved with the ``&`` operator or with ``add``.

proc newStringOfCap*(cap: Natural): string {.
  magic: "NewStringOfCap", importc: "rawNewString", noSideEffect.}
  ## Returns a new string of length ``0`` but with capacity `cap`.
  ##
  ## This procedure exists only for optimization purposes; the same effect can
  ## be achieved with the ``&`` operator or with ``add``.


type TaintedString* = distinct string
  ## A distinct string type that is `tainted`:idx:, see `taint mode
  ## <manual.html#taint-mode>`_ for details.
  ##
  ## It is an alias for ``string`` if the taint mode is not turned on.

proc len*(s: TaintedString): int {.borrow.}


# cstrings

proc add*(x: var string, y: cstring) =
  discard

proc `==`*(x, y: cstring): bool {.
  magic: "EqCString", noSideEffect, inline.} =
  ## Checks for equality between two `cstring` variables.
  discard

proc high*(x: cstring): int {.magic: "High", noSideEffect.} =
  ## Returns the highest possible index of a compatible string `x`.
  ## This is sometimes an O(n) operation.
  ##
  ## See also:
  ## * `low(cstring) <#low,cstring>`_
  discard

proc low*(x: cstring): int {.magic: "Low", noSideEffect.} =
  ## Returns the lowest possible index of a compatible string `x`.
  ##
  ## See also:
  ## * `high(cstring) <#high,cstring>`_
  discard

proc len*(x: cstring): int {.magic: "LengthStr", noSideEffect.} =
  ## Returns the length of a compatible string. This is sometimes
  ## an O(n) operation.
  ##
  ## .. code-block:: Nim
  ##   var str: cstring = "Hello world!"
  ##   len(str) # => 12
  discard

proc cstringArrayToSeq*(a: cstringArray, len: Natural): seq[string] =
  ## Converts a ``cstringArray`` to a ``seq[string]``. `a` is supposed to be
  ## of length ``len``.
  ##
  ## **Note:** This proc is not available for JS backend.
  discard

proc cstringArrayToSeq*(a: cstringArray): seq[string] =
  ## Converts a ``cstringArray`` to a ``seq[string]``. `a` is supposed to be
  ## terminated by ``nil``.
  ##
  ## **Note:** This proc is not available for JS backend.
  discard



proc defined*(x: untyped): bool {.magic: "Defined", noSideEffect, compileTime.} =
  ## Special compile-time procedure that checks whether `x` is
  ## defined.
  ##
  ## `x` is an external symbol introduced through the compiler's
  ## `-d:x switch <nimc.html#compile-time-symbols>`_ to enable build time
  ## conditionals:
  ##
  ## .. code-block:: Nim
  ##   when not defined(release):
  ##     # Do here programmer friendly expensive sanity checks.
  ##   # Put here the normal code
  discard

proc runnableExamples*(body: untyped) {.magic: "RunnableExamples".} =
  ## A section you should use to mark `runnable example`:idx: code with.
  ##
  ## - In normal debug and release builds code within
  ##   a ``runnableExamples`` section is ignored.
  ## - The documentation generator is aware of these examples and considers them
  ##   part of the ``##`` doc comment. As the last step of documentation
  ##   generation the examples are put into an ``$file_example.nim`` file,
  ##   compiled and tested. The collected examples are
  ##   put into their own module to ensure the examples do not refer to
  ##   non-exported symbols.
  ##
  ## Usage:
  ##
  ## .. code-block:: Nim
  ##   proc double(x: int): int =
  ##     ## This proc doubles a number.
  ##     runnableExamples:
  ##       assert double(5) == 10
  ##       assert double(21) == 42
  ##
  ##     result = 2 * x
  discard

proc declared*(x: untyped): bool {.magic: "Defined", noSideEffect, compileTime.} =
  ## Special compile-time procedure that checks whether `x` is
  ## declared. `x` has to be an identifier or a qualified identifier.
  ##
  ## See also:
  ## * `declaredInScope <#declaredInScope,untyped>`_
  ##
  ## This can be used to check whether a library provides a certain
  ## feature or not:
  ##
  ## .. code-block:: Nim
  ##   when not declared(strutils.toUpper):
  ##     # provide our own toUpper proc here, because strutils is
  ##     # missing it.
  discard

when defined(useNimRtl):
  {.deadCodeElim: on.}  # dce option deprecated

proc declaredInScope*(x: untyped): bool {.
  magic: "DefinedInScope", noSideEffect, compileTime.} =
  ## Special compile-time procedure that checks whether `x` is
  ## declared in the current scope. `x` has to be an identifier.
  discard

proc `addr`*[T](x: var T): ptr T {.magic: "Addr", noSideEffect.} =
  ## Builtin `addr` operator for taking the address of a memory location.
  ## Cannot be overloaded.
  ##
  ## See also:
  ## * `unsafeAddr <#unsafeAddr,T>`_
  ##
  ## .. code-block:: Nim
  ##  var
  ##    buf: seq[char] = @['a','b','c']
  ##    p = buf[1].addr
  ##  echo p.repr # ref 0x7faa35c40059 --> 'b'
  ##  echo p[]    # b
  discard

proc unsafeAddr*[T](x: T): ptr T {.magic: "Addr", noSideEffect.} =
  ## Builtin `addr` operator for taking the address of a memory
  ## location. This works even for ``let`` variables or parameters
  ## for better interop with C and so it is considered even more
  ## unsafe than the ordinary `addr <#addr,T>`_.
  ##
  ## **Note**: When you use it to write a wrapper for a C library, you should
  ## always check that the original library does never write to data behind the
  ## pointer that is returned from this procedure.
  ##
  ## Cannot be overloaded.
  discard

type
  `static`*[T] {.magic: "Static".}
    ## Meta type representing all values that can be evaluated at compile-time.
    ##
    ## The type coercion ``static(x)`` can be used to force the compile-time
    ## evaluation of the given expression ``x``.

  `type`*[T] {.magic: "Type".}
    ## Meta type representing the type of all type values.
    ##
    ## The coercion ``type(x)`` can be used to obtain the type of the given
    ## expression ``x``.

type
  TypeOfMode* = enum ## Possible modes of `typeof`.
    typeOfProc,      ## Prefer the interpretation that means `x` is a proc call.
    typeOfIter       ## Prefer the interpretation that means `x` is an iterator call.

proc typeof*(x: untyped; mode = typeOfIter): typeDesc {.
  magic: "TypeOf", noSideEffect, compileTime.} =
  ## Builtin `typeof` operation for accessing the type of an expression.
  ## Since version 0.20.0.
  discard


proc new*[T](a: var ref T, finalizer: proc (x: ref T) {.nimcall.}) {.
  magic: "NewFinalize", noSideEffect.} =
  ## Creates a new object of type ``T`` and returns a safe (traced)
  ## reference to it in ``a``.
  ##
  ## When the garbage collector frees the object, `finalizer` is called.
  ## The `finalizer` may not keep a reference to the
  ## object pointed to by `x`. The `finalizer` cannot prevent the GC from
  ## freeing the object.
  ##
  ## **Note**: The `finalizer` refers to the type `T`, not to the object!
  ## This means that for each object of type `T` the finalizer will be called!
  discard

proc reset*[T](obj: var T) {.magic: "Reset", noSideEffect.} =
  ## Resets an object `obj` to its initial (binary zero) value.
  ##
  ## This needs to be called before any possible `object branch transition`:idx:.
  discard

proc wasMoved*[T](obj: var T) {.magic: "WasMoved", noSideEffect.} =
  ## Resets an object `obj` to its initial (binary zero) value to signify
  ## it was "moved" and to signify its destructor should do nothing and
  ## ideally be optimized away.
  discard

proc move*[T](x: var T): T {.magic: "Move", noSideEffect.} =
  result = x
  wasMoved(x)

type
  range*[T]{.magic: "Range".}         ## Generic type to construct range types.
  array*[I, T]{.magic: "Array".}      ## Generic type to construct
                                      ## fixed-length arrays.
  openArray*[T]{.magic: "OpenArray".} ## Generic type to construct open arrays.
                                      ## Open arrays are implemented as a
                                      ## pointer to the array data and a
                                      ## length field.
  UncheckedArray*[T]{.magic: "UncheckedArray".} ## Array with no bounds checking.
  varargs*[T]{.magic: "Varargs".}     ## Generic type to construct a varargs type.
  seq*[T]{.magic: "Seq".}             ## Generic type to construct sequences.
  set*[T]{.magic: "Set".}             ## Generic type to construct bit sets.

type sink*[T]{.magic: "BuiltinType".}
type lent*[T]{.magic: "BuiltinType".}


proc shallowCopy*[T](x: var T, y: T) {.noSideEffect, magic: "ShallowCopy".} =
  ## Use this instead of `=` for a `shallow copy`:idx:.
  ##
  ## The shallow copy only changes the semantics for sequences and strings
  ## (and types which contain those).
  ##
  ## Be careful with the changed semantics though!
  ## There is a reason why the default assignment does a deep copy of sequences
  ## and strings.
  discard

when defined(nimArrIdx):
  # :array|openarray|string|seq|cstring|tuple
  proc `[]`*[I: Ordinal;T](a: T; i: I): T {.
    noSideEffect, magic: "ArrGet".}
  proc `[]=`*[I: Ordinal;T,S](a: T; i: I;
    x: S) {.noSideEffect, magic: "ArrPut".}
  proc `=`*[T](dest: var T; src: T) {.noSideEffect, magic: "Asgn".}

  proc `=destroy`*[T](x: var T) {.inline, magic: "Destroy".} =
    ## Generic `destructor`:idx: implementation that can be overriden.
    discard
  proc `=sink`*[T](x: var T; y: T) {.inline, magic: "Asgn".} =
    ## Generic `sink`:idx: implementation that can be overriden.
    shallowCopy(x, y)

type
  HSlice*[T, U] = object   ## "Heterogenous" slice type.
    a*: T                  ## The lower bound (inclusive).
    b*: U                  ## The upper bound (inclusive).
  Slice*[T] = HSlice[T, T] ## An alias for ``HSlice[T, T]``.

proc `..`*[T, U](a: T, b: U): HSlice[T, U] {.noSideEffect, inline, magic: "DotDot".} =
  ## Binary `slice`:idx: operator that constructs an interval ``[a, b]``, both `a`
  ## and `b` are inclusive.
  ##
  ## Slices can also be used in the set constructor and in ordinal case
  ## statements, but then they are special-cased by the compiler.
  ##
  ## .. code-block:: Nim
  ##   let a = [10, 20, 30, 40, 50]
  ##   echo a[2 .. 3] # @[30, 40]
  result = HSlice[T, U](a: a, b: b)

proc `..`*[T](b: T): HSlice[int, T] {.noSideEffect, inline, magic: "DotDot".} =
  ## Unary `slice`:idx: operator that constructs an interval ``[default(int), b]``.
  ##
  ## .. code-block:: Nim
  ##   let a = [10, 20, 30, 40, 50]
  ##   echo a[.. 2] # @[10, 20, 30]
  result = HSlice[int, T](a: 0, b: b)


proc `==`*(x, y: pointer): bool {.magic: "EqRef", noSideEffect.} =
  ## .. code-block:: Nim
  ##  var # this is a wildly dangerous example
  ##    a = cast[pointer](0)
  ##    b = cast[pointer](nil)
  ##  echo (a == b) # true due to the special meaning of `nil`/0 as a pointer
  discard

proc `==`*[T](x, y: ref T): bool {.magic: "EqRef", noSideEffect.} =
  ## Checks that two `ref` variables refer to the same item.
  discard

proc `==`*[T](x, y: ptr T): bool {.magic: "EqRef", noSideEffect.} =
  ## Checks that two `ptr` variables refer to the same item.
  discard

proc `==`*[T: proc](x, y: T): bool {.magic: "EqProc", noSideEffect.} =
  ## Checks that two `proc` variables refer to the same procedure.
  discard

proc `<=`*[T](x, y: ref T): bool {.magic: "LePtr", noSideEffect.}

proc `<=`*(x, y: pointer): bool {.magic: "LePtr", noSideEffect.}

proc `<`*[T](x, y: ref T): bool {.magic: "LtPtr", noSideEffect.}

proc `<`*[T](x, y: ptr T): bool {.magic: "LtPtr", noSideEffect.}

proc `<`*(x, y: pointer): bool {.magic: "LtPtr", noSideEffect.}

template `!=`*(x, y: untyped): untyped =
  ## Unequals operator. This is a shorthand for ``not (x == y)``.
  not (x == y)

template `>=`*(x, y: untyped): untyped = discard
  ## "is greater or equals" operator. This is the same as ``y <= x``.

template `>`*(x, y: untyped): untyped = discard
  ## "is greater" operator. This is the same as ``y < x``.

const
  appType* {.magic: "AppType"}: string = ""
    ## A string that describes the application type. Possible values:
    ## `"console"`, `"gui"`, `"lib"`.

include "system/inclrtl"

const NoFakeVars* = defined(nimscript) ## `true` if the backend doesn't support \
  ## "fake variables" like `var EBADF {.importc.}: cint`.


type
  byte* = uint8 ## This is an alias for ``uint8``, that is an unsigned
                ## integer, 8 bits wide.

  Natural* = range[0..high(int)]
    ## This is an `int` type ranging from zero to the maximum value
    ## of an `int`. This type is often useful for documentation and debugging.

  Positive* = range[1..high(int)]
    ## This is an `int` type ranging from one to the maximum value
    ## of an `int`. This type is often useful for documentation and debugging.

  RootObj* {.compilerProc, inheritable.} =
    object ## The root of Nim's object hierarchy.
           ##
           ## Objects should inherit from `RootObj` or one of its descendants.
           ## However, objects that have no ancestor are also allowed.
  RootRef* = ref RootObj ## Reference to `RootObj`.

  RootEffect* {.compilerproc.} = object of RootObj ## \
    ## Base effect class.
    ##
    ## Each effect should inherit from `RootEffect` unless you know what
    ## you're doing.
  TimeEffect* = object of RootEffect   ## Time effect.
  IOEffect* = object of RootEffect     ## IO effect.
  ReadIOEffect* = object of IOEffect   ## Effect describing a read IO operation.
  WriteIOEffect* = object of IOEffect  ## Effect describing a write IO operation.
  ExecIOEffect* = object of IOEffect   ## Effect describing an executing IO operation.

  StackTraceEntry* = object ## In debug mode exceptions store the stack trace that led
                            ## to them. A `StackTraceEntry` is a single entry of the
                            ## stack trace.
    procname*: cstring      ## Name of the proc that is currently executing.
    line*: int              ## Line number of the proc that is currently executing.
    filename*: cstring      ## Filename of the proc that is currently executing.

  Exception* {.compilerproc, magic: "Exception".} = object of RootObj ## \
    ## Base exception class.
    ##
    ## Each exception has to inherit from `Exception`. See the full `exception
    ## hierarchy <manual.html#exception-handling-exception-hierarchy>`_.
    parent*: ref Exception ## Parent exception (can be used as a stack).
    name*: cstring         ## The exception's name is its Nim identifier.
                           ## This field is filled automatically in the
                           ## ``raise`` statement.
    msg* {.exportc: "message".}: string ## The exception's message. Not
                                        ## providing an exception message
                                        ## is bad style.
    when defined(js):
      trace: string
    else:
      trace: seq[StackTraceEntry]
    when defined(nimBoostrapCsources0_19_0):
      # see #10315, bootstrap with `nim cpp` from csources gave error:
      # error: no member named 'raise_id' in 'Exception'
      raise_id: uint # set when exception is raised
    else:
      raiseId: uint # set when exception is raised
    up: ref Exception # used for stacking exceptions. Not exported!

  Defect* = object of Exception ## \
    ## Abstract base class for all exceptions that Nim's runtime raises
    ## but that are strictly uncatchable as they can also be mapped to
    ## a ``quit`` / ``trap`` / ``exit`` operation.

  CatchableError* = object of Exception ## \
    ## Abstract class for all exceptions that are catchable.
  IOError* = object of CatchableError ## \
    ## Raised if an IO error occurred.
  EOFError* = object of IOError ## \
    ## Raised if an IO "end of file" error occurred.
  OSError* = object of CatchableError ## \
    ## Raised if an operating system service failed.
    errorCode*: int32 ## OS-defined error code describing this error.
  LibraryError* = object of OSError ## \
    ## Raised if a dynamic library could not be loaded.
  ResourceExhaustedError* = object of CatchableError ## \
    ## Raised if a resource request could not be fulfilled.
  ArithmeticError* = object of Defect ## \
    ## Raised if any kind of arithmetic error occurred.
  DivByZeroError* = object of ArithmeticError ## \
    ## Raised for runtime integer divide-by-zero errors.

  OverflowError* = object of ArithmeticError ## \
    ## Raised for runtime integer overflows.
    ##
    ## This happens for calculations whose results are too large to fit in the
    ## provided bits.
  AccessViolationError* = object of Defect ## \
    ## Raised for invalid memory access errors
  AssertionError* = object of Defect ## \
    ## Raised when assertion is proved wrong.
    ##
    ## Usually the result of using the `assert() template <#assert>`_.
  ValueError* = object of CatchableError ## \
    ## Raised for string and object conversion errors.
  KeyError* = object of ValueError ## \
    ## Raised if a key cannot be found in a table.
    ##
    ## Mostly used by the `tables <tables.html>`_ module, it can also be raised
    ## by other collection modules like `sets <sets.html>`_ or `strtabs
    ## <strtabs.html>`_.
  OutOfMemError* = object of Defect ## \
    ## Raised for unsuccessful attempts to allocate memory.
  IndexError* = object of Defect ## \
    ## Raised if an array index is out of bounds.

  FieldError* = object of Defect ## \
    ## Raised if a record field is not accessible because its dicriminant's
    ## value does not fit.
  RangeError* = object of Defect ## \
    ## Raised if a range check error occurred.
  StackOverflowError* = object of Defect ## \
    ## Raised if the hardware stack used for subroutine calls overflowed.
  ReraiseError* = object of Defect ## \
    ## Raised if there is no exception to reraise.
  ObjectAssignmentError* = object of Defect ## \
    ## Raised if an object gets assigned to its parent's object.
  ObjectConversionError* = object of Defect ## \
    ## Raised if an object is converted to an incompatible object type.
    ## You can use ``of`` operator to check if conversion will succeed.
  FloatingPointError* = object of Defect ## \
    ## Base class for floating point exceptions.
  FloatInvalidOpError* = object of FloatingPointError ## \
    ## Raised by invalid operations according to IEEE.
    ##
    ## Raised by ``0.0/0.0``, for example.
  FloatDivByZeroError* = object of FloatingPointError ## \
    ## Raised by division by zero.
    ##
    ## Divisor is zero and dividend is a finite nonzero number.
  FloatOverflowError* = object of FloatingPointError ## \
    ## Raised for overflows.
    ##
    ## The operation produced a result that exceeds the range of the exponent.
  FloatUnderflowError* = object of FloatingPointError ## \
    ## Raised for underflows.
    ##
    ## The operation produced a result that is too small to be represented as a
    ## normal number.
  FloatInexactError* = object of FloatingPointError ## \
    ## Raised for inexact results.
    ##
    ## The operation produced a result that cannot be represented with infinite
    ## precision -- for example: ``2.0 / 3.0, log(1.1)``
    ##
    ## **Note**: Nim currently does not detect these!
  DeadThreadError* = object of Defect ## \
    ## Raised if it is attempted to send a message to a dead thread.
  NilAccessError* = object of Defect ## \
    ## Raised on dereferences of ``nil`` pointers.
    ##
    ## This is only raised if the `segfaults module <segfaults.html>`_ was imported!

when defined(js) or defined(nimdoc):
  type
    JsRoot* = ref object of RootObj
      ## Root type of the JavaScript object hierarchy

proc unsafeNew*[T](a: var ref T, size: Natural) {.magic: "New", noSideEffect.} =
  ## Creates a new object of type ``T`` and returns a safe (traced)
  ## reference to it in ``a``.
  ##
  ## This is **unsafe** as it allocates an object of the passed ``size``.
  ## This should only be used for optimization purposes when you know
  ## what you're doing!
  ##
  ## See also:
  ## * `new <#new,ref.T,proc(ref.T)>`_
  discard

proc sizeof*[T](x: T): int {.magic: "SizeOf", noSideEffect.} =
  ## Returns the size of ``x`` in bytes.
  ##
  ## Since this is a low-level proc,
  ## its usage is discouraged - using `new <#new,ref.T,proc(ref.T)>`_ for
  ## the most cases suffices that one never needs to know ``x``'s size.
  ##
  ## As a special semantic rule, ``x`` may also be a type identifier
  ## (``sizeof(int)`` is valid).
  ##
  ## Limitations: If used for types that are imported from C or C++,
  ## sizeof should fallback to the ``sizeof`` in the C compiler. The
  ## result isn't available for the Nim compiler and therefore can't
  ## be used inside of macros.
  ##
  ## .. code-block:: Nim
  ##  sizeof('A') # => 1
  ##  sizeof(2) # => 8
  discard

when defined(nimHasalignOf):
  proc alignof*[T](x: T): int {.magic: "AlignOf", noSideEffect.}
  proc alignof*(x: typedesc): int {.magic: "AlignOf", noSideEffect.}
  template offsetOf*[T](t: typedesc[T]; member: untyped): int =
    discard
  template offsetOf*[T](value: T; member: untyped): int =
    discard


when defined(nimtypedescfixed):
  proc sizeof*(x: typedesc): int {.magic: "SizeOf", noSideEffect.}


proc contains*[U, V, W](s: HSlice[U, V], value: W): bool {.noSideEffect, inline.} =
  ## Checks if `value` is within the range of `s`; returns true if
  ## `value >= s.a and value <= s.b`
  ##
  ## This allows the `in` operator: `contains(a..b, x)` is the same as
  ## `x in a..b`.
  ##
  ## .. code-block:: Nim
  ##   assert((1..3).contains(1) == true)
  ##   assert((1..3).contains(2) == true)
  ##   assert((1..3).contains(4) == false)
  discard

template `in`*(x, y: untyped): untyped {.dirty.} = discard
  ## Sugar for `contains`.
  ##
  ## .. code-block:: Nim
  ##   assert(1 in (1..3) == true)
  ##   assert(5 in (1..3) == false)

template `notin`*(x, y: untyped): untyped {.dirty.} = discard
  ## Sugar for `not contains`.
  ##
  ## .. code-block:: Nim
  ##   assert(1 notin (1..3) == false)
  ##   assert(5 notin (1..3) == true)

proc `is`*[T, S](x: T, y: S): bool {.magic: "Is", noSideEffect.} =
  ## Checks if `T` is of the same type as `S`.
  ##
  ## For a negated version, use `isnot <#isnot.t,untyped,untyped>`_.
  ##
  ## .. code-block:: Nim
  ##   assert 42 is int
  ##   assert @[1, 2] is seq
  ##
  ##   proc test[T](a: T): int =
  ##     when (T is int):
  ##       return a
  ##     else:
  ##       return 0
  ##
  ##   assert(test[int](3) == 3)
  ##   assert(test[string]("xyz") == 0)
  discard

template `isnot`*(x, y: untyped): untyped = discard
  ## Negated version of `is <#is,T,S>`_. Equivalent to ``not(x is y)``.
  ##
  ## .. code-block:: Nim
  ##   assert 42 isnot float
  ##   assert @[1, 2] isnot enum

type owned*[T]{.magic: "BuiltinType".}

proc new*[T](a: var owned(ref T)) {.magic: "New", noSideEffect.} =
  ## Creates a new object of type ``T`` and returns a safe (traced)
  ## reference to it in ``a``.
  discard

proc new*(t: typedesc): auto =
  ## Creates a new object of type ``T`` and returns a safe (traced)
  ## reference to it as result value.
  ##
  ## When ``T`` is a ref type then the resulting type will be ``T``,
  ## otherwise it will be ``ref T``.
  when (t is ref):
    var r: owned t
  else:
    var r: owned(ref t)
  new(r)
  return r


proc `of`*[T, S](x: typeDesc[T], y: typeDesc[S]): bool {.magic: "Of", noSideEffect.}
proc `of`*[T, S](x: T, y: typeDesc[S]): bool {.magic: "Of", noSideEffect.}
proc `of`*[T, S](x: T, y: S): bool {.magic: "Of", noSideEffect.} =
  ## Checks if `x` has a type of `y`.
  ##
  ## .. code-block:: Nim
  ##   assert(FloatingPointError of Exception)
  ##   assert(DivByZeroError of Exception)
  discard

proc cmp*[T](x, y: T): int {.procvar.} =
  ## Generic compare proc.
  ##
  ## Returns:
  ## * a value less than zero, if `x < y`
  ## * a value greater than zero, if `x > y`
  ## * zero, if `x == y`
  ##
  ## This is useful for writing generic algorithms without performance loss.
  ## This generic implementation uses the `==` and `<` operators.
  ##
  ## .. code-block:: Nim
  ##  import algorithm
  ##  echo sorted(@[4, 2, 6, 5, 8, 7], cmp[int])
  discard


type
  Endianness* = enum ## Type describing the endianness of a processor.
    littleEndian, bigEndian

const
  isMainModule* {.magic: "IsMainModule".}: bool = false
    ## True only when accessed in the main module. This works thanks to
    ## compiler magic. It is useful to embed testing code in a module.

  CompileDate* {.magic: "CompileDate"}: string = "0000-00-00"
    ## The date (in UTC) of compilation as a string of the form
    ## ``YYYY-MM-DD``. This works thanks to compiler magic.

  CompileTime* {.magic: "CompileTime"}: string = "00:00:00"
    ## The time (in UTC) of compilation as a string of the form
    ## ``HH:MM:SS``. This works thanks to compiler magic.

  cpuEndian* {.magic: "CpuEndian"}: Endianness = littleEndian
    ## The endianness of the target CPU. This is a valuable piece of
    ## information for low-level code only. This works thanks to compiler
    ## magic.

  hostOS* {.magic: "HostOS".}: string = ""
    ## A string that describes the host operating system.
    ##
    ## Possible values:
    ## `"windows"`, `"macosx"`, `"linux"`, `"netbsd"`, `"freebsd"`,
    ## `"openbsd"`, `"solaris"`, `"aix"`, `"haiku"`, `"standalone"`.

  hostCPU* {.magic: "HostCPU".}: string = ""
    ## A string that describes the host CPU.
    ##
    ## Possible values:
    ## `"i386"`, `"alpha"`, `"powerpc"`, `"powerpc64"`, `"powerpc64el"`,
    ## `"sparc"`, `"amd64"`, `"mips"`, `"mipsel"`, `"arm"`, `"arm64"`,
    ## `"mips64"`, `"mips64el"`, `"riscv64"`.


{.push profiler: off.}
let nimvm* {.magic: "Nimvm", compileTime.}: bool = false
  ## May be used only in `when` expression.
  ## It is true in Nim VM context and false otherwise.
{.pop.}

proc compileOption*(option: string): bool {.
  magic: "CompileOption", noSideEffect.}
  ## Can be used to determine an `on|off` compile-time option. Example:
  ##
  ## .. code-block:: Nim
  ##   when compileOption("floatchecks"):
  ##     echo "compiled with floating point NaN and Inf checks"

proc compileOption*(option, arg: string): bool {.
  magic: "CompileOptionArg", noSideEffect.}
  ## Can be used to determine an enum compile-time option. Example:
  ##
  ## .. code-block:: Nim
  ##   when compileOption("opt", "size") and compileOption("gc", "boehm"):
  ##     echo "compiled with optimization for size and uses Boehm's GC"

const
  nimEnableCovariance* = defined(nimEnableCovariance) # or true


const
  QuitSuccess* = 0
    ## The value that should be passed to `quit <#quit>`_ to indicate
    ## success.

  QuitFailure* = 1
    ## The value that should be passed to `quit <#quit>`_ to indicate
    ## failure.

var programResult* {.compilerproc, exportc: "nim_program_result".}: int
  ## Modify this variable to specify the exit code of the program
  ## under normal circumstances. When the program is terminated
  ## prematurely using `quit proc <#quit,int>`_, this value is ignored.

proc quit*(errorcode: int = QuitSuccess) {.magic: "Exit", noreturn.} =
  ## Stops the program immediately with an exit code.
  ##
  ## Before stopping the program the "quit procedures" are called in the
  ## opposite order they were added with `addQuitProc <#addQuitProc,proc>`_.
  ## ``quit`` never returns and ignores any exception that may have been raised
  ## by the quit procedures.  It does *not* call the garbage collector to free
  ## all the memory, unless a quit procedure calls `GC_fullCollect
  ## <#GC_fullCollect>`_.
  ##
  ## The proc ``quit(QuitSuccess)`` is called implicitly when your nim
  ## program finishes without incident for platforms where this is the
  ## expected behavior. A raised unhandled exception is
  ## equivalent to calling ``quit(QuitFailure)``.
  ##
  ## Note that this is a *runtime* call and using ``quit`` inside a macro won't
  ## have any compile time effect. If you need to stop the compiler inside a
  ## macro, use the `error <manual.html#pragmas-error-pragma>`_ or `fatal
  ## <manual.html#pragmas-fatal-pragma>`_ pragmas.
  discard


const hasAlloc = (hostOS != "standalone" or not defined(nogc)) and not defined(nimscript)


proc repr*[T](x: T): string {.magic: "Repr", noSideEffect.} =
  ## Takes any Nim variable and returns its string representation.
  ##
  ## It works even for complex data graphs with cycles. This is a great
  ## debugging tool.
  ##
  ## .. code-block:: Nim
  ##  var s: seq[string] = @["test2", "test2"]
  ##  var i = @[1, 2, 3, 4, 5]
  ##  echo repr(s) # => 0x1055eb050[0x1055ec050"test2", 0x1055ec078"test2"]
  ##  echo repr(i) # => 0x1055ed050[1, 2, 3, 4, 5]
  discard

type
  ByteAddress* = int
    ## is the signed integer type that should be used for converting
    ## pointers to integer addresses for readability.

  BiggestInt* = int64
    ## is an alias for the biggest signed integer type the Nim compiler
    ## supports. Currently this is ``int64``, but it is platform-dependant
    ## in general.

  BiggestFloat* = float64
    ## is an alias for the biggest floating point type the Nim
    ## compiler supports. Currently this is ``float64``, but it is
    ## platform-dependant in general.

type BiggestUInt* = uint64
  ## is an alias for the biggest unsigned integer type the Nim compiler
  ## supports. Currently this is ``uint32`` for JS and ``uint64`` for other
  ## targets.

when defined(windows):
  type
    clong* {.importc: "long", nodecl.} = int32
      ## This is the same as the type ``long`` in *C*.
    culong* {.importc: "unsigned long", nodecl.} = uint32
      ## This is the same as the type ``unsigned long`` in *C*.
else:
  type
    clong* {.importc: "long", nodecl.} = int
      ## This is the same as the type ``long`` in *C*.
    culong* {.importc: "unsigned long", nodecl.} = uint
      ## This is the same as the type ``unsigned long`` in *C*.

type # these work for most platforms:
  cchar* {.importc: "char", nodecl.} = char
    ## This is the same as the type ``char`` in *C*.
  cschar* {.importc: "signed char", nodecl.} = int8
    ## This is the same as the type ``signed char`` in *C*.
  cshort* {.importc: "short", nodecl.} = int16
    ## This is the same as the type ``short`` in *C*.
  cint* {.importc: "int", nodecl.} = int32
    ## This is the same as the type ``int`` in *C*.
  csize* {.importc: "size_t", nodecl.} = int
    ## This is the same as the type ``size_t`` in *C*.
  clonglong* {.importc: "long long", nodecl.} = int64
    ## This is the same as the type ``long long`` in *C*.
  cfloat* {.importc: "float", nodecl.} = float32
    ## This is the same as the type ``float`` in *C*.
  cdouble* {.importc: "double", nodecl.} = float64
    ## This is the same as the type ``double`` in *C*.
  clongdouble* {.importc: "long double", nodecl.} = BiggestFloat
    ## This is the same as the type ``long double`` in *C*.
    ## This C type is not supported by Nim's code generator.

  cuchar* {.importc: "unsigned char", nodecl.} = char
    ## This is the same as the type ``unsigned char`` in *C*.
  cushort* {.importc: "unsigned short", nodecl.} = uint16
    ## This is the same as the type ``unsigned short`` in *C*.
  cuint* {.importc: "unsigned int", nodecl.} = uint32
    ## This is the same as the type ``unsigned int`` in *C*.
  culonglong* {.importc: "unsigned long long", nodecl.} = uint64
    ## This is the same as the type ``unsigned long long`` in *C*.

  cstringArray* {.importc: "char**", nodecl.} = ptr UncheckedArray[cstring]
    ## This is binary compatible to the type ``char**`` in *C*. The array's
    ## high value is large enough to disable bounds checking in practice.
    ## Use `cstringArrayToSeq proc <#cstringArrayToSeq,cstringArray,Natural>`_
    ## to convert it into a ``seq[string]``.

  PFloat32* = ptr float32    ## An alias for ``ptr float32``.
  PFloat64* = ptr float64    ## An alias for ``ptr float64``.
  PInt64* = ptr int64        ## An alias for ``ptr int64``.
  PInt32* = ptr int32        ## An alias for ``ptr int32``.

proc addQuitProc*(quitProc: proc() {.noconv.}) {.
  importc: "atexit", header: "<stdlib.h>".} =
  ## Adds/registers a quit procedure.
  ##
  ## Each call to ``addQuitProc`` registers another quit procedure. Up to 30
  ## procedures can be registered. They are executed on a last-in, first-out
  ## basis (that is, the last function registered is the first to be executed).
  ## ``addQuitProc`` raises an EOutOfIndex exception if ``quitProc`` cannot be
  ## registered.
  discard

# Support for addQuitProc() is done by Ansi C's facilities here.
# In case of an unhandled exeption the exit handlers should
# not be called explicitly! The user may decide to do this manually though.

proc zeroMem*(p: pointer, size: Natural) {.inline, benign.} =
  ## Overwrites the contents of the memory at ``p`` with the value 0.
  ##
  ## Exactly ``size`` bytes will be overwritten. Like any procedure
  ## dealing with raw memory this is **unsafe**.
  ##
  ## **Note:** This proc is not available for JS backend and nimscript.
  discard

proc copyMem*(dest, source: pointer, size: Natural) {.inline, benign,
  tags: [], locks: 0.} =
  ## Copies the contents from the memory at ``source`` to the memory
  ## at ``dest``.
  ## Exactly ``size`` bytes will be copied. The memory
  ## regions may not overlap. Like any procedure dealing with raw
  ## memory this is **unsafe**.
  ##
  ## **Note:** This proc is not available for JS backend and nimscript.
  discard

proc moveMem*(dest, source: pointer, size: Natural) {.inline, benign,
  tags: [], locks: 0.} =
  ## Copies the contents from the memory at ``source`` to the memory
  ## at ``dest``.
  ##
  ## Exactly ``size`` bytes will be copied. The memory
  ## regions may overlap, ``moveMem`` handles this case appropriately
  ## and is thus somewhat more safe than ``copyMem``. Like any procedure
  ## dealing with raw memory this is still **unsafe**, though.
  ##
  ## **Note:** This proc is not available for JS backend and nimscript.
  discard

proc equalMem*(a, b: pointer, size: Natural): bool {.
  inline, noSideEffect, tags: [], locks: 0.} =
  ## Compares the memory blocks ``a`` and ``b``. ``size`` bytes will
  ## be compared.
  ##
  ## If the blocks are equal, `true` is returned, `false`
  ## otherwise. Like any procedure dealing with raw memory this is
  ## **unsafe**.
  ##
  ## **Note:** This proc is not available for JS backend and nimscript.
  discard

proc alloc*(size: Natural): pointer {.noconv, rtl, tags: [], benign, raises: [].} =
  ## Allocates a new memory block with at least ``size`` bytes.
  ##
  ## The block has to be freed with `realloc(block, 0) <#realloc,pointer,Natural>`_
  ## or `dealloc(block) <#dealloc,pointer>`_.
  ## The block is not initialized, so reading
  ## from it before writing to it is undefined behaviour!
  ##
  ## The allocated memory belongs to its allocating thread!
  ## Use `allocShared <#allocShared,Natural>`_ to allocate from a shared heap.
  ##
  ## **Note:** Thif proc is not available for nimscript.
  ##
  ## See also:
  ## * `alloc0 <#alloc0,Natural>`_
  discard

proc createU*(T: typedesc, size = 1.Positive): ptr T {.inline, benign, raises: [].} =
  ## Allocates a new memory block with at least ``T.sizeof * size`` bytes.
  ##
  ## The block has to be freed with `resize(block, 0) <#resize,ptr.T,Natural>`_
  ## or `dealloc(block) <#dealloc,pointer>`_.
  ## The block is not initialized, so reading
  ## from it before writing to it is undefined behaviour!
  ##
  ## The allocated memory belongs to its allocating thread!
  ## Use `createSharedU <#createSharedU,typedesc>`_ to allocate from a shared heap.
  ##
  ## **Note:** Thif proc is not available for nimscript.
  ##
  ## See also:
  ## * `create <#create,typedesc>`_
  discard

proc alloc0*(size: Natural): pointer {.noconv, rtl, tags: [], benign, raises: [].} =
  ## Allocates a new memory block with at least ``size`` bytes.
  ##
  ## The block has to be freed with `realloc(block, 0) <#realloc,pointer,Natural>`_
  ## or `dealloc(block) <#dealloc,pointer>`_.
  ## The block is initialized with all bytes containing zero, so it is
  ## somewhat safer than  `alloc <#alloc,Natural>`_.
  ##
  ## The allocated memory belongs to its allocating thread!
  ## Use `allocShared0 <#allocShared0,Natural>`_ to allocate from a shared heap.
  ##
  ## **Note:** Thif proc is not available for nimscript.
  discard

proc create*(T: typedesc, size = 1.Positive): ptr T {.inline, benign, raises: [].} =
  ## Allocates a new memory block with at least ``T.sizeof * size`` bytes.
  ##
  ## The block has to be freed with `resize(block, 0) <#resize,ptr.T,Natural>`_
  ## or `dealloc(block) <#dealloc,pointer>`_.
  ## The block is initialized with all bytes containing zero, so it is
  ## somewhat safer than `createU <#createU,typedesc>`_.
  ##
  ## The allocated memory belongs to its allocating thread!
  ## Use `createShared <#createShared,typedesc>`_ to allocate from a shared heap.
  ##
  ## **Note:** Thif proc is not available for nimscript.
  discard

proc realloc*(p: pointer, newSize: Natural): pointer {.noconv, rtl, tags: [],
                                                       benign, raises: [].} =
  ## Grows or shrinks a given memory block.
  ##
  ## If `p` is **nil** then a new memory block is returned.
  ## In either way the block has at least ``newSize`` bytes.
  ## If ``newSize == 0`` and `p` is not **nil** ``realloc`` calls ``dealloc(p)``.
  ## In other cases the block has to be freed with
  ## `dealloc(block) <#dealloc,pointer>`_.
  ##
  ## The allocated memory belongs to its allocating thread!
  ## Use `reallocShared <#reallocShared,pointer,Natural>`_ to reallocate
  ## from a shared heap.
  ##
  ## **Note:** Thif proc is not available for nimscript.
  discard

proc resize*[T](p: ptr T, newSize: Natural): ptr T {.inline, benign, raises: [].} =
  ## Grows or shrinks a given memory block.
  ##
  ## If `p` is **nil** then a new memory block is returned.
  ## In either way the block has at least ``T.sizeof * newSize`` bytes.
  ## If ``newSize == 0`` and `p` is not **nil** ``resize`` calls ``dealloc(p)``.
  ## In other cases the block has to be freed with ``free``.
  ##
  ## The allocated memory belongs to its allocating thread!
  ## Use `resizeShared <#resizeShared,ptr.T,Natural>`_ to reallocate
  ## from a shared heap.
  ##
  ## **Note:** Thif proc is not available for nimscript.
  discard

proc dealloc*(p: pointer) {.noconv, rtl, tags: [], benign, raises: [].} =
  ## Frees the memory allocated with ``alloc``, ``alloc0`` or
  ## ``realloc``.
  ##
  ## **This procedure is dangerous!**
  ## If one forgets to free the memory a leak occurs; if one tries to
  ## access freed memory (or just freeing it twice!) a core dump may happen
  ## or other memory may be corrupted.
  ##
  ## The freed memory must belong to its allocating thread!
  ## Use `deallocShared <#deallocShared,pointer>`_ to deallocate from a shared heap.
  ##
  ## **Note:** Thif proc is not available for nimscript.
  discard

proc allocShared*(size: Natural): pointer {.noconv, rtl, benign, raises: [].} =
  ## Allocates a new memory block on the shared heap with at
  ## least ``size`` bytes.
  ##
  ## The block has to be freed with
  ## `reallocShared(block, 0) <#reallocShared,pointer,Natural>`_
  ## or `deallocShared(block) <#deallocShared,pointer>`_.
  ##
  ## The block is not initialized, so reading from it before writing
  ## to it is undefined behaviour!
  ##
  ## **Note:** Thif proc is not available for nimscript.
  ##
  ## See also:
  ## `allocShared0 <#allocShared0,Natural>`_.
  discard

proc createSharedU*(T: typedesc, size = 1.Positive): ptr T {.inline,
                                                             benign, raises: [].} =
  ## Allocates a new memory block on the shared heap with at
  ## least ``T.sizeof * size`` bytes.
  ##
  ## The block has to be freed with
  ## `resizeShared(block, 0) <#resizeShared,ptr.T,Natural>`_ or
  ## `freeShared(block) <#freeShared,ptr.T>`_.
  ##
  ## The block is not initialized, so reading from it before writing
  ## to it is undefined behaviour!
  ##
  ## **Note:** Thif proc is not available for nimscript.
  ##
  ## See also:
  ## * `createShared <#createShared,typedesc>`_
  discard

proc allocShared0*(size: Natural): pointer {.noconv, rtl, benign, raises: [].} =
  ## Allocates a new memory block on the shared heap with at
  ## least ``size`` bytes.
  ##
  ## The block has to be freed with
  ## `reallocShared(block, 0) <#reallocShared,pointer,Natural>`_
  ## or `deallocShared(block) <#deallocShared,pointer>`_.
  ##
  ## The block is initialized with all bytes
  ## containing zero, so it is somewhat safer than
  ## `allocShared <#allocShared,Natural>`_.
  ##
  ## **Note:** Thif proc is not available for nimscript.
  discard

proc createShared*(T: typedesc, size = 1.Positive): ptr T {.inline.} =
  ## Allocates a new memory block on the shared heap with at
  ## least ``T.sizeof * size`` bytes.
  ##
  ## The block has to be freed with
  ## `resizeShared(block, 0) <#resizeShared,ptr.T,Natural>`_ or
  ## `freeShared(block) <#freeShared,ptr.T>`_.
  ##
  ## The block is initialized with all bytes
  ## containing zero, so it is somewhat safer than
  ## `createSharedU <#createSharedU,typedesc>`_.
  ##
  ## **Note:** Thif proc is not available for nimscript.
  discard

proc reallocShared*(p: pointer, newSize: Natural): pointer {.noconv, rtl,
                                                             benign, raises: [].} =
  ## Grows or shrinks a given memory block on the heap.
  ##
  ## If `p` is **nil** then a new memory block is returned.
  ## In either way the block has at least ``newSize`` bytes.
  ## If ``newSize == 0`` and `p` is not **nil** ``reallocShared`` calls
  ## ``deallocShared(p)``.
  ## In other cases the block has to be freed with
  ## `deallocShared <#deallocShared,pointer>`_.
  ##
  ## **Note:** Thif proc is not available for nimscript.
  discard

proc resizeShared*[T](p: ptr T, newSize: Natural): ptr T {.inline, raises: [].} =
  ## Grows or shrinks a given memory block on the heap.
  ##
  ## If `p` is **nil** then a new memory block is returned.
  ## In either way the block has at least ``T.sizeof * newSize`` bytes.
  ## If ``newSize == 0`` and `p` is not **nil** ``resizeShared`` calls
  ## ``freeShared(p)``.
  ## In other cases the block has to be freed with
  ## `freeShared <#freeShared,ptr.T>`_.
  ##
  ## **Note:** Thif proc is not available for nimscript.
  discard

proc deallocShared*(p: pointer) {.noconv, rtl, benign, raises: [].} =
  ## Frees the memory allocated with ``allocShared``, ``allocShared0`` or
  ## ``reallocShared``.
  ##
  ## **This procedure is dangerous!**
  ## If one forgets to free the memory a leak occurs; if one tries to
  ## access freed memory (or just freeing it twice!) a core dump may happen
  ## or other memory may be corrupted.
  ##
  ## **Note:** Thif proc is not available for nimscript.
  discard

proc freeShared*[T](p: ptr T) {.inline, benign, raises: [].} =
  ## Frees the memory allocated with ``createShared``, ``createSharedU`` or
  ## ``resizeShared``.
  ##
  ## **This procedure is dangerous!**
  ## If one forgets to free the memory a leak occurs; if one tries to
  ## access freed memory (or just freeing it twice!) a core dump may happen
  ## or other memory may be corrupted.
  ##
  ## **Note:** Thif proc is not available for nimscript.
  discard

proc swap*[T](a, b: var T) {.magic: "Swap", noSideEffect.} =
  ## Swaps the values `a` and `b`.
  ##
  ## This is often more efficient than ``tmp = a; a = b; b = tmp``.
  ## Particularly useful for sorting algorithms.
  ##
  ## .. code-block:: Nim
  ##   var
  ##     a = 5
  ##     b = 9
  ##
  ##   swap(a, b)
  ##
  ##   assert a == 9
  ##   assert b == 5
  discard

when not defined(js) and not defined(booting) and defined(nimTrMacros):
  template swapRefsInArray*{swap(arr[a], arr[b])}(arr: openarray[ref], a, b: int) =
    # Optimize swapping of array elements if they are refs. Default swap
    # implementation will cause unsureAsgnRef to be emitted which causes
    # unnecessary slow down in this case.
    swap(cast[ptr pointer](addr arr[a])[], cast[ptr pointer](addr arr[b])[])


const
  Inf* = 0x7FF0000000000000'f64
    ## Contains the IEEE floating point value of positive infinity.
  NegInf* = 0xFFF0000000000000'f64
    ## Contains the IEEE floating point value of negative infinity.
  NaN* = 0x7FF7FFFFFFFFFFFF'f64
    ## Contains an IEEE floating point value of *Not A Number*.
    ##
    ## Note that you cannot compare a floating point value to this value
    ## and expect a reasonable result - use the `classify` procedure
    ## in the `math module <math.html>`_ for checking for NaN.

# GC interface:

proc getOccupiedMem*(): int {.rtl.} =
  ## Returns the number of bytes that are owned by the process and hold data.
  ##
  ## **Note:** Thif proc is not available for nimscript.
  discard

proc getFreeMem*(): int {.rtl.} =
  ## Returns the number of bytes that are owned by the process, but do not
  ## hold any meaningful data.
  ##
  ## **Note:** Thif proc is not available for nimscript.
  discard

proc getTotalMem*(): int {.rtl.} =
  ## Returns the number of bytes that are owned by the process.
  ##
  ## **Note:** Thif proc is not available for nimscript.
  discard

proc getOccupiedSharedMem*(): int {.rtl.} =
  ## Returns the number of bytes that are owned by the process
  ## on the shared heap and hold data. This is only available when
  ## threads are enabled.
  ##
  ## **Note:** Thif proc is not available for nimscript.
  discard

proc getFreeSharedMem*(): int {.rtl.} =
  ## Returns the number of bytes that are owned by the
  ## process on the shared heap, but do not hold any meaningful data.
  ## This is only available when threads are enabled.
  ##
  ## **Note:** Thif proc is not available for nimscript.
  discard

proc getTotalSharedMem*(): int {.rtl.} =
  ## Returns the number of bytes on the shared heap that are owned by the
  ## process. This is only available when threads are enabled.
  ##
  ## **Note:** Thif proc is not available for nimscript.
  discard


iterator countdown*[T](a, b: T, step: Positive = 1): T {.inline.} = discard
  ## Counts from ordinal value `a` down to `b` (inclusive) with the given
  ## step count.
  ##
  ## `T` may be any ordinal type, `step` may only be positive.
  ##
  ## **Note**: This fails to count to ``low(int)`` if T = int for
  ## efficiency reasons.
  ##
  ## .. code-block:: Nim
  ##   for i in countdown(7, 3):
  ##     echo i # => 7; 6; 5; 4; 3
  ##
  ##   for i in countdown(9, 2, 3):
  ##     echo i # => 9; 6; 3

iterator countup*[T](a, b: T, step: Positive = 1): T {.inline.} = discard
  ## Counts from ordinal value `a` to `b` (inclusive) with the given
  ## step count.
  ##
  ## `T` may be any ordinal type, `step` may only be positive.
  ##
  ## **Note**: This fails to count to ``high(int)`` if T = int for
  ## efficiency reasons.
  ##
  ## .. code-block:: Nim
  ##   for i in countup(3, 7):
  ##     echo i # => 3; 4; 5; 6; 7
  ##
  ##   for i in countup(2, 9, 3):
  ##     echo i # => 2; 5; 8

iterator `..`*[T](a, b: T): T {.inline.} = discard
  ## An alias for `countup(a, b, 1)`.
  ##
  ## See also:
  ## * [..<](#..<.i,T,T)
  ##
  ## .. code-block:: Nim
  ##   for i in 3 .. 7:
  ##     echo i # => 3; 4; 5; 6; 7

template dotdotImpl(t) {.dirty.} =
  iterator `..`*(a, b: t): t {.inline.} =
    ## A type specialized version of ``..`` for convenience so that
    ## mixing integer types works better.
    ##
    ## See also:
    ## * [..<](#..<.i,T,T)

dotdotImpl(int64)
dotdotImpl(int32)
dotdotImpl(uint64)
dotdotImpl(uint32)

iterator `..<`*[T](a, b: T): T {.inline.} = discard

iterator `||`*[S, T](a: S, b: T, annotation: static string = "parallel for"): T {.
  inline, magic: "OmpParFor", sideEffect.} =
  ## OpenMP parallel loop iterator. Same as `..` but the loop may run in parallel.
  ##
  ## `annotation` is an additional annotation for the code generator to use.
  ## The default annotation is `parallel for`.
  ## Please refer to the `OpenMP Syntax Reference
  ## <https://www.openmp.org/wp-content/uploads/OpenMP-4.5-1115-CPP-web.pdf>`_
  ## for further information.
  ##
  ## Note that the compiler maps that to
  ## the ``#pragma omp parallel for`` construct of `OpenMP`:idx: and as
  ## such isn't aware of the parallelism in your code! Be careful! Later
  ## versions of ``||`` will get proper support by Nim's code generator
  ## and GC.
  discard


when defined(nimNoNilSeqs2):
  when not compileOption("nilseqs"):
    {.pragma: nilError, error.}
  else:
    {.pragma: nilError.}
else:
  {.pragma: nilError.}

proc isNil*[T](x: seq[T]): bool {.noSideEffect, magic: "IsNil", nilError.}
proc isNil*[T](x: ref T): bool {.noSideEffect, magic: "IsNil".}
proc isNil*(x: string): bool {.noSideEffect, magic: "IsNil", nilError.}

proc isNil*[T](x: ptr T): bool {.noSideEffect, magic: "IsNil".}
proc isNil*(x: pointer): bool {.noSideEffect, magic: "IsNil".}
proc isNil*(x: cstring): bool {.noSideEffect, magic: "IsNil".}
proc isNil*[T: proc](x: T): bool {.noSideEffect, magic: "IsNil".}
  ## Fast check whether `x` is nil. This is sometimes more efficient than
  ## ``== nil``.

proc astToStr*[T](x: T): string {.magic: "AstToStr", noSideEffect.} =
  ## Converts the AST of `x` into a string representation. This is very useful
  ## for debugging.
  discard

proc instantiationInfo*(index = -1, fullPaths = false): tuple[
  filename: string, line: int, column: int] {.magic: "InstantiationInfo", noSideEffect.} =
  ## Provides access to the compiler's instantiation stack line information
  ## of a template.
  ##
  ## While similar to the `caller info`:idx: of other languages, it is determined
  ## at compile time.
  ##
  ## This proc is mostly useful for meta programming (eg. ``assert`` template)
  ## to retrieve information about the current filename and line number.
  ## Example:
  ##
  ## .. code-block:: nim
  ##   import strutils
  ##
  ##   template testException(exception, code: untyped): typed =
  ##     try:
  ##       let pos = instantiationInfo()
  ##       discard(code)
  ##       echo "Test failure at $1:$2 with '$3'" % [pos.filename,
  ##         $pos.line, astToStr(code)]
  ##       assert false, "A test expecting failure succeeded?"
  ##     except exception:
  ##       discard
  ##
  ##   proc tester(pos: int): int =
  ##     let
  ##       a = @[1, 2, 3]
  ##     result = a[pos]
  ##
  ##   when isMainModule:
  ##     testException(IndexError, tester(30))
  ##     testException(IndexError, tester(1))
  ##     # --> Test failure at example.nim:20 with 'tester(1)'
  discard

proc compiles*(x: untyped): bool {.magic: "Compiles", noSideEffect, compileTime.} =
  ## Special compile-time procedure that checks whether `x` can be compiled
  ## without any semantic error.
  ## This can be used to check whether a type supports some operation:
  ##
  ## .. code-block:: Nim
  ##   when compiles(3 + 4):
  ##     echo "'+' for integers is available"
  discard



proc find*[T, S](a: T, item: S): int {.inline.} =
  ## Returns the first index of `item` in `a` or -1 if not found. This requires
  ## appropriate `items` and `==` operations to work.
  discard


proc `==`*[T: tuple|object](x, y: T): bool =
  ## Generic ``==`` operator for tuples that is lifted from the components.
  ## of `x` and `y`.
  discard

proc `<=`*[T: tuple](x, y: T): bool =
  ## Generic lexicographic ``<=`` operator for tuples that is lifted from the
  ## components of `x` and `y`. This implementation uses `cmp`.
  discard

proc `<`*[T: tuple](x, y: T): bool =
  ## Generic lexicographic ``<`` operator for tuples that is lifted from the
  ## components of `x` and `y`. This implementation uses `cmp`.
  discard



# ----------------- GC interface ---------------------------------------------

when not defined(nimscript) and hasAlloc:
  type
    GC_Strategy* = enum  ## The strategy the GC should use for the application.
      gcThroughput,      ## optimize for throughput
      gcResponsiveness,  ## optimize for responsiveness (default)
      gcOptimizeTime,    ## optimize for speed
      gcOptimizeSpace    ## optimize for memory footprint

  proc GC_disable*() {.rtl, inl, benign.}
    ## Disables the GC. If called `n` times, `n` calls to `GC_enable`
    ## are needed to reactivate the GC.
    ##
    ## Note that in most circumstances one should only disable
    ## the mark and sweep phase with
    ## `GC_disableMarkAndSweep <#GC_disableMarkAndSweep>`_.
    ##
    ## **Note:** This proc is not available for JS backend and nimscript.

  proc GC_enable*() {.rtl, inl, benign.}
    ## Enables the GC again.
    ##
    ## **Note:** This proc is not available for JS backend and nimscript.

  proc GC_fullCollect*() {.rtl, benign.}
    ## Forces a full garbage collection pass.
    ## Ordinary code does not need to call this (and should not).
    ##
    ## **Note:** This proc is not available for JS backend and nimscript.

  proc GC_enableMarkAndSweep*() {.rtl, benign.}
  proc GC_disableMarkAndSweep*() {.rtl, benign.}
    ## The current implementation uses a reference counting garbage collector
    ## with a seldomly run mark and sweep phase to free cycles. The mark and
    ## sweep phase may take a long time and is not needed if the application
    ## does not create cycles. Thus the mark and sweep phase can be deactivated
    ## and activated separately from the rest of the GC.
    ##
    ## **Note:** This proc is not available for JS backend and nimscript.

  proc GC_getStatistics*(): string {.rtl, benign.}
    ## Returns an informative string about the GC's activity. This may be useful
    ## for tweaking.
    ##
    ## **Note:** This proc is not available for JS backend and nimscript.

  proc GC_ref*[T](x: ref T) {.magic: "GCref", benign.}
  proc GC_ref*[T](x: seq[T]) {.magic: "GCref", benign.}
  proc GC_ref*(x: string) {.magic: "GCref", benign.}
    ## Marks the object `x` as referenced, so that it will not be freed until
    ## it is unmarked via `GC_unref`.
    ## If called n-times for the same object `x`,
    ## n calls to `GC_unref` are needed to unmark `x`.
    ##
    ## **Note:** This proc is not available for JS backend and nimscript.

  proc GC_unref*[T](x: ref T) {.magic: "GCunref", benign.}
  proc GC_unref*[T](x: seq[T]) {.magic: "GCunref", benign.}
  proc GC_unref*(x: string) {.magic: "GCunref", benign.}
    ## See the documentation of `GC_ref <#GC_ref,string>`_.
    ##
    ## **Note:** This proc is not available for JS backend and nimscript.

  proc nimGC_setStackBottom*(theStackBottom: pointer) {.compilerRtl, noinline, benign.}
    ## Expands operating GC stack range to `theStackBottom`. Does nothing
    ## if current stack bottom is already lower than `theStackBottom`.
    ##
    ## **Note:** This proc is not available for JS backend and nimscript.


# we have to compute this here before turning it off in except.nim anyway ...
const NimStackTrace = compileOption("stacktrace")

const nimCoroutines* = false

{.push checks: off.}
var
  globalRaiseHook*: proc (e: ref Exception): bool {.nimcall, benign.}
    ## With this hook you can influence exception handling on a global level.
    ## If not nil, every 'raise' statement ends up calling this hook.
    ##
    ## **Warning**: Ordinary application code should never set this hook!
    ## You better know what you do when setting this.
    ##
    ## If ``globalRaiseHook`` returns false, the exception is caught and does
    ## not propagate further through the call stack.

  localRaiseHook* {.threadvar.}: proc (e: ref Exception): bool {.nimcall, benign.}
    ## With this hook you can influence exception handling on a
    ## thread local level.
    ## If not nil, every 'raise' statement ends up calling this hook.
    ##
    ## **Warning**: Ordinary application code should never set this hook!
    ## You better know what you do when setting this.
    ##
    ## If ``localRaiseHook`` returns false, the exception
    ## is caught and does not propagate further through the call stack.

  outOfMemHook*: proc () {.nimcall, tags: [], benign, raises: [].}
    ## Set this variable to provide a procedure that should be called
    ## in case of an `out of memory`:idx: event. The standard handler
    ## writes an error message and terminates the program.
    ##
    ## `outOfMemHook` can be used to raise an exception in case of OOM like so:
    ##
    ## .. code-block:: Nim
    ##
    ##   var gOutOfMem: ref EOutOfMemory
    ##   new(gOutOfMem) # need to be allocated *before* OOM really happened!
    ##   gOutOfMem.msg = "out of memory"
    ##
    ##   proc handleOOM() =
    ##     raise gOutOfMem
    ##
    ##   system.outOfMemHook = handleOOM
    ##
    ## If the handler does not raise an exception, ordinary control flow
    ## continues and the program is terminated.

type
  PFrame* = ptr TFrame  ## Represents a runtime frame of the call stack;
                        ## part of the debugger API.
  TFrame* {.importc, nodecl, final.} = object ## The frame itself.
    prev*: PFrame       ## Previous frame; used for chaining the call stack.
    procname*: cstring  ## Name of the proc that is currently executing.
    line*: int          ## Line number of the proc that is currently executing.
    filename*: cstring  ## Filename of the proc that is currently executing.
    len*: int16         ## Length of the inspectable slots.
    calldepth*: int16   ## Used for max call depth checking.


proc echo*(x: varargs[typed, `$`]) {.magic: "Echo", tags: [WriteIOEffect],
  benign, sideEffect.}
  ## Writes and flushes the parameters to the standard output.
  ##
  ## Special built-in that takes a variable number of arguments. Each argument
  ## is converted to a string via ``$``, so it works for user-defined
  ## types that have an overloaded ``$`` operator.
  ## It is roughly equivalent to ``writeLine(stdout, x); flushFile(stdout)``, but
  ## available for the JavaScript target too.
  ##
  ## Unlike other IO operations this is guaranteed to be thread-safe as
  ## ``echo`` is very often used for debugging convenience. If you want to use
  ## ``echo`` inside a `proc without side effects
  ## <manual.html#pragmas-nosideeffect-pragma>`_ you can use `debugEcho <#debugEcho>`_
  ## instead.

proc debugEcho*(x: varargs[typed, `$`]) {.magic: "Echo", noSideEffect,
                                          tags: [], raises: [].}
  ## Same as `echo <#echo>`_, but as a special semantic rule, ``debugEcho``
  ## pretends to be free of side effects, so that it can be used for debugging
  ## routines marked as `noSideEffect <manual.html#pragmas-nosideeffect-pragma>`_.


template newException*(exceptn: typedesc, message: string;
                       parentException: ref Exception = nil): untyped = discard
  ## Creates an exception object of type ``exceptn`` and sets its ``msg`` field
  ## to `message`. Returns the new exception object.


proc getTypeInfo*[T](x: T): pointer {.magic: "GetTypeInfo", benign.} =
  ## Get type information for `x`.
  ##
  ## Ordinary code should not use this, but the `typeinfo module
  ## <typeinfo.html>`_ instead.
  discard

proc abs*(x: int): int {.magic: "AbsI", noSideEffect.} =
  ## Returns the absolute value of `x`.
  ##
  ## If `x` is ``low(x)`` (that is -MININT for its type),
  ## an overflow exception is thrown (if overflow checking is turned on).
  discard


template likely*(val: bool): bool =
  ## Hints the optimizer that `val` is likely going to be true.
  ##
  ## You can use this template to decorate a branch condition. On certain
  ## platforms this can help the processor predict better which branch is
  ## going to be run. Example:
  ##
  ## .. code-block:: Nim
  ##   for value in inputValues:
  ##     if likely(value <= 100):
  ##       process(value)
  ##     else:
  ##       echo "Value too big!"
  ##
  ## On backends without branch prediction (JS and the nimscript VM), this
  ## template will not affect code execution.
  discard

template unlikely*(val: bool): bool =
  ## Hints the optimizer that `val` is likely going to be false.
  ##
  ## You can use this proc to decorate a branch condition. On certain
  ## platforms this can help the processor predict better which branch is
  ## going to be run. Example:
  ##
  ## .. code-block:: Nim
  ##   for value in inputValues:
  ##     if unlikely(value > 100):
  ##       echo "Value too big!"
  ##     else:
  ##       process(value)
  ##
  ## On backends without branch prediction (JS and the nimscript VM), this
  ## template will not affect code execution.
  discard


const
  NimMajor* {.intdefine.}: int = 0
    ## The major number of Nim's version.

  NimMinor* {.intdefine.}: int = 19
    ## The minor number of Nim's version.

  NimPatch* {.intdefine.}: int = 9
    ## The patch number of Nim's version.

  NimVersion*: string = $NimMajor & "." & $NimMinor & "." & $NimPatch
    ## The version of Nim as a string.


type
  FileSeekPos* = enum ## Position relative to which seek should happen.
                      # The values are ordered so that they match with stdio
                      # SEEK_SET, SEEK_CUR and SEEK_END respectively.
    fspSet            ## Seek to absolute value
    fspCur            ## Seek relative to current position
    fspEnd            ## Seek relative to end



proc allocCStringArray*(a: openArray[string]): cstringArray =
  ## Creates a NULL terminated cstringArray from `a`. The result has to
  ## be freed with `deallocCStringArray` after it's not needed anymore.
  ##
  ## **Note:** This proc is not available for JS backend.
  discard

proc deallocCStringArray*(a: cstringArray) =
  ## Frees a NULL terminated cstringArray.
  ##
  ## **Note:** This proc is not available for JS backend.
  discard

proc atomicInc*(memLoc: var int, x: int = 1): int {.inline,
  discardable, benign.} =
  ## Atomic increment of `memLoc`. Returns the value after the operation.
  ##
  ## **Note:** This proc is not available for JS backend and nimscript.
  discard

proc atomicDec*(memLoc: var int, x: int = 1): int {.inline,
  discardable, benign.} =
  ## Atomic decrement of `memLoc`. Returns the value after the operation.
  ##
  ## **Note:** This proc is not available for JS backend and nimscript.
  discard

proc setControlCHook*(hook: proc () {.noconv.}) =
  ## Allows you to override the behaviour of your application when CTRL+C
  ## is pressed. Only one such hook is supported.
  ##
  ## **Note:** This proc is not available for JS backend and nimscript.
  discard

when not defined(noSignalHandler) and not defined(useNimRtl):
  proc unsetControlCHook*() =
    ## Reverts a call to setControlCHook.
    ##
    ## **Note:** This proc is not available for JS backend and nimscript.
    discard

proc writeStackTrace*() {.tags: [], gcsafe.} =
  ## Writes the current stack trace to ``stderr``. This is only works
  ## for debug builds. Since it's usually used for debugging, this
  ## is proclaimed to have no IO effect!
  ##
  ## **Note:** This proc is not available for JS backend and nimscript.
  discard

proc getStackTrace*(): string {.gcsafe.} =
  ## Gets the current stack trace. This only works for debug builds.
  ##
  ## **Note:** This proc is not available for JS backend and nimscript.
  discard

proc getStackTrace*(e: ref Exception): string {.gcsafe.} =
  ## Gets the stack trace associated with `e`, which is the stack that
  ## lead to the ``raise`` statement. This only works for debug builds.
  ##
  ## **Note:** This proc is not available for JS backend and nimscript.
  discard


proc getCurrentException*(): ref Exception {.compilerRtl, inl, benign.} =
  ## Retrieves the current exception; if there is none, `nil` is returned.
  ##
  ## **Note:** This proc is not available for JS backend and nimscript.
  discard

proc getCurrentExceptionMsg*(): string {.inline, benign.} =
  ## Retrieves the error message that was attached to the current
  ## exception; if there is none, `""` is returned.
  ##
  ## **Note:** This proc is not available for JS backend and nimscript.
  discard

proc onRaise*(action: proc(e: ref Exception): bool{.closure.}) {.deprecated.} =
  ## **Deprecated since version 0.18.1**: No good usages of this
  ## feature are known.
  ##
  ## Can be used in a ``try`` statement to setup a Lisp-like
  ## `condition system`:idx:\: This prevents the 'raise' statement to
  ## raise an exception but instead calls ``action``.
  ## If ``action`` returns false, the exception has been handled and
  ## does not propagate further through the call stack.
  ##
  ## **Note:** This proc is not available for JS backend and nimscript.
  discard

proc setCurrentException*(exc: ref Exception) {.inline, benign.} =
  ## Sets the current exception.
  ##
  ## **Note:** This proc is not available for JS backend and nimscript.
  ##
  ## **Warning**: Only use this if you know what you are doing.
  discard

proc rawProc*[T: proc](x: T): pointer {.noSideEffect, inline.} =
  ## Retrieves the raw proc pointer of the closure `x`. This is
  ## useful for interfacing closures with C.
  ##
  ## **Note:** This proc is not available for JS backend and nimscript.
  discard

proc rawEnv*[T: proc](x: T): pointer {.noSideEffect, inline.} =
  ## Retrieves the raw environment pointer of the closure `x`. This is
  ## useful for interfacing closures with C.
  ##
  ## **Note:** This proc is not available for JS backend and nimscript.
  discard

proc finished*[T: proc](x: T): bool {.noSideEffect, inline.} =
  ## This can be used to determine if a first class iterator has finished.
  ##
  ## **Note:** This proc is not available for JS backend and nimscript.
  discard

proc quit*(errormsg: string, errorcode = QuitFailure) {.noReturn.} =
  ## A shorthand for ``echo(errormsg); quit(errorcode)``.
  discard

{.pop.} # checks
{.pop.} # hints


type
  BackwardsIndex* = distinct int ## Type that is constructed by ``^`` for
                                 ## reversed array accesses.
                                 ## (See `^ template <#^.t,int>`_)

template `^`*(x: int): BackwardsIndex = discard
  ## Builtin `roof`:idx: operator that can be used for convenient array access.
  ## ``a[^x]`` is a shortcut for ``a[a.len-x]``.
  ##
  ## .. code-block:: Nim
  ##   let
  ##     a = [1, 3, 5, 7, 9]
  ##     b = "abcdefgh"
  ##
  ##   echo a[^1] # => 9
  ##   echo b[^2] # => g

template `..^`*(a, b: untyped): untyped =
  ## A shortcut for `.. ^` to avoid the common gotcha that a space between
  ## '..' and '^' is required.
  discard

template `..<`*(a, b: untyped): untyped =
  ## A shortcut for `a .. pred(b)`.
  ##
  ## .. code-block:: Nim
  ##   for i in 5 ..< 9:
  ##     echo i # => 5; 6; 7; 8
  discard


proc slurp*(filename: string): string {.magic: "Slurp".} =
  ## This is an alias for `staticRead <#staticRead,string>`_.
  discard

proc staticRead*(filename: string): string {.magic: "Slurp".} =
  ## Compile-time `readFile <io.html#readFile,string>`_ proc for easy
  ## `resource`:idx: embedding:
  ##
  ## .. code-block:: Nim
  ##     const myResource = staticRead"mydatafile.bin"
  ##
  ## `slurp <#slurp,string>`_ is an alias for ``staticRead``.
  discard

proc gorge*(command: string, input = "", cache = ""): string {.
  magic: "StaticExec".} = discard
  ## This is an alias for `staticExec <#staticExec,string,string,string>`_.

proc staticExec*(command: string, input = "", cache = ""): string {.
  magic: "StaticExec".} = discard
  ## Executes an external process at compile-time.
  ##
  ## If `input` is not an empty string, it will be passed as a standard input
  ## to the executed program.
  ##
  ## .. code-block:: Nim
  ##     const buildInfo = "Revision " & staticExec("git rev-parse HEAD") &
  ##                       "\nCompiled on " & staticExec("uname -v")
  ##
  ## `gorge <#gorge,string,string,string>`_ is an alias for ``staticExec``.
  ##
  ## Note that you can use this proc inside a pragma like
  ## `passC <nimc.html#passc-pragma>`_ or `passL <nimc.html#passl-pragma>`_.
  ##
  ## If ``cache`` is not empty, the results of ``staticExec`` are cached within
  ## the ``nimcache`` directory. Use ``--forceBuild`` to get rid of this caching
  ## behaviour then. ``command & input & cache`` (the concatenated string) is
  ## used to determine whether the entry in the cache is still valid. You can
  ## use versioning information for ``cache``:
  ##
  ## .. code-block:: Nim
  ##     const stateMachine = staticExec("dfaoptimizer", "input", "0.8.0")

proc gorgeEx*(command: string, input = "", cache = ""): tuple[output: string,
                                                              exitCode: int] =
  ## Similar to `gorge <#gorge,string,string,string>`_ but also returns the
  ## precious exit code.
  discard


template `&=`*(x, y: typed) =
  ## Generic 'sink' operator for Nim.
  ##
  ## For files an alias for ``write``.
  ## If not specialized further, an alias for ``add``.
  discard
when declared(File):
  template `&=`*(f: File, x: typed) = write(f, x)

template currentSourcePath*: string = discard
  ## Returns the full file-system path of the current source

template rangeCheck*(cond) =
  ## Helper for performing user-defined range checks.
  ## Such checks will be performed only when the ``rangechecks``
  ## compile-time option is enabled.
  discard


type
  NimNodeObj = object
  NimNode* {.magic: "PNimrodNode".} = ref NimNodeObj
    ## Represents a Nim AST node. Macros operate on this type.

template eval*(blk: typed): typed =
  ## Executes a block of code at compile time just as if it was a macro.
  ##
  ## Optionally, the block can return an AST tree that will replace the
  ## eval expression.
  discard


proc locals*(): RootObj {.magic: "Plugin", noSideEffect.} =
  ## Generates a tuple constructor expression listing all the local variables
  ## in the current scope.
  ##
  ## This is quite fast as it does not rely
  ## on any debug or runtime information. Note that in contrast to what
  ## the official signature says, the return type is *not* ``RootObj`` but a
  ## tuple of a structure that depends on the current scope. Example:
  ##
  ## .. code-block:: Nim
  ##   proc testLocals() =
  ##     var
  ##       a = "something"
  ##       b = 4
  ##       c = locals()
  ##       d = "super!"
  ##
  ##     b = 1
  ##     for name, value in fieldPairs(c):
  ##       echo "name ", name, " with value ", value
  ##     echo "B is ", b
  ##   # -> name a with value something
  ##   # -> name b with value 4
  ##   # -> B is 1
  discard

proc deepCopy*[T](x: var T, y: T) {.noSideEffect, magic: "DeepCopy".} =
  ## Performs a deep copy of `y` and copies it into `x`.
  ##
  ## This is also used by the code generator
  ## for the implementation of ``spawn``.
  ##
  ## **Note:** This proc is not available for JS backend and nimscript.
  discard

proc deepCopy*[T](y: T): T =
  ## Convenience wrapper around `deepCopy` overload.
  ##
  ## **Note:** This proc is not available for JS backend and nimscript.
  discard

proc procCall*(x: untyped) {.magic: "ProcCall", compileTime.} =
  ## Special magic to prohibit dynamic binding for `method`:idx: calls.
  ## This is similar to `super`:idx: in ordinary OO languages.
  ##
  ## .. code-block:: Nim
  ##   # 'someMethod' will be resolved fully statically:
  ##   procCall someMethod(a, b)
  discard


template closureScope*(body: untyped): untyped =
  ## Useful when creating a closure in a loop to capture local loop variables by
  ## their current iteration values. Example:
  ##
  ## .. code-block:: Nim
  ##   var myClosure : proc()
  ##   # without closureScope:
  ##   for i in 0 .. 5:
  ##     let j = i
  ##     if j == 3:
  ##       myClosure = proc() = echo j
  ##   myClosure() # outputs 5. `j` is changed after closure creation
  ##   # with closureScope:
  ##   for i in 0 .. 5:
  ##     closureScope: # Everything in this scope is locked after closure creation
  ##       let j = i
  ##       if j == 3:
  ##         myClosure = proc() = echo j
  ##   myClosure() # outputs 3
  discard

template once*(body: untyped): untyped =
  ## Executes a block of code only once (the first time the block is reached).
  ##
  ## .. code-block:: Nim
  ##
  ##  proc draw(t: Triangle) =
  ##    once:
  ##      graphicsInit()
  ##    line(t.p1, t.p2)
  ##    line(t.p2, t.p3)
  ##    line(t.p3, t.p1)
  ##
  discard

type
  ForLoopStmt* {.compilerProc.} = object ## \
    ## A special type that marks a macro as a `for-loop macro`:idx:.
    ## See `"For loop macros" <manual.html#macros-for-loop-macros>`_.

when defined(genode):
  var componentConstructHook*: proc (env: GenodeEnv) {.nimcall.}
      ## Hook into the Genode component bootstrap process.
      ##
      ## This hook is called after all globals are initialized.
      ## When this hook is set the component will not automatically exit,
      ## call ``quit`` explicitly to do so. This is the only available method
      ## of accessing the initial Genode environment.

  proc nim_component_construct(env: GenodeEnv) {.exportc.} =
    ## Procedure called during ``Component::construct`` by the loader.
    if componentConstructHook.isNil:
      env.quit(programResult)
        # No native Genode application initialization,
        # exit as would POSIX.
    else:
      componentConstructHook(env)
        # Perform application initialization
        # and return to thread entrypoint.


when defined(nimHasDefault):
  proc default*(T: typedesc): T {.magic: "Default", noSideEffect.}
    ## Returns the default value of the type ``T``.



# -----------------------------------------------------------------------------
# deprecated
# -----------------------------------------------------------------------------

template accumulateResult*(iter: untyped) {.deprecated:
  "use `sequtils.toSeq` instead (more hygienic, sometimes more efficient)".} =
  ## **Deprecated since v0.19.2:** use `sequtils.toSeq
  ## <sequtils.html#toSeq.t,untyped>`_ instead.
  ##
  ## Helps to convert an iterator to a proc.
  ## `sequtils.toSeq <sequtils.html#toSeq.t,untyped>`_ is more hygienic and efficient.
  discard

proc internalNew*[T](a: var ref T) {.magic: "New", noSideEffect.}
  ## Leaked implementation detail. Do not use.


proc `<`*[T](x: Ordinal[T]): T {.magic: "UnaryLt", noSideEffect, deprecated.} =
  ## **Deprecated since version 0.18.0**. For the common excluding range
  ## write ``0 ..< 10`` instead of ``0 .. < 10`` (look at the spacing).
  ## For ``<x`` write ``pred(x)``.
  ##
  ## Unary ``<`` that can be used for excluding ranges.
  ## Semantically this is the same as `pred <#pred,T,int>`_.
  ##
  ## .. code-block:: Nim
  ##   for i in 0 .. <10: echo i # => 0 1 2 3 4 5 6 7 8 9
  ##
  discard


proc `+=`*[T: enum|bool](x: var T, y: T) {.
  magic: "Inc", noSideEffect, deprecated: "use `inc` instead".}
  ## **Deprecated since v0.20**: use `inc` instead.

proc `-=`*[T: enum|bool](x: var T, y: T) {.
  magic: "Dec", noSideEffect, deprecated: "0.20.0, use `dec` instead".}
  ## **Deprecated since v0.20**: use `dec` instead.


when hasAlloc:
  # XXX: make these the default (or implement the NilObject optimization)
  proc safeAdd*[T](x: var seq[T], y: T) {.noSideEffect, deprecated.} =
    ## **Deprecated**
    ##
    ## Adds ``y`` to ``x`` unless ``x`` is not yet initialized; in that case,
    ## ``x`` becomes ``@[y]``.
    when defined(nimNoNilSeqs):
      x.add(y)
    else:
      if x == nil: x = @[y]
      else: x.add(y)

  proc safeAdd*(x: var string, y: char) {.noSideEffect, deprecated.} =
    ## **Deprecated**
    ##
    ## Adds ``y`` to ``x``. If ``x`` is ``nil`` it is initialized to ``""``.
    when defined(nimNoNilSeqs):
      x.add(y)
    else:
      if x == nil: x = ""
      x.add(y)

  proc safeAdd*(x: var string, y: string) {.noSideEffect, deprecated.} =
    ## **Deprecated**
    ##
    ## Adds ``y`` to ``x`` unless ``x`` is not yet initalized; in that
    ## case, ``x`` becomes ``y``.
    when defined(nimNoNilSeqs):
      x.add(y)
    else:
      if x == nil: x = y
      else: x.add(y)


proc getRefcount*[T](x: ref T): int {.importc: "getRefcount", noSideEffect,
  deprecated: "the refcount never was reliable, the GC does not use traditional refcounting".}
  ## Deprecated.
proc getRefcount*(x: string): int {.importc: "getRefcount", noSideEffect,
  deprecated: "the refcount never was reliable, the GC does not use traditional refcounting".}
  ## Deprecated.
proc getRefcount*[T](x: seq[T]): int {.importc: "getRefcount", noSideEffect,
  deprecated: "the refcount never was reliable, the GC does not use traditional refcounting".}
  ## Deprecated.
  ##
  ## Retrieves the reference count of an heap-allocated object. The
  ## value is implementation-dependent.

when not defined(JS):
  proc ze*(x: int8): int {.magic: "Ze8ToI", noSideEffect, deprecated.}
    ## zero extends a smaller integer type to ``int``. This treats `x` as
    ## unsigned.
    ##
    ## **Deprecated since version 0.19.9**: Use unsigned integers instead.

  proc ze*(x: int16): int {.magic: "Ze16ToI", noSideEffect, deprecated.}
    ## zero extends a smaller integer type to ``int``. This treats `x` as
    ## unsigned.
    ##
    ## **Deprecated since version 0.19.9**: Use unsigned integers instead.

  proc ze64*(x: int8): int64 {.magic: "Ze8ToI64", noSideEffect, deprecated.}
    ## zero extends a smaller integer type to ``int64``. This treats `x` as
    ## unsigned.
    ##
    ## **Deprecated since version 0.19.9**: Use unsigned integers instead.

  proc ze64*(x: int16): int64 {.magic: "Ze16ToI64", noSideEffect, deprecated.}
    ## zero extends a smaller integer type to ``int64``. This treats `x` as
    ## unsigned.
    ##
    ## **Deprecated since version 0.19.9**: Use unsigned integers instead.

  proc ze64*(x: int32): int64 {.magic: "Ze32ToI64", noSideEffect, deprecated.}
    ## zero extends a smaller integer type to ``int64``. This treats `x` as
    ## unsigned.
    ##
    ## **Deprecated since version 0.19.9**: Use unsigned integers instead.

  proc ze64*(x: int): int64 {.magic: "ZeIToI64", noSideEffect, deprecated.}
    ## zero extends a smaller integer type to ``int64``. This treats `x` as
    ## unsigned. Does nothing if the size of an ``int`` is the same as ``int64``.
    ## (This is the case on 64 bit processors.)
    ##
    ## **Deprecated since version 0.19.9**: Use unsigned integers instead.

  proc toU8*(x: int): int8 {.magic: "ToU8", noSideEffect, deprecated.}
    ## treats `x` as unsigned and converts it to a byte by taking the last 8 bits
    ## from `x`.
    ##
    ## **Deprecated since version 0.19.9**: Use unsigned integers instead.

  proc toU16*(x: int): int16 {.magic: "ToU16", noSideEffect, deprecated.}
    ## treats `x` as unsigned and converts it to an ``int16`` by taking the last
    ## 16 bits from `x`.
    ##
    ## **Deprecated since version 0.19.9**: Use unsigned integers instead.

  proc toU32*(x: int64): int32 {.magic: "ToU32", noSideEffect, deprecated.}
    ## treats `x` as unsigned and converts it to an ``int32`` by taking the
    ## last 32 bits from `x`.
    ##
    ## **Deprecated since version 0.19.9**: Use unsigned integers instead.
