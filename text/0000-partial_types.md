- Feature Name: `partial_types`
- Start Date: 2023-05-15
- RFC PR: [rust-lang/rfcs#0000](https://github.com/rust-lang/rfcs/pull/0000)
- Rust Issue: [rust-lang/rust#0000](https://github.com/rust-lang/rust/issues/0000)


# Summary
[summary]: #summary

This proposal is universal flexible tool to work **safe** with partial Enums, Structs and Tuples in parameters, arguments, references and  borrows.

Advantages: maximum type safety, maximum type control guarantee, no ambiguities, zero-cost-binary, flexibility, usability and universality.


# Motivation
[motivation]: #motivation

Partial Types proposal is a generalization on "partial borrowing"-like proposals. Safe, Flexible controllable partial parameters for functions and partial consumption (including partial borrowing) are highly needed.

Partial Types extension gives to Sum Types (`ST = T1 or T2 or T3 or ..`), Enums first of all, a good tool for "partial functions".
```rust
enum EnumABC { A(u32), B(i64), C(f32), }

// function with partial parameter Enum
fn print_A(a: EnumABC.{A}) {
    println!("a is {}", a.0);
}

let ea = EnumABC::A(7);
//  ea : EnumABC.{A} inferred

print_A(ea);
```

Partial Types extension gives to Product Types (`PT = T1 and T2 and T3 and ..`), Structs and Tuples first of all, a good **mathematical guarantee** to borrow-checker that borrowing the whole variable with partial type and pretending to borrow just permitted fields is **fully safe**.
```rust
struct StructABC { a: u32, b: i64, c: f32, }

// function with partial parameter Struct
fn ref_a (s : & StructABC.{a}) -> &u32 {
	&s.a
}

let s = StructABC {a: 4, b: 7, c: 0.0};

// partial expression at argument
let sa = ref_a(& s.{a});
```

And since it is a guarantee by **type**, not by **values**, it has _zero cost_ in binary! Any type error is a compiler error, so no errors in the runtime.

This extension is not only fully backward-compatible, but is fully forward-compatible! Forward-compatibility is an ability to use updated functions old way.

# Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

Partial (Sub-)Types are types, whiсh has not full range of possible values and they have limited access to own fields.

Partial Types are not Contract Types (Types with invariant), but this proposal could coexist with Contract Types.

Partiality of type (or partial type access) is written as `Path.{fld1, fld2, ..}` after Path (Type name), where `fld1`, `fld2`, .. are only permitted fields of this type, the rest of fields are denied access fields.

It is forbidden to use somehow denied fields (like have outside access to private field), including access to read denied field, to write, to borrow, to move. It is a compile error if someone try to access.

## Partial Enums

**_(A)_** *independent sub-proposal*, if it implemented after (B), no extra syntax is needed.

For Sum Types (`ST = T1 or T2 or T3 or ..`), for Enums adding partiality to type is **enough** to have **full flexibility** of using those sub-types.

```rust
enum MyEnum {
  A(u32),
  B { x: u32 },
  C(u32),
  D(u32),
}

fn print_A(a: MyEnum.{A}) {
    println!("a is {}", a.0);
}

fn print_B(b: MyEnum.{B}) {
    println!("b is {}", b.x);
}

fn print_no_pattern(e: MyEnum) {
  match e {
    MyEnum::A(_)  => print_A(e),     // e : MyEnum.{A}
    b @ MyEnum::B(..) => print_B(b), // b : MyEnum.{B}
    _ => (),                         // e : MyEnum.{C, D};
  }
}
```

Type checker must guess (without calculating) from assigning **values**, binding **values** and matching **values** which is subtype of Enum type. If the type-checker is more clever, then more conclusions of Enum subtype it has.

Sum-Typed argument type must match with parameter type for function or argument type could has **less** permitted partiality then parameter type.
```rust
// Enum ~ Sum Type
enum E4 {A (i32), B(i32), C (i32), D(i32)}

fn do_eab(e : E4.{A, B}) { /* .. */ }

let e : E4; 
do_eab(e); // e.{*} - error
let e : E4.{A, B, C};
do_eab(e); // e.{A, B, C} - error
let e : E4.{A, B};
do_eab(e); // e.{A, B} - Ok
let e : E4.{A} = E4::A(5);
do_eab(e); // e.{A} - Ok
let e : E4.{B} = E4::B(7);
do_eab(e); // e.{B} - Ok
```

Sum-Typed argument type must select for Implementations same type or type, which has more permitted partiality.

Implementation of sub-type must have same result as Implementation for full type.

So for ergonomic it is Ok to have for each "one-field sub-Enum" Implementation and one Implementation for "full Enum", which reuse one-field sub-Enum Implementation.

## Partial Structs and Tuples

**_(B)_** *independent sub-proposal*

For Product Types `PT = T1 and T2 and T3 and ..`), for structs, tuples we need not only partiality of a type, but "partial access" expression: `Expr .{fld1, fld2, ..}`, where `fld1`, `fld2`, .. are permitted fields of this type, the rest of fields are denied.

One step to partial borrows Structs and Tuples.
```rust
struct Point {
  x: f64,
  y: f64, 
  was_x: f64, 
  was_y: f64,
  state : f64,
}
let mut p1 = Point {x:1.0, y:2.0, was_x: 4.0, was_y: 5.0, state: 12.0};
	// p1 : Point
	
let ref_p1was = &mut p1.{wax_x, was_y};
	// ref_p1was : &mut Point.{was_x, was_y}
	
let ref_p1now = &mut p1.{x, y};
	// ref_p1now : &mut Point.{x, y}
```
It is simple and will be possible. 

If explicit partiality is omitted, then implicit partiality is used. 

For Types, implicit partiality is `.{*}` (all fields are permitted).

For Expressions, implicit partiality is `.{_}` ("don't care" partiality) for most cases. For references and arguments, type-checker infer partiality.

Same easy to write functions, which consume partial parameters:
```rust
impl Point {
	fn ref_x (self : & Self.{x}) -> &f64 {
		&self.x
	}

	fn refmut_y (self : &mut Self.{y}) -> &mut f64 {
		&mut self.y
	}
}
let ref_p1x = p1.ref_x();
let refmut_p1y = p1.refmut_y();
```
It is expected, that `self` is **always** cut partiality of argument by same partiality as self-parameter by partial expression before use (even if implicit rules are off)!

Pseudo-rust:
```rust
	fn ref_xy (self : & Self.{'a @( x, y)}) -> &f64 {
		/*  */
	}
	
p1.ref_xy();
// "desugar"
Point::ref_xy(p1.{'a});
```

Product-Typed argument type must match with parameter type for function or argument type could has **more** permitted partiality then parameter type.
```rust
// Struct ~ Product Type
struct S4 {a : i32, b : i32, c : i32, d : i32}

fn do_sab(s : S4.{a, b}) { /* .. */ }

let s = S4 {a: 6, b: 7, c: 8, d: 9};

do_sab(s.{*});       // s.{*} - Ok
do_sab(s.{a, b, c}); // s.{a, b, c} - Ok
do_sab(s);           // s.{a, b}, inferred - Ok
do_sab(s.{a, b});    // s.{a, b} - Ok
do_sab(s.{a});       // s.{a} - error
do_sab(s.{b});       // s.{b} - error
```
Implementation of sub-Product-type is no needed.

## Several Selfs

**_(C)_** maybe insecure sub-proposal, which could be added together or after (B), especially before (D) or alternative to (D)

Before (or instead of) adding (D) Partial Mutability extension it would be nice, if a general parameter `Smv` as "same variable" is added in Implementations.

*An alternative keywords `self1`, `self2` are added.*

The idea is that general parameter `Smv` add same variable as 2nd parameter:
```rust
impl SomeStruct<Smv = Self>{
	pub fn foo(self : &mut Self.{/*'a*/}, smv : & Smv.{/*'b*/})
}

var.foo();
// desugars
SomeStruct::foo(&mut var.{/*'a*/}, & var.{/*'b*/});
```
It is expected, that `Smv` parameter is **always** cut partiality of argument by same partiality as self-parameter by partial expression before use (even if implicit rules are off)!


This makes partial borrowing fully flexible!
```rust
impl Point<Smv = Self>{
   pub fn mx_rstate(self : &mut Self.{x}, smv : & Smv.{state}) 
   { *self.x += *smv.state; }
		
   pub fn my_rstate(self : &mut Self.{y}, smv : & Smv.{state}) 
   { *self.y += *smv.state; }
   
   pub fn mxy_rstate(self : &mut.{x,y} Self.{x, y, state}) { 
     /* ... */
	 Self::mx_rstate(self.{x}, smv); // explicit
     Self::mx_rstate(self, smv);     // same implicit
     /* ... */
     Self::mx_rstate(self.{y}, smv); // explicit
	 Self::mx_rstate(self, smv);     // same implicit
    /* ... */
   }
}
```

This sub-proposal, has unresolved question is it secure not to check the origin if variable is the same if we explicitly write associated function
```rust
impl Bar<Smv = Self>{
  fn foo(self : &mut Self::{x}, smv: & Smv::{y}) { /* */ }
}
Bar::foo(&mut bar.{x}, & bar.{y}); // Ok
Bar::foo(&mut bar.{x}, & baz.{y}); // Error? Ok?
```
I think it is insecure, error, but who knows.
If it is secure, then this sub-proposal.

## Partial Mutability

**_(D)_** *partly independent sub-proposal*. If it is implemented before (B), then partly-mutable references are off.

For full flexibility of using partial borrowing partial mutability is needed!

For Product Partial Types (structs, tuples) we use "partial mutability" expression: `mut .{fld1, fld2, ..}`, where `fld1`, `fld2`, .. are mutable fields of this type, the rest of fields are immutable(constant).
 
Partly mutable variables become possible for Product Partial Types:
```rust
struct S4 {a : i32, b : i32, c : i32, d : i32}

let mut.{a}       s_ma   = S4 {a: 6, b: 7, c: 8, d: 9};
let mut.{b, c}    s_mbc  = S4 {a: 6, b: 7, c: 8, d: 9};
let mut.{a, c, d} s_macd = S4 {a: 6, b: 7, c: 8, d: 9};
```

It is also possible to make partial-mutable references, if it is implemented after (B):
```rust
   fn mab_s(&mut.{a,b} s : &mut.{a,b} S4) 
   { /* ... */ }
   
   mab_s(&mut.{a,b} s_macd);
```
It is expected, that `&mut.{..}` is a third type of borrowing!

If this extension is added, no extension (C) Several Selfs is needed (but it is no contradiction to use both extensions):
```rust
impl Point {
   pub fn mx_rstate(self : &mut.{x} Self.{x, state}) { /* ... */ }
		
   pub fn my_rstate(self : &mut.{y} Self.{y, state}) { /* ... */ }
	
   pub fn mxy_rstate(self : &mut.{x,y} Self.{x, y, state}) { 
     /* ... */
	 self.{x, state}.mx_rstate(); // explicit
     self.mx_rstate(); // same implicit
     /* ... */
     self.{y, state}.my_rstate(); // explicit
	 self.my_rstate(); // same implicit
    /* ... */
   }
}
```

## Pretty Partial Tuples

**_(E)_** sub-proposal, which could be added together or after (B), it is better before (F)

This extension is not a mandatory. Tuple type has "naked" structure, so it would be handy have more pretty visuals, instead of mark all permitted fields in "partiality", write `deny` before denied field.
```rust
let t :: (i32, &u64, f64, u8).{1,3};
// same as
let t :: (deny i32, &u64, deny f64, u8);
```

This extension is not just pretty, but useful with extension (F) partial initializing Tuples.

## Partial Initializing Structs and Tuples

**_(F)_** sub-proposal, which could be added together or after (B), it is better after (E)

All syntax and semantic is ready to have partial initializing Structs. If not all fields are initialized, then variable has partial type.
```rust
struct S4 {a : i32, b : i32, c : i32, d : i32}

let s_abcd : S4 = S4 {a: 6, b: 7, c: 8, d: 9};

let s_abc : S4.{a, b, c} = S4 {a: 6, b: 7, c: 8};
let s_bcd : S4.{b, c, d} = S4 {b: 7, c: 8, d: 9};
let s_ab  : S4.{a, b}    = S4 {a: 6, b: 7};
let s_bd  : S4.{b, d}    = S4 {b: 7, d: 9};
let s_cd  : S4.{c, d}    = S4 {c: 8, d: 9};
let s_a   : S4.{a}       = S4 {a: 6};

let s_abd = S4 {a: 6, b: 7, d: 9}; // s_abd : S4.{a, b, d}
let s_acd = S4 {a: 6, c: 8, d: 9}; // s_acd : S4.{a, c, d}
let s_ac  = S4 {a: 6, c: 8};       // s_ac  : S4.{a, c}
let s_ad  = S4 {a: 6, d: 9};       // s_ad  : S4.{a, d}
let s_bc  = S4 {b: 7, c: 8};       // s_bc  : S4.{b, c}
let s_b   = S4 {b: 7};             // s_b   : S4.{b}
let s_c   = S4 {c: 8};             // s_c   : S4.{c}
let s_d   = S4 {d: 9};             // s_d   : S4.{d}
```
Sure, it is forbidden to fill private fields outside of module.

It is also become possible to use **several** partly typed variables(sure, their permitted fields must not overlap) for using as Stuct Base fill missed fields:
```rust
struct S4 {a : i32, b : i32, c : i32, d : i32}

let s_ab : S4.{a, b} = S4 {a: 6, b: 7};
let s_c = S4 {c: 8}; // s_c : S4.{c}

let s1 : S4 = S4{b: 11, d: 17, ..s_ab, ..s_c};

let s_abc = S4{b: 11, ..s_ab, ..s_c}; // s_abc : S4.{a, b, c}

let s2 : S4 = S4{d: 17, ..s_ab, ..s_c};
```

If extension (E) pretty partiality for tuples is added, then partial initializing Tuples is also possible with `deny` pre-field and maybe **miss** Expr if type is clear.

Or partial experssion is used:
```rust
let t_0123: (i32, u16, f64, f32) = (6i32, 7u16, 8.0f64, 9.0f32);

let t_013 : (i32, u16, f64, f32).{0, 1, 3} = (6i32, 7u16, deny 8.0f64, 9.0f32);
let t_023 : (i32, u16, f64, f32).{0, 2, 3} = (6i32, deny, 8.0f64,      9.0f32);
let t_0   : (i32, u16, f64, f32).{0}       = (6i32, deny, deny,        deny);

let t_012 : (i32,      u16,      f64,      deny f32) = (6i32,      7u16,      8.0f64, deny 9.0f32);
let t_2   : (deny i32, deny u16, f64,      deny f32) = (deny 6i32, deny 7u16, 8.0f64, deny 9.0f32);
let t_23  : (deny i32, deny u16, f64,      f32)      = (deny,      deny 7u16, 8.0f64, 9.0f32);
let t_123 : (deny i32, u16,      f64,      f32)      = (deny,      7u16,      8.0f64, 9.0f32);
let t_13  : (deny i32, u16,      deny f64, f32)      = (deny,      7u16,      deny,   9.0f32);
let t_3   : (deny i32, deny u16, deny f64, f32)      = (deny,      deny,      deny,   9.0f32);

let t_01  : (i32, u16, f64, f32).{0, 1} = (6i32, 7u16, 8.0f64, 9.0f32).{0, 1};

let t_02 = (6i32,      deny 7u16, 8.0f64,      deny 9.0f32); // t_02 : (i32, u16, f64, f32).{0, 2}
let t_03 = (6i32,      deny 7u16, deny 8.0f64, 9.0f32);      // t_03 : (i32, u16, f64, f32).{0, 3}
let t_1  = (deny 6i32, 7u16,      deny 8.0f64, deny9.0f32);  // t_1  : (i32, u16, f64, f32).{1}

let t_12 = (6i32, 7u16, 8.0f64, 9.0f32).{1, 2}; // t_12 : (i32, u16, f64, f32).{1, 2}
```

## Extended Partly initializing Structs and Tuples

**_(G)_** sub-proposal, which could be added together or after (B)

*Theory of types* do not forbid to become permitted denied fields of Partial Type, but internal Rust representation of variables gives significant limitations on such action.

"Fresh" only variables could be extended, where denied fields are write only fields.

Not Fresh are referenced variables, dereferenced variables and maybe(unclear) moved variables.

Maybe assign operator `=` is enough (or new operator `let=` is needed) for that:
```rust
struct SR <T>{
    val : T,
    lnk : & T, // reference to val
}

let x = SR {val : 5i32 };
    // x : SR<i32>.{val}

x.lnk = & x.val;
    // x : SR<i32>;
```

This extension is useful for late initialization, including Self-Referential Types.

## Inferred Structs and Enums

**_(H)_** sub-proposal, which could be added together or after (B) and/or (A)

Inferred Structs and Enums is not a mandatory extension, and it is alternative to "anonymous" Srtucts and Enums.
```rust
struct StructInfr {..}

let s_c = StructInfr {c: 8i32}; // s_c : StructInfr/*{c: i32, .. }*/.{c}

let s_d = StructInfr {d: 8u8}; // s_d : StructInfr/*{c: i32, d: u8, .. }*/.{d}

enum EnumInfr {..}

let e_c = EnumInfr::C(8i32); // e_c : EnumInfr/*{C(i32), ..}*/.{C}

let e_d = EnumInfr::D{d: 77u64}; // es_d : EnumInfr/*{C(i32), D{d: u64}, ..}*/.{D}
```
It is expected, that type-checker could infers type from using its fields.


# Reference-level explanation

The core Idea of this proposal is "Proxy Borrowing" - we borrow the whole variable, but borrow-checker pretends it borrow just permitted fields.

And Type-checker gives a mathematical guarantee, than all denied fields remain intact and all partly-immutable fields remain immutable! 

And this mean, that Proxy Borrowing borrowing is fully **safe** and _zero cost_ in binary.

## Proxy Borrowing

**_(B)_** and **_(B + D)_**

Borrowing rules for partial types:

`PermittedField` field borrowing rules are ordinary Rust rules. New variable borrows the whole variable (with partial type), but checker pretends it borrows just permitted fields of this variable.

Not `PermittedField` filed is always is ready to mutable and immutable borrow regardless if origin field is denied(by move, by reference, by borrow), is visible, is mutable.

When we write a code for partial borrow (or partly mutable borrow), the link of object itself returns, but borrow-checker borrows proxy of permitted fields only.

When we write a code for full (or partial) borrow of a partial borrowed reference, the link of object itself returns again, but borrow-checker, borrows proxy of proxy of permitted fields only.

This new mechanism of Proxy Borrowing is simple and universal.

```rust
struct S4 {a : i32, b : i32, c : i32, d : i32}
let s = S4 {a : 5, b: 6, c: 7, d: 8};
    // s : S4

let r_sd = & s.{d};
    // r_sd : & S4.{d}
    //
    // r_sd  ~ Link(s);
    // borrow-checker borrows ProxyLink(s.d)

let mut mr_sabc = &mut s.{a, b, c};
    // mr_sabc : &mut S4.{a, b, c}
    //
    // mr_sabc  ~ mut Link(s);
    // borrow-checker: mut ProxyLink(s.a), mut ProxyLink(s.b), mut ProxyLink(s.c)

let rr_sbc = & mr_sabc.{b, c};
    // rr_sbc : && S4.{b, c}
    //
    // rr_sbc  ~ Link(mr_sabc);
    // borrow-checker: ProxyLink(ProxyLink(s.b)), ProxyLink(ProxyLink(s.c))

let mut mrr_sa = &mut mr_sabc.{a};
    // mrr_sa : &&mut S4.{a}
    //
    // mrr_sa  ~ mut Link(mr_sabc);
    // borrow-checker: mut ProxyLink(ProxyLink(s.a))
```

## Syntax

Second, but still important - syntax.

### Partiality Syntax

**_(Stage 1)_**

Minimal Partiality we could write such:
```
Partiality: .{ PartialFields* }
PartialFields: PermittedField (, PermittedField )* ,?
PermittedField: IDENTIFIER | TUPLE_INDEX | * | _ 
```

**_(Stage 2B)_**

If we wish to have "recursive" sub-partial Product-types for (B) and / or (D)
```
PartialFields:    PartialField (, PartialField )* ,?
PartialField:     PermittedField Partiality?
PermittedField:   IDENTIFIER | TUPLE_INDEX | * | _
```

**_(Stage 2A)_**

If we wish to have "recursive" sub-partial Enum-types for (A)
```
PartialFields:     PartialField (, PartialField )* ,?
PartialField:      PermittedField PartialSubFields?
PartialSubFields:  { PartialSubField (, PartialSubField )* ,? }
PartialSubField:   SpecificSubField Partiality
PermittedField:    IDENTIFIER | TUPLE_INDEX | * | _
SpecificSubField:  IDENTIFIER | TUPLE_INDEX
```

**_(Stage 1+2A+2B)_**

Finally, Partiality with full and maximum control and flexibility:
```
Partiality:            .{ PartialFields* }
PartialFields:         PartialField (, PartialField )* ,?
PartialField:          PermittedField PartialSubEnumFields? Partiality?
PartialSubEnumFields:  { PartialSubEnumField (, PartialSubEnumField )* ,? }
PartialSubEnumField:   SubEnumField Partiality
PermittedField:        IDENTIFIER | TUPLE_INDEX | * | ..
SubEnumField:          IDENTIFIER | TUPLE_INDEX
```

### Partial Enums syntax

**_(A)_**

The only one syntax needed to Enum - is update `TypePath` into
```
TypePath:   ::? TypePathSegment (:: TypePathSegment)* Partiality?
```

### Partial Struct and Tuple syntax

**_(B)_**

Syntax is needed to Struct Type - is update `TypePath` (same as to Enum)
```
TypePath:   ::? TypePathSegment (:: TypePathSegment)* Partiality?
```

For Tuple Type we need to update `TupleType`
```
TupleType:  ( ) | ( ( Type , )+ Type? ) Partiality?
```

For Expression we need create new kind of Expression:
```
PartialExpression:   Expression Partiality
```

and include it into `ExpressionWithoutBdeny`:
```
ExpressionWithoutBdeny:   ... | FieldExpression | PartialExpression | ...
```

### Several Selfs syntax

**_(C)_**

No special syntax For this extension is needed.

### Partial Mutability syntax

**_(D)_**

We add Mutability
```
PartialMutability: mut Partiality?
```

And change `mut` into `PartialMutability` at `IdentifierPattern`:
```
IdentifierPattern : ref? PartialMutability? IDENTIFIER (@ PatternNoTopAlt ) ?
```

If it implemented together or after (B), we change rest of `mut` into `PartialMutability`:
```
BorrowExpression : (&|&&) Expression | (&|&&) PartialMutability? Expression

ReferenceType: & Lifetime? PartialMutability? TypeNoBounds

Function:ShorthandSelf : (& | & Lifetime)? PartialMutability? self
Function:TypedSelf : PartialMutability? self : Type
```

### Pretty Partial Tuple syntax

**_(E)_**

For Tuple Type we need to update `TupleType` again:
```
TupleType:        ( ) | ( ( TupleTypeSingle , )+ TupleTypeSingle? ) Partiality?
TupleTypeSingle:  deny? Type
```

### Partial Initializing Structs and Tuples Syntax

**_(F)_**

If we allow several fillers "rest of fields", then 
```
StructExprStruct:  PathInExpression { (StructExprFields | StructBases)? }
StructExprFields:  StructExprField (, StructExprField)* (, StructBases | ,?)
StructBases:       StructBase (, StructBase )* ,?
StructBase:        .. Expression
```

No special syntax for partial initialization of Struct is don't needed.

If (E) extension is on, then we also need to change `TupleExpr` and `StructExprTuple`:
```
TupleExpression:  ( TupleElements? )
TupleElements :   ( TupleExprSingle , )+ TupleExprSingle?
TupleExprSingle:  deny | deny? Expression

StructExprTuple:  PathInExpression ( ( TupleExprSingle (, TupleExprSingle)* ,? )? )
```

### Extended Partly initializing Variables Syntax

**_(G)_**

Special syntax is not required.

### Inferred Structs and Enums Syntax

**_(H)_**

This extension is need to support `..` (or `_`) "rest of fields" field name to infer Enum / Struct fields.
```
StructExprStruct:  PathInExpression { ( .. | StructExprFields | StructBase)? }
```


# Drawbacks
[drawbacks]: #drawbacks

- it is definitely not a minor change
- type system became much more complicated


# Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

(A) Proposals that are alternatives to Partial Sum Types in a whole:
 - Enum variant types [#2593](https://github.com/rust-lang/rfcs/pull/2593)
 - Enum Variant Types [lang_t#122](https://github.com/rust-lang/lang-team/issues/122)

(B) A lot of proposals that are alternatives to Partial Product Types in a whole:
 - Partial Types (v2) [#3426](https://github.com/rust-lang/rfcs/pull/3426)
 - Partial Mutability [#3428](https://github.com/rust-lang/rfcs/pull/3428)
 - Partial Types [#3420](https://github.com/rust-lang/rfcs/pull/3420)
 - Partial borrowing [issue#1215](https://github.com/rust-lang/rfcs/issues/1215)
 - View patterns [internals#16879](https://internals.rust-lang.org/t/view-types-based-on-pattern-matching/16879)
 - Permissions [#3380](https://github.com/rust-lang/rfcs/pull/3380)
 - Field projection [#3318](https://github.com/rust-lang/rfcs/pull/3318)
 - Fields in Traits [#1546](https://github.com/rust-lang/rfcs/pull/1546)
 - ImplFields [issue#3269](https://github.com/rust-lang/rfcs/issues/3269)

(F) Proposals of partial initializing
 - Direct and Partial Initialization using ref uninit [#2534](https://github.com/rust-lang/rfcs/pull/2534)
 - Unsafe lifetime [#1918](https://github.com/rust-lang/rfcs/pull/1918)
 - 

(H) Proposals of Anonymous Types:
 - Structural Record [#2584](https://github.com/rust-lang/rfcs/pull/2584)
 - Anonymous variant types, a minimal ad-hoc sum type[#2587](https://github.com/rust-lang/rfcs/pull/2587)
 - Disjoins (anonymous enums) [#1154](https://github.com/rust-lang/rfcs/pull/1154)
 - Anonymous enum types called joins, as A | B [#402](https://github.com/rust-lang/rfcs/pull/402)
 - Anonymous enum types (A|B) take 2 [#514](https://github.com/rust-lang/rfcs/pull/5142)

(any.details) Alternative for another names or corrections for Partial Types.


# Prior art
[prior-art]: #prior-art

Most languages don't have such strict rules for references and links as Rust, so this feature is almost unnecessary for them.


# Unresolved questions
[unresolved-questions]: #unresolved-questions

Is is possible after creating one variable with missed field, move (partly) it into another variable, and then independently extend same field at both variables?


# Future possibilities
[future-possibilities]: #future-possibilities

Any of modules (A), (B), (C), (D), (E), (F), (G), (H).
