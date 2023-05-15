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

For Expressions, implicit partiality is `.{_}` ("don't care" partiality). 

We could alternatively permit to implicitly infer partiality for arguments. In this case we must explicitly write `.{_}` or `.{*}` to prevent inferring.

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
Point::ref_xy(& p1.{'a});
```

Product-Typed argument type must match with parameter type for function or argument type could has **more** permitted partiality then parameter type.
```rust
// Struct ~ Product Type
struct S4 {a : i32, b : i32, c : i32, d : i32}

fn do_sab(s : S4.{a, b}) { /* .. */ }

let s = S4 {a: 6, b: 7, c: 8, d: 9};

do_sab(s.{*});       // s.{*} - Ok
do_sab(s.{a, b, c}); // s.{a, b, c} - Ok
do_sab(s);           // s.{a, b}, implicit partiality - Ok
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
If it is secure, then this sub-proposal is good. 

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

## Explicit Deny Fields

**_(E)_** sub-proposal, which could be added together or after (B), it is better before (F)

This extension is not a mandatory. Tuple type has "naked" structure, so it would be handy have more pretty visuals, instead of mark all permitted fields in "partiality", write `deny` before denied field.
```rust
let t :: (i32, &u64, f64, u8).{1,3};
// same as
let t :: (deny i32, &u64, deny f64, u8);
```

This extension is not just pretty, but useful with extension (F) partial initializing Tuples.

## Partial Initializing and Pattern Deconstruction Structs and Tuples

**_(F)_** sub-proposal, which could be added together or after (B), it is better after (E)

All syntax and semantic is ready to have implicit partial initializing and partial pattern deconstruction Structs. If not all fields are initialized, then variable has partial type.

Alternative to implicit partial initialization is explicit, 
```rust
struct S4 {a : i32, b : i32, c : i32, d : i32}

let s_abcd : S4 = S4 {a: 6, b: 7, c: 8, d: 9};

// Explicit partial initialization
let s_bd  : S4.{b, d}    = S4 {b: 7, d: 9, .. deny};
let s_a   : S4.{a}       = S4 {a: 6, .. deny};
// Implicit partial initialization (alternative)
let s_a   : S4.{b}       = S4 {b: 26};

// Explicit partial initialization
let s_bc  = S4 {b: 7, c: 8, .. deny};       // s_bc  : S4.{b, c}
let s_c   = S4 {c: 8, .. deny};             // s_c   : S4.{c}
// Implicit partial initialization (alternative)
let s_d   = S4 {d: 9};                      // s_d   : S4.{d}

let s_abc : S4.{a, b, c} = S4 {a: 6, b: 7, c: 8, deny d};
let s_abd = S4 {a: 6, b: 7, deny c, d: 9};  // s_abd : S4.{a, b, d}
```
Sure, it is forbidden to fill private fields outside of module.

If extension (E) Explicit Deny Fields is added, then partial initializing Tuples is also possible with `deny` pre-field and maybe **miss** Expr if type is clear.

Or partial expression is used:
```rust
let t_0123: (i32, u16, f64, f32) = (6i32, 7u16, 8.0f64, 9.0f32);

let t_013 : (i32, u16, f64, f32).{0, 1, 3} = (6i32, 7u16, deny 8.0f64, 9.0f32);
let t_0   : (i32, u16, f64, f32).{0}       = (6i32, deny, deny,        deny);

let t_012 : (i32,      u16,      f64,      deny f32) = (6i32, 7u16,      8.0f64, deny 9.0f32);
let t_23  : (deny i32, deny u16, f64,      f32)      = (deny, deny 7u16, 8.0f64, 9.0f32);
let t_3   : (deny i32, deny u16, deny f64, f32)      = (deny, deny,      deny,   9.0f32);

let t_01  : (i32, u16, f64, f32).{0, 1} = (6i32, 7u16, 8.0f64, 9.0f32).{0, 1};

let t_02 = (6i32, deny 7u16, 8.0f64, deny 9.0f32); // t_02 : (i32, u16, f64, f32).{0, 2}

let t_12 = (6i32, 7u16, 8.0f64, 9.0f32).{1, 2};    // t_12 : (i32, u16, f64, f32).{1, 2}
```

If extension (E) Explicit Deny Fields is added, then alternative partial initializing Structs is also possible with `deny` pre-field and maybe **miss** Expr if type is clear.
```rust
struct S4 {a : i32, b : i32, c : i32, d : i32}

let s_abc : S4.{a, b, c} = S4 {a: 6, b: 7, c: 8, deny d};
let s_abd = S4 {a: 6, b: 7, deny c, d: 9};  // s_abd : S4.{a, b, d}
```

## Update Structs by Partial Structs

**_(G)_** sub-proposal, which could be added together or after (B)

It is also become possible to use **several** variables which have partial struct-type and their permitted fields must not overlap, for updating omitted fields:
```rust
struct S4 {a : i32, b : i32, c : i32, d : i32}

let s = S4 {a: 8, b: 9, c: 11};
let rs_c = & s.{c};

// if (F)
let s_ab : S4.{a, b} = S4 {a: 6, b: 7, .. deny};

// if (F)
let s_abc = S4{b: 11, ..s_ab, .. *rs_c, .. deny}; // s_abc : S4.{a, b, c}

let s1 : S4 = S4{b: 11, d: 17, ..s_ab, .. *rs_c};

let s2 : S4 = S4{d: 17, ..s_ab, .. *rs_c};
```

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

## Partial Uniniting Types

**_(IJ)_** sub-proposal, which could be added together or after (B) 

Rust allow to write uninitialized variables. But they are fully uninitialized. This extension allow to write much more.

We add `uninit` before Type Name and we also add partiality to it - Partial Uniniting.

If we use explicit unitialization (together with (F)), if implicitly, then instead of (F) (it is better remain Error if not fields are filled)
```rust
struct S4 {a : i32, b : i32, c : i32, d : i32}

let s_bd  : uninit.{a, c} S4 = S4 {b: 7, d: 9, ..uninit};
```

If we also use extension (E), then we could also write uniniting for tuples and more flexible 
```rust
let s_bd  : uninit.{c} S4 = S4 {a: 3, b: 7, uninit c, d: 9};

let t_1   : uninit.{1} (i32, u16, f64, f32) = (uninit, 4, uninit, uninit);

let t_3   : (uninit i32, uninit u16, uninit f64, f32) = (uninit, uninit, uninit, 9.0f32);

let t_02 = (6i32, uninit 7u16, 8.0f64, uninit 9.0f32); // t_02 : (i32, uninit u16, f64, uninit f32)
```

 If extension (E) is off, we must allow infer uniniting from the type:
```rust
let t_3   : uninit.{0,1,2} (i32, u16, f64, f32) = (1, 7, 8, 9.0f32);
```
Sure, it is forbidden to read, to move and to borrow uninit fields.

Now we could create self-Referential Types:
```rust
struct SR <T>{
    val : T,
    lnk : & T, // reference to val
}

let x : uninit.{lnk} SR<i32> = SR {val : 5, uninit lnk };

x.lnk = & x.val;
    // x : SR<i32>;
```

This Uniniting as we added is enough to extend by "referential uniniting" (and not only for Partial Types, but for any types (Sized at least)), but due complexity it is not part of this extension.

**_(J+)_** *independent sub-proposal* **Uniniting Types or Movable and Referential Uniniting**

Most important: Uniniting variable after initialization (droping uniniting), is no longer `uninit`!
```rust
let a :  i32;
// a : uninit i32;

a = 7;
// a : i32; uninit is "drop"/inited
```

Movable non-refential uniniting is easy: uniniting is "moved" by move from sender to receiver.
```rust
let a :  i32;
// a : uninit i32;

let b = a;
// b : uninit i32;
// a : i32; // not longer uninit, but moved
```

Referential Uniniting is a bit complicated. 

(1) Uniniting is "moved" by move from sender to receiver (reference)

(2) Uniniting Reference is always "exclusive", regardless if it mutable or not (till drop).

(3) Uniniting dereferenceble Variable is always at least once write-only, regardless if it mutable or not

(4) Uniniting Reference is forbidden to move (after initialization, reference is not longer Uniniting).

(5) Uniniting Reference is forbidden to **drop** (after initialization, reference is not longer Uniniting). 

```rust
let a :  i32;
// a : uninit i32;

let b = &a;
// b : & uninit i32;
// a : i32; // not longer uninit, but exclusive borrowed!

*b = 7;
// b : & i32;
// now reference 'b' could be droped

drop(b);
// a == 7
```

Uniniting Parameters are similar to Referential Uniniting

(1) Uniniting must be written explicitly at Type

(2) If Uniniting Parameter is not-reference, then it behaves same as uniniting non-reference.

(3) If Uniniting Parameter is reference, then it behaves same as uniniting reference.

(4) If Uniniting Parameter is reference initialization must happens before return or together with return;

```rust
struct S4 {a : i32, b : i32, c : i32, d : i32}

impl S4 {
    fn init_a(self : & unit.{a} Self.{a}) {
        *self.a = 5;
    }
}
```
Uniniting arguments, again easy: uniniting of argument and parameter must match! It is error if not.

```rust
struct S4 {a : i32, b : i32, c : i32, d : i32}

let s : uninit.{a} S4 = S4 { uninit a, b : 7, c : 33, d : 4};

s.init_a();
```

## Partial Unions

**_(K)_**

Unions are always is unsafe to use. Partiality could be extended to `Unions` same as for `Struct`.

But it do not make using units more safe. 


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

Minimal Partiality we could write:
```
Partiality:      .{ PartialFields* }
PartialFields:   PermittedField (, PermittedField )* ,?
PermittedField:  IDENTIFIER | TUPLE_INDEX | * | _ 
```

If no implicit rules are used, then we could not get rid of `*` and `_` quasi-fields.
```
PermittedField:  IDENTIFIER | TUPLE_INDEX
```

**_(Stage 2B)_**

If we wish to have "recursive" sub-partial Product-types for (B) and / or (D)
```
Partiality:       .{ PartialFields* }
PartialFields:    PartialField (, PartialField )* ,?
PartialField:     PermittedField Partiality?
PermittedField:   IDENTIFIER | TUPLE_INDEX | * | _
```

**_(Stage 2A)_**

If we wish to have "recursive" sub-partial Enum-types for (A)
```
Partiality:        .{ PartialFields* }
PartialFields:     PartialField (, PartialField )* ,?
PartialField:      PermittedField PartialSubFields?
PartialSubFields:  { PartialSubField (, PartialSubField )* ,? }
PartialSubField:   SpecificSubField Partiality
PermittedField:    IDENTIFIER | TUPLE_INDEX | * | _
SpecificSubField:  IDENTIFIER | TUPLE_INDEX
```

**_(Stage 2A+2B)_**

Finally, Partiality with full and maximum control and flexibility:
```
Partiality:            .{ PartialFields* }
PartialFields:         PartialField (, PartialField )* ,?
PartialField:          PermittedField PartialSubEnumFields? Partiality?
PartialSubEnumFields:  { PartialSubEnumField (, PartialSubEnumField )* ,? }
PartialSubEnumField:   SubEnumField Partiality
PermittedField:        IDENTIFIER | TUPLE_INDEX | * | _
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

### Explicit Deny Fields syntax

**_(E)_**

For Tuple Type we need to update `TupleType` again:
```
TupleType:        ( ) | ( ( TupleTypeSingle , )+ TupleTypeSingle? ) Partiality?
TupleTypeSingle:  deny? Type
```

### Partial Initializing Structs and Tuples Syntax

**_(F)_**

No special syntax for implicit partial initialization of Struct is don't needed.

For explicit partial initialization of Struct we update `StructBase`.
```
StructBase:  .. ( deny | Expression )
```

If (E) extension is on, then we also need to change `StructExprField` and `StructExprTuple` for Structs:
```
StructExprField:  OuterAttribute * ( deny? IDENTIFIER | deny TUPLE_INDEX | (IDENTIFIER | TUPLE_INDEX) : Expression )

StructExprTuple:  PathInExpression ( ( TupleExprSingle (, TupleExprSingle)* ,? )? )
```

If (E) extension is on, then we also need to change `TupleExpr`:
```
TupleExpression:  ( TupleElements? )
TupleElements :   ( TupleExprSingle , )+ TupleExprSingle?
TupleExprSingle:  deny | deny? Expression
```

### Update Structs by Partial Structs Syntax

**_(G)_**

For updating Struct by several Structs, then we change `StructBase` to `StructBases`:
```
StructExprStruct:  PathInExpression { (StructExprFields | StructBases)? }
StructExprFields:  StructExprField (, StructExprField)* (, StructBases | ,?)
StructBases:       StructBase (, StructBase )* ,?
StructBase:        .. Expression
```

### Inferred Structs and Enums Syntax

**_(H)_**

This extension is need to support `..` (or `_`) "rest of fields" field name to infer Enum / Struct fields.
```
StructExprStruct:  PathInExpression { ( .. | StructExprFields | StructBase)? }
```

### Partial Uniniting Types Syntax

**_(IJ)_**

First 
```
Uniniting:      uninit Partiality?
ReferenceType:  & Lifetime? PartialMutability?  Uniniting? TypeNoBounds
```

and create `UnunitingType` for "naked" Uniniting Types and add it into  and insert into `TypeNoBounds`
```
UnunitingType:  Uniniting TypeNoBounds
TypeNoBounds:   ... | ReferenceType | UnunitingType | ...
```

If (E) extension is on or/and (F) extension is on we replace all `deny` into `deny  | uninit`.

So, maximum changes are: 
```
DenyOrUninit:     deny | uninit

StructBase:       .. ( DenyOrUninit | Expression )
StructExprField:  OuterAttribute * ( DenyOrUninit? IDENTIFIER | DenyOrUninit TUPLE_INDEX | (IDENTIFIER | TUPLE_INDEX) : Expression )
TupleExprSingle:  DenyOrUninit | DenyOrUninit? Expression
```

**_(J+)_**

No special syntax is needed.

### Partial Unions Syntax

**_(K)_**

No special syntax is needed.

### Partial Unions Syntax

**_(IJ)_**

No special syntax is needed.

## Logic Scheme

Third, but still important - Logic Scheme.

For pseudo-rust we suppose, partiality is a `HashSet` of permitted field-names.

Common rules:
```rust
fn bar(v : SomeType.{'type_prtlty}) 
{ /* .. */ }

let v : SomeType.{'var_prtlty}; 
```
Then:

(1) If `SomeType` is not supported type (Enum, Struct, Tuple) then Error.

(2) If partiality has no extra field-names `type_prtlty.is_subset(full_prtlty)` it compiles, otherwise Error.

(3) If `var_prtlty.is_subset(full_prtlty)` it compiles, otherwise Error.

(4) If `type_prtlty.is_empty()` or `var_prtlty.is_empty()`  (if they are explicitly written) then Error

### Partial Enums Logic Scheme

**_(A)_**

Let we have (pseudo-rust) and `enm_param_prtlty` and `enm_arg_prtlty` are `HashSet` of permitted field-names: 
```rust
fn bar(e : SomeEnum.{'enm_param_prtlty}) 
{ /* .. */ }

let e : SomeEnum.{'enm_arg_prtlty}; 
bar(e);

impl SomeEnum.{'enm_impl_prtlty} {
    fn foo(self : Self.{'enm_slf_prtlty}) 
	{ /* .. */ }
}

e.foo();
```
Then:

(1) If `enm_arg_prtlty.is_subset(enm_param_prtlty)` it compiles, otherwise Error (**inverse** of Struct). 

(2) If `enm_slf_prtlty.is_subset(enm_impl_prtlty)` it compiles, otherwise Error (same as Struct).

(3) Let we have several implementations for same type, but different partiality. And `all_enm_impl_prtlty` is an `array` of each `enm_impl_prtlty`.

If `all_enm_impl_prtlty.iter().any(|&eip| enm_arg_prtlty.is_subset(eip))` it compiles, otherwise Error (same as Struct+).

(4) If `1 == all_enm_impl_prtlty.iter().fold(0, |acc, &eip| if enm_arg_prtlty.is_subset(eip) {acc+1} else {acc})` it compiles, otherwise ?Error.

We expect that just one "implementation" partiality is match and we choose it for calling a method.

### Partial Struct and Tuples Logic Scheme

**_(B)_**

Let we have (pseudo-rust) and `st_param_prtlty` and `st_arg_prtlty` are `HashSet` of permitted field-names: 
```rust
fn bar(s : SomeStructOrTuple.{'st_param_prtlty}) 
{ /* .. */ }

let s : SomeStructOrTuple.{'st_arg_prtlty}; 
bar(s);

let rsp = & s.{'expr_prtlty};

impl SomeStructOrTuple.{'st_impl_prtlty} {
    fn foo(self : Self.{'st_slf_prtlty}) 
	{ /* .. */ }
}

s.foo();
// (4) desugars into:
SomeStructOrTuple.{'st_impl_prtlty}::foo(s.{'st_slf_prtlty});
```
Then:

(1) If `st_arg_prtlty.is_superset(st_param_prtlty)` it compiles, otherwise Error (**inverse** of Enum).

(2) If `expr_prtlty.is_subset(st_arg_prtlty)` it compiles, otherwise Error.

(3) If `st_slf_prtlty.is_subset(st_impl_prtlty)` it compiles, otherwise Error (same as Enum).

(4) Updating desugaring for `self` (and `Rhs`) variables.

Desugaring `s.foo()` into `SomeStructOrTuple.{'st_impl_prtlty}::foo(s.{'st_slf_prtlty})` .

(5) It has **no sense** to have several implementation of same product-type and different partiality. 

(5+) Anyway let we have several implementations for same type, but different partiality. And `all_st_impl_prtlty` is an `array` of each `st_impl_prtlty`.

If `all_st_impl_prtlty.iter().any(|&sip| st_arg_prtlty.is_subset(sip))` it compiles, otherwise Error. (same as Enum).

(6+) If `1 == all_st_impl_prtlty.iter().fold(0, |acc, &sip| if st_arg_prtlty.is_subset(sip) {acc+1} else {acc})` it compiles, otherwise ?Error.

We expect that just one "implementation" partiality is match and we choose it for calling a method.

### Several Selfs Logic Scheme

**_(C)_**

Let we have (pseudo-rust) and partiality "variables" are `HashSet` of permitted field-names: 
```rust
let s : SomeStructOrTuple.{'st_arg_prtlty};

impl SomeStructOrTuple.{'st_impl_prtlty}<Smv = Self> {
    fn foo(self : Self.{'st_slf_prtlty}, smv : Smv.{'st_smv_prtlty})
	{ /* .. */ }
}

s.foo();
// (2) desugars into:
SomeStructOrTuple.{'st_impl_prtlty}::foo(s.{'st_slf_prtlty}, s.{'st_smv_prtlty});
```
Then:

(1) Adding `Smv = Self`(always `Self`) as "same variable" general parameter.

(2) If `st_smv_prtlty.is_subset(st_impl_prtlty)` it compiles, otherwise Error (same as for `self`).

(3) Adding a rule for `Smv` general parameter.

Desugaring `s.foo()` into `SomeStructOrTuple.{st_impl_prtlty}::foo(s.{st_slf_prtlty}, s.{st_smv_prtlty})` .

### Partial Mutability Logic Scheme

**_(D)_**

Let we have (pseudo-rust) and partiality "variables" are `HashSet` of permitted field-names: 
```rust
let mut.{'mut_var_prtlty} s : SomeStructOrTuple.{'st_var_prtlty};

let mrsp = &mut.{'mut_arg_prtlty} s;
```
Then:

(1) If `mut_arg_prtlty.intersection(st_var_prtlty).is_subset(mut_var_prtlty)` it compiles, otherwise Error

### Explicit Deny Fields Logic Scheme

**_(E)_**

No special rules requires.

### Partial Initializing Structs and Tuples Logic Scheme

**_(F)_**

No special rules requires.

### Update Structs by Partial Structs Logic Scheme

**_(G)_**

Let we have (pseudo-rust) and partiality "variables" are `HashSet` of permitted field-names: 
```rust
let s1 : SomeStruct.{'st1_prtlty};
let s2 : SomeStruct.{'st2_prtlty};
/* .. */
let sN : SomeStruct.{'stN_prtlty};

let snew : SomeStruct = SomeStruct {..s1, ..s2, /* */ ..sN};

let sprt = SomeStruct {..s1, ..s2, /* */ ..sN, .. deny};
```
Then:

(1) If for any `J > I` we have `st<I>_prtlty.is_disjoint(st<J>_prtlty) == true` it compiles, otherwise Error ;

(2) If for `snew` variable `full_prtlty.difference(st1_prtlty).difference(st2_prtlty)/* */.difference(stN_prtlty).is_empty()` it compiles, otherwise Error.

### Inferred Structs and Enums Logic Scheme

**_(H)_**

No special rules requires.

### Partial Uniniting Types Logic Scheme

**_(IJ + J+)_**

Let we have (pseudo-rust) and partiality "variables" are `HashSet` of permitted field-names: 
```rust
let s : uninit.{'uninit_var_prtlty} SomeStructOrTuple.{'st_var_prtlty};

let rsp : & uninit.{'uninit_arg_prtlty} = & s.{'st_expr_prtlty};

// `s` change uniniting after consuming into 
// s : uninit.{'uninit_after_prtlty} SomeStructOrTuple.{'st_var_prtlty};
```
Then:

(1) If `uninit_var_prtlty.is_subset(st_var_prtlty)` it compiles, otherwise Error

(2) If `uninit_arg_prtlty.is_subset(uninit_var_prtlty)` it compiles, otherwise Error

(3) If `uninit_after_prtlty.is_subset(uninit_var_prtlty)` it compiles, otherwise Error

(4) If `uninit_var_prtlty.intersection(st_expr_prtlty).is_subset(uninit_arg_prtlty)` it compiles, otherwise Error

(5) If `uninit_var_prtlty.difference(st_expr_prtlty).is_subset(uninit_after_prtlty)` it compiles, otherwise Error

### Partial Unions Logic Scheme

**_(K)_**

No special rules requires.


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

(H) Proposals of Anonymous Types:
 - Structural Record [#2584](https://github.com/rust-lang/rfcs/pull/2584)
 - Anonymous variant types, a minimal ad-hoc sum type[#2587](https://github.com/rust-lang/rfcs/pull/2587)
 - Disjoins (anonymous enums) [#1154](https://github.com/rust-lang/rfcs/pull/1154)
 - Anonymous enum types called joins, as A | B [#402](https://github.com/rust-lang/rfcs/pull/402)
 - Anonymous enum types (A|B) take 2 [#514](https://github.com/rust-lang/rfcs/pull/5142)

(IJ) Proposals of partial initializing
 - Direct and Partial Initialization using ref uninit [#2534](https://github.com/rust-lang/rfcs/pull/2534)
 - Unsafe lifetime [#1918](https://github.com/rust-lang/rfcs/pull/1918)
 
(any.details) Alternative for another names or corrections for Partial Types.


# Prior art
[prior-art]: #prior-art

Most languages don't have such strict rules for references and links as Rust, so this feature is almost unnecessary for them.


# Unresolved questions
[unresolved-questions]: #unresolved-questions

(C) Security of using this extension.
Is it secure not to check the origin if variable is the same if we explicitly write associated function?
```rust
impl Bar<Smv = Self>{
    fn foo(self : &mut Self::{x}, smv: & Smv::{y}) { /* */ }
}
Bar::foo(&mut bar.{x}, & bar.{y}); // Ok
Bar::foo(&mut bar.{x}, & baz.{y}); // Error? Ok?
```
I think it is insecure, error, but who knows.
If yes, then Ok.


# Future possibilities
[future-possibilities]: #future-possibilities

Any of modules (A), (B), (C), (D), (E), (F), (G), (H), (IJ), (K).
