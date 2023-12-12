- Feature Name: `partial_types_sum`
- Start Date: 2023-12-01
- RFC PR: [rust-lang/rfcs#0000](https://github.com/rust-lang/rfcs/pull/0000)
- Rust Issue: [rust-lang/rust#0000](https://github.com/rust-lang/rust/issues/0000)


# Summary
[summary]: #summary

This proposal is universal flexible tool to work **safe** with partial Enums.

Advantages: maximum type safety, maximum type control guarantee, no ambiguities, zero-cost-binary, flexibility, usability and universality.


# Motivation
[motivation]: #motivation

Partial Types to Sum Types proposal is a parallell, but syncronized with Partial Types to Product Types proposal for "partial borrowing"-like proposals. 

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

Partial Types extension gives to variables with Enum types a good **mathematical guarantee** to checker that use variable with partial type **fully safe**.

And since it is a guarantee by **type**, not by **values**, it has _zero cost_ in binary! Any type error is a compiler error, so no errors in the runtime.

This extension is not only fully backward-compatible, but is fully forward-compatible! Forward-compatibility is an ability to use updated functions old way.

# Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

Partial (Sub-)Types are types, which has not full range of possible values and they have limited access to own fields.

Partial Types are not Contract Types (Types with invariant), but this proposal could coexist with Contract Types.

Partiality of type (or partial type access) is written as `Path.{fld1, fld2, ..}` after Path (Type name), where `fld1`, `fld2`, .. are only permitted fields of this type, the rest of fields has denied access.

It is forbidden to use somehow denied fields (like have outside access to private field), including access to read such field, to write, to borrow, to move. It is a compile error if someone try to access it.

## Partial Enums

For Sum Types (`ST = T1 or T2 or T3 or ..`), for Enums partiality to type addition is **enough** to have **full flexibility** of using those sub-types.

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
    MyEnum::A(_)  => print_A(e),     // e : MyEnum.{A} inferred
    b @ MyEnum::B(..) => print_B(b), // b : MyEnum.{B} inferred
    _ => (),                         // e : MyEnum.{C, D}; inferred
  }
}
```

Type checker must guess (without calculating) from assigning **values**, binding **values** and matching **values** which is partial sub-type of Enum type. The more clever type-checker is, then more conclusions of Enum sub-type it has.

Sum-Typed argument type must match with function parameter type or argument type could has **less** permitted partiality then parameter type.
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


# Reference-level explanation

The core Idea of this proposal is "Proxy Borrowing" - we borrow the whole variable, but borrow-checker pretends it borrow just permitted fields.

And Type-checker gives a mathematical guarantee, than all denied fields remain intact and all partly-immutable fields remain immutable! 

And this mean, that Proxy Borrowing borrowing is fully **safe** and _zero cost_ in binary.

## Syntax

Second, but still important - syntax.

### Partiality Syntax

Minimal Partiality we could write:
```
Partiality:      .{ PartialFields* }
PartialFields:   PermittedField (, PermittedField )* ,?
PermittedField:  IDENTIFIER | TUPLE_INDEX | * | _ 
```

If no implicit rules are used, then we could get rid of `*` and `_` quasi-fields.
```
PermittedField:  IDENTIFIER | TUPLE_INDEX
```

### Partial Enums syntax

**_(A)_**

The only one syntax needed to Enum - is update `TypePath` into
```
TypePath:   ::? TypePathSegment (:: TypePathSegment)* Partiality?
```
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

(4) If `type_prtlty.is_empty()` or `var_prtlty.is_empty()` (if they are explicitly written as '`.{}`') then Error

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


# Drawbacks
[drawbacks]: #drawbacks

- it is definitely not a minor change


# Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

Proposals that are alternatives to Partial Sum Types in a whole:
 - Enum variant types [#2593](https://github.com/rust-lang/rfcs/pull/2593)
 - Enum Variant Types [lang_t#122](https://github.com/rust-lang/lang-team/issues/122)


# Prior art
[prior-art]: #prior-art

Most languages don't have such strict rules for references and links as Rust, so this feature is almost unnecessary for them.

# Unresolved questions
[unresolved-questions]: #unresolved-questions




# Future possibilities
[future-possibilities]: #future-possibilities

Any of modules (A), (B), (C?), (D), (E), (F), (G), (H), (IJ), (K), (L).
