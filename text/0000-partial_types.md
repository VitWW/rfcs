- Feature Name: `partial_types`
- Start Date: 2023-05-10
- RFC PR: [rust-lang/rfcs#0000](https://github.com/rust-lang/rfcs/pull/0000)
- Rust Issue: [rust-lang/rust#0000](https://github.com/rust-lang/rust/issues/0000)


# Summary
[summary]: #summary

Partial Types proposal is a generalization on "partial borrowing"-like proposals.

This proposal is universal flexible tool to work **safe** with partial parameters, partial arguments, partial references and partial borrowing.

Advantages: maximum type safety, maximum type control guarantee, no ambiguities, flexibility, usability and universality.


# Motivation
[motivation]: #motivation

Safe, Flexible controllable partial parameters for functions and partial consumption (including partial not borrowing) are highly needed.

Partial Types extension gives to Sum Types (`ST = T1 or T2 or T3 or ..`), Enums first of all, a good tool for "partial functions".

Partial Types extension gives to Product Types (`PT = T1 and T2 and T3 and ..`), Structs and Tuples first of all, a good **mathematical guarantee** to borrow-checker that by `&var.{x,y}` or `&mut var.{z,t}` borrowing the whole variable and pretending to borrow just several fields is **fully safe**.

And since it is a guarantee by **type**, not by **values**, it has _zero cost_ in binary! Any type error is a compiler error, so no errors in the runtime.

This extension is not only fully backward-compatible, but is fully forward-compatible! Forward-compatibility is an ability to use updated functions old way.
