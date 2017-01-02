# purescript-infinite-list: Infinite lists in purescript

[![Build Status](https://travis-ci.org/robertmassaioli/purescript-infinite-list.svg?branch=master)](https://travis-ci.org/robertmassaioli/purescript-infinite-list)

This language allows you to have infinite lists that are:

 - Type-safe
 - Memory efficient
 - Implement [Functor](https://pursuit.purescript.org/packages/purescript-prelude/2.1.0/docs/Data.Functor#t:Functor)
 - Implement [Filterable](https://pursuit.purescript.org/packages/purescript-filterable/1.0.0/docs/Data.Filterable#t:Filterable)

You can do this with an easy to understand interface that guarantees infinite lists.

For example, to create an infinite list of the natural numbers:

    naturals = iterate ((+) 1) 1

Then, to get only the elements that are divisible by three or five, you could write:

    byThreeOrFive = filter (\x -> (x `mod` 3 == 0) || (x `mod` 5 == 0)) naturals

You can then multiply every number in that list by two:

    byThreeOrFiveDoubled = map (\x -> x * 2) byThreeOrFive

Running this through psci you then get the following results:

    > take 10 byThreeOrFiveDoubled
    (6 : 10 : 12 : 18 : 20 : 24 : 30 : 36 : 40 : 42 : Nil)

    >

You can use the methods provided by this library to create many different kinds
of infinite lists. Please read the documentation further to learn how to further
generate and manipulate infinite lists.
