# SoundCloud data challenge solution

This solution consists of three approaches:

1. An algorithmic Scala solution.
2. A map-reduce Haskell solution.
3. A for-fun Prolog solution.

## An algorithmic solution.

The problem is basically a depth-aware undirected graph traversal.
After reading the data, we transform it to the adjacency list.
We then perform a depth first search through the graph for each
vertex. Time complexity of one traversal is `O(|E|)`. Since we're
doing a traversal for each vertex, it is `O(|E| * |V|)`. Also, since we're
not traversing all the graph, but only to the required depth `d`, it
is `O(|V| * bᵈ)`, where `b` is the branching factor.
We're using adjacency list, so space complexity is `O(|V| + |E|)`.

Presented solution makes heavy use of effect control from scalaz
library. The package object, where all the interesting stuff is going
on, is a [cake](http://jonasboner.com/2008/10/06/real-world-scala-dependency-injection-di/).

To run:

```
$ ./sbt # the provided script will work if you don't have sbt installed
> run path/to/file/with/data N path/to/output/file # Where N is a number of levels in a problem
```

## A map-reduce solution.

I gave a shot at a MapReduce solution, using the Hadoop streaming
API. Did it in Haskell. I haven't programmed any MapReduce jobs
before, so not sure, if it is an ok solution. ~~Basically, for
each vertex mapper does a breadth first search to the required level,
spitting out all the vertices it encountered on its way. The reducer then
cleans up the list produced by the mapper, removes duplicates and does
the sorting.~~ The solution was not ok: it loaded the whole graph into
each mapper. The new solution presents a one-step mapper and
reducer. At each step mapper broadens frontier, adding one more level
of reachable vertices. Reducer then cleans up the mess, produced by
mapper, joining all the data per source vertex into one line. To get
to the second level of friends, run mapper + reducer twice:

```
λ Alonzo_Church haskell → λ git master* → cat ~/workspace/soundcloud-challenge/misc/test.txt | ./MapperOneStep | ./ReducerOneStep | ./MapperOneStep | ./ReducerOneStep                                                                           
brendan	kim	omid	torsten           
davidbowie	kim	mick	omid	torsten	ziggy
kim	brendan	davidbowie	omid	torsten	ziggy
mick	davidbowie	ziggy
omid	brendan	davidbowie	kim	torsten	ziggy
torsten	brendan	davidbowie	kim	omid
ziggy	davidbowie	kim	mick	omid
```

## Logic-programming solution.

Graph search problems are very naturally solved by logic programming
techniques. I implemented one of the possible approaches in
Prolog (which is a language behind [IBM Watson](http://www-03.ibm.com/innovation/us/watson/) and [WolframAlpha](http://www.wolframalpha.com/)).
Unfortunately, I don't have a big expertise in logic programming,
only recently got interested in it, so the solution is
a bit hacky. It is also not complete: it basically only implements the
mapper part from the previous solution.
Nevertheless, it is extremely declarative: the path search algorithm
is correct if what we mean by 'path' is correct. Here we define path
of length n to be one step and a path of length n + 1 after it. The
actual graph traversal is done by Prolog itself (by the way, Prolog
actually performs a depth first search).
All paths of length 2 are obtained the following way:

```prolog
findall((X, Path), path(X, Y, Path, 2), List).
```

To run, you will need to have swi-prolog installed.

```
$ cd path/to/project/src/main/prolog
$ swipl
Welcome to SWI-Prolog (Multi-threaded, 64 bits, Version 6.2.6)
Copyright (c) 1990-2012 University of Amsterdam, VU Amsterdam
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software,
and you are welcome to redistribute it under certain conditions.
Please visit http://www.swi-prolog.org for details.

For help, use ?- help(Topic). or ?- apropos(Word).

?- [soundcloud].
% soundcloud compiled 0.00 sec, 12 clauses
true.

?- findall((X, Path), path(X, Y, Path, 2), List).
List = [ (davidbowie, [davidbowie, omid, torsten]), (davidbowie, [davidbowie, kim, torsten]), (kim, [kim, torsten, omid]), (kim, [kim, torsten, brendan]), (torsten, [torsten, omid, davidbowie]), (brendan, [brendan, torsten|...]), (brendan, [brendan|...]), (ziggy, [...|...]), (..., ...)|...].

```

**Funny fact:** Scala and Haskell are both logic programming languages
... in type system. So, having implicit vals/typeclass instances, and
implicit vals/typeclass instances depending on them presents us with
most of Prolog's instruments, and the process of implicit/typeclass
resolution is actually the famous [unification](http://en.wikipedia.org/wiki/Unification_(computer_science)) process.
This approach is exposed in Miles Sabin's [shapeless](https://github.com/milessabin/shapeless/) library.
