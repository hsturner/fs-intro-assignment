(*       F# Tutorial Exercises and Assignment 1, 2022 edition

         By Chuck Liang, For CSC 123/252, Hofstra University

This interactive tutorial will try to teach you a language in a different
way.  As you read this tutorial, type out all the lines marked with
'>' in the F# interpreter and carry out the required exercises.  After
you type something, *record how the interpreter responded.*

Type up the answers to the exercises in a separate file so it can be
submitted.   EXERCISES WERE DUE YESTERDAY!

F# has an interactive interpreter.  The first part of these exercises
will require you to use the interpreter.  Start it in the Linux virtual
machine (Mono) with 'fsharpi'.  On native windows, start it with 'fsi'
(referring to the fsi.exe program in the directory where it's
installed).  Mono is also installed on hucsc3.  I recommend Mono even
if you have windows, unless you have and prefer Visual Studio.  Find 
online resources on FSharp at fsharp.org.

Fsharp programs work like scripts (a main function is not required
unless you're using Visual Studio), but unlike many scripting
languages it is strongly typed. To compile and run a Fsharp program
with a .fs suffix (on Mono, including class VM):

fsharpc program.fs   (optionally -r prog.dll, --target:library, similar to C#)
mono ./program.exe

Another way to write a Fsharp program is to write a program2.fsx, then:
  open the fsharp repl (interactive) program fsharpi  (fsi on windows)
  type #load "program2.fsx";;
       open Program2;;
The module name you open must begin with a capital letter even if the filename
does not.  This loads and executes the program like a script and prints out
the types inferred for your definitions in the program.

But for now, you should use the interactive interpreter (start fsharpi)
---

At the '>' prompt, type

> open System;;

The ;; at the end tells the interpreter that the command is complete and it
should be executed.  A single ; is needed to separate multiple statements on
the same line.  Try the following:

> printfn "a"; printfn "b";;

The ;; is only needed when writing programs inside the interpreter, not when
a program is compiled: normally only python-style indentation is used.


1. ******* Declaring and Using Variables ********

Type the following:

> let x = 1;;
> let mutable y = 1.0;;

Notice that F# inferred the types of x and y as int and float
respectively (don't worry, float is actually a 'double').  F# is very
strict when it comes to types, and is one of the most statically type
safe languages.  But unlike other languages, you usually don't have
to declare what type you're using: it uses type inference aggressively.
However, there are also some unpleasant aspects of this type system:
it will not convert ints automatically to floats: (1+1.0) will give you
an error: you have to use 'float(1)+1.0' or 1+int(1.0).

The word 'mutable' means that y can be changed.  Type:

> y <- y+1;;
> y;;

What is the <- operator?  It's the assignment operation (set!).  F# has lofty
mathematical origins, so '=' is not used for assignment, only for
definitions and for BOOLEAN EQUALITY.  Type

> x = x+1;;

What do you expect here?  The operator '==' is not used in F#.

Most other common arithmetic and boolean operators such as &&, ||, <=,
%, etc. are the same in F#, but there are a couple of other differences:
  
 <> represents inequality (!= in C/Java)
 not(a) represents boolean negation (!a in C/Java).

The ! operator is used to dereference a pointer in F#, thus the differences.

equality and inequality on strings are ok: "abc" = "ab"+"c" returns true (= is
more than just pointer equality, because F# supports operator overloading).


2.           ********** Defining functions ***********

Functions are just lambda terms and you can bind them to variables: that's
one way to define functions:

> let I = fun x -> x;;  // this is lambda x.x in F# syntax
> let K = fun x y -> x;;  // lambda x.lambda y.x

Note that it inferred the type of I as x:'a -> 'a.  Here, 'a is a
generic (polymorphic) type variable.  F# inferred the most general type
for I.  Most other languages (C#, java, kotlin) can infer simple types,
like for 'var x = "abc"', but only a truly functional language in the
class of ML (F#) can infer the generic type of functions.

Applying functions:
> I 3;;
> I(3);;  // same as above

*** EXERCISE I1: define the combinator 
  S = lambda x.lambda y.lambda z. x z (y z).  
Note that the polymorphic types inferred for these terms correspond to
propositional tautologies (read 'a -> 'a as "a implies a").  
*)

(* ----- 2b. Quicker ways to define functions: ------

> let f x = x+1;;
> f 3;;

'let rec' is needed to define recursive functions.  Type

> let rec gcd(a,b) = if (a=0) then b else gcd(b%a,a);;
> gcd(8,12);;

Yes you need the word "then" in the if statement.  If statements are
not used all that much in F#: there are better ways to write most
programs.  Please keep your mind open and try to learn something new.
You can use F# like any old language, but then what's the point?

2c.  Curried and Uncurried functions.

Type and infer the difference between 

> let f x y = x+y;;

and 

> let g(x,y) = x+y;;

Note the types inferred for these functions: Both functions take a
single argument.  g takes a pair of ints and returns an int and f takes
one int and returns a function that expects the other int.  
> f 3 4;;
> f 3;;  // what do you expect here?

When defining functions that take multiple parameters, you would USUALLY
use the non-curried form (g).  However, sometimes the curried form (f) is
the better choice.  One use of the curried form is functions that
can take an undetermined number of arguments (or optional parameters),
like the printf/printfn function:

> printfn "%d %d" 1 2;;

> printfn "%d %d" 1 g(2,4);;  // what do you expect here?

Didn't get what you expect?  Got an error instead?  (Get over it!)
The reason you got an error is that you tried to pass to printfn THREE
arguments instead of the two expected: the int 1, the function g, 
and the pair (2,4).  You would get the same error if you typed

> f 3 g(2,3)    or     > f g(2,3) 4

In other words, every F# function is a lambda term that expects one argument:
when you write let f x y = x+y;  it's equivalent to writing
let f = fun x -> fun y -> x+y;   fun x -> A  is lambda x.A in F#.  The difference
between f and g above is that g's one argument is a pair.   You can also define g as

> let g = fun (x,y) -> x+y;;

Note that there's only one fun in g, whereas f is "more fun" because it's Curried.


*** EXERCISE I2: figure out how to apply printfn to 1 and g(2,4)
correctly.  And in general, the proper way to apply curried functions.
The hint is in the error message.


**** Type annotations.  Type inference works well most of the time,
but sometimes you have to give it a hint, specifically when an
operator like '+' works on multiple types:

> let h(x:float,y:float) = x+y;;
without the type annotation, the types of x,y will be inferred as ints.

**** Scoping of functions, loops, etc can be done by indentation like in python:

> let f x (y:int) =
    let z = string(y)   // convert x to string  (type annotation needed on y)
    printfn "%s" z
    Console.WriteLine(z);;  // because you opened 'System'


****** A function's parameters are not mutable variables:

The following won't work:

> let f x =
    while x>0 do
      printfn "%d" x
      x <- x-1;;      // a parameter is not a mutable variable

It is in fact a common trait among many languages that value parameters
can't be changed.  C/Java is the exception.

*** EXERCISE I3: fix the above function. (dont use references or anything
weird - you just need one more line.)

A function with no arguments can be defined as follows:

> let f() = printfn "hello";;   // function with no argument, no return val

The type 'unit' means void, essentially.

*** EXERCISE I4: devise an experiment to verify that F# uses static as
opposed to dynamic scoping (I will resign if it's dynamic).

Just to remind you, here's such a program in C:
int x = 1;
int f() { return x; } // f must refer to an external (free) variable
int main()
{
   int x = 2;
   printf("%d", f());  // prints 1 if static, 2 if dynamically scoped
   return 0;
}
But there's no main in F# ... unless you use Visual Studio, which makes you
write one.

*** EXERCISE I4.5 (somewhat harder): Using what you learned above,
recreate the "makeaccumulator" program from scheme/perl: write a
function that returns a closure lambda and show that it carries its
own state: I should be able to do:

let a1 = makeaccumulator 0;
let a2 = makeaccumulator 0;
a1 2;; // returns 2
a1 2;; // returns 4
a2 2;; // returns 2

  # reminder: in perl it was:
  sub makeaccumulator {
     my $x = $_[0]; # initial value of accumulator
     sub { $x = $x+$_[0]; $x}
  }

You need to write a function that returns a function inside the
scope of a let ...  Here's a function that takes two functions as
arguments and composes them. Applying the composition of f and g to x
is the same as f(g(x)).  Observe the type that F# inferred for your
makeaccumulator function.

--- *)
let compose f g = 
   printfn "composing functions ..."; // use indentation like python
   fun x -> f(g(x));  // last expression is the return value
// you will see how F# infers the type of this function...
printfn "composed: %d" (compose (fun x -> x+x) (fun y -> y*y) 3);;   // prints 18


(*         ********  Tuples, Lists and Arrays *******   

In addition to the ability to use any .Net data structure, F# has special
syntax for Tuples, lists and arrays:

Type the following in the interactive interpreter

> let a = (1,2,4);;  // This is a tuple of 3 ints, of type int*int*int
> let b = [1;2;4];;  // This is a 'int list'
> let c = [1,2,4];;  // before hitting return, what do you expect here?

c is a list of tuples, in this case just one tuple: (1,2,4).

Tuples can be used like arrays:

> b.Length;;  // returns length
> b.[0];;     // returns the first element: the dot reminds you it's an object.

But lists (and tuples) are immutables.  Arrays are similar to lists, but
are mutable:

> let d = [|2;3;5;7|];; // this has type int[] - an array
> d.[1] <- 9;; // values in the array are mutable.

Arrays can also be created this way:

> let M:char[] = Array.create 10 'a';; // what kind of array does this create?


When defining a function with an array argument, a type annotation is
often needed because the syntax for using arrays clashes with those of
other structures: for example, if you write:

> let f x = x.[0];;  // we wouldn't know if x is a list or an array.

Define it like this instead:

> let f (x:'a []) =   x.[0];;

*** EXERCISE I5. Devise an experiment to test if, when an array is
passed to a function as an argument, whether a reference (pointer) is
passed, or is the entire array is copied.  Your function just need to
change some value in the array, and you need to test if the change is
just local.

Besides tuples, lists and arrays, you can use any .Net data structure
the way they're used in other languages.  Sometimes, F# provides special
syntax so you can use some of them more easily.  In fact the 'a list
data structure is based on System.Collections.Generic.List<T>.

If you
> open System.Collections.Generic;;

You can also use a "Dictionary" (aka associative list or HashMap):

> let ID = Dictionary<string,int>();;  // the word "new" is optional
> ID.["Mary"] <- 702345126;;  
> ID.["Larz"] <- 701555667;; 

> printfn "%d" (ID.["Larz"]);;

So you can write software with some components in F# and others in C#
or some other .Net language.


                     ***** Pointers *****

Efficient data structures such as trees will still require the use of
pointers (see the AVL tree program).

> let x = 1;;
> let px = ref x;; // sets px to point to x 
> px := 5;;        // change referenced value to 5 
> !px;;            // dereferences pointer (returns 5)

F#'s pointers do not work in exactly the same way as C pointers.  In C we can have:

  int x = 1;
  int *px = &x;  // assigns p to memory address where x is located
  *px = 5;       // changes memory contents, THEREFORE CHANGES x also!
  x==5==*px;     // this is now true. x has been change through px.

But in F#, with the code above, x is still 1 after px:=5. !px=x is
false.  This is because the expression (ref x) copies the value of x
into a new memory location before returning the reference.  It doesn't
matter if x is declared mutable or not. This is argued to be safer
because it's less likely that something's changed that shouldn't be
through pointers.  To have the same effect in F#, use two references:

> let qx = px; // qx and px reference the same contents
> qx := 6;     // this will also change what px points to. !px and !qx are both now 6

Pointers are further demonstrated in the sample programs.

To exit the interactive interpreter cleanly:

> exit 0;;   // just like in C
*)


(* --------------------------------------------------------------------

3. *****************  Pattern Matching *****************

   So far, we haven't seen much about F# that's not in other languages
(except for more powerful type inference).  One of the most important
features of F# (and similar languages including ML, Haskell and Scala)
is pattern matching.  The gcd function above can also be defined as
follows:

>let rec gcd x =
    match x with
      | (0,b) -> b
      | (a,b) -> gcd(b%a,a);;
or 
     
>let rec gcd = function
    | (0,b) -> b
    | (a,b) -> gcd(b%a,a);;  // syntax warning: don't use a tab here

The word "function" is just more convenient syntax for matching the
the (last) argument of gcd against patterns.

In older languages such as C, the term "Data Structure" is a bit of a
misnomer.  The "structure" only exists at compile time.  Once
compiled, all "structure" disappears and all data are treated as
blocks of bytes.  In object-oriented languages, some notion of
"structure" is retained at runtime in the form of the runtime/dynamic
type tag that a piece of data is "blessed" with.  In these languages,
we can write "constructors" to construct a data structure from its
components, but there is no corresponding notion of a *destructor*,
which is the ability to decompose a data structure back into its
components and subcomponents, and do it in a way that's type safe: we
do not want to deconstruct a double into a char[8].  Clearly just have
a type tag attached to an object is still not sufficient for this kind
of deconstruction.  In F# and similar languages (which includes ML,
Scala, Haskell, Elm, and Rust to a degree), data must fully retain
their entire structure at runtime, for otherwise pattern matching
would not work.  The match keyword exposes the structure of data at
runtime.  'match x' first exposes x as a tuple of two ints, and then
distinguishes between tuples where the first value is, or is not 0.
The two patterns separated by '|' are tested from top to bottom, so
the second pattern matches all pairs (a,b) where a is not 0.

Some languages (python and perl) have the ability to deconstruct certain
built-in types (in python: (a,(b,c)) = (1,(2,(3,4))) will assign a to 1,
b to 2 and c to (3,4)), but such deconstruction does not apply to user
defined types, where it can be most useful.

In F#, these user-defined types are called **discriminated unions**:
*)

type expr = Num of int | Plus of expr*expr | Times of expr*expr | Minus of expr*expr | Neg of expr;;

//This will allow us to form expression trees such as

let t = Plus(Num(2),Times(Num(4),Neg(Num(3))));;  // (2 + 4 * -3)

//Function to evaluate an expression:
let rec eval = function
   | Num(n) -> n
   | Plus(a,b) -> eval(a) + eval(b)
   | Minus(a,b) -> eval(a) - eval(b)
   | Times(Num(0),a) | Times(a,Num(0)) -> 0  // ***
   | Times(a,b) -> eval(a) * eval(b)
   | Neg(a) -> eval(a) * -1;;

//Function to convert expression tree to infix string:
let rec tostring = function
   | Num(n) -> string(n)
   | Plus(a,b) -> "(" + tostring(a) + " + " + tostring(b) + ")"   
   | Minus(a,b) -> "(" + tostring(a) + " - " + tostring(b) + ")"   
   | Times(a,b) -> tostring(a) + "*" + tostring(b)
   | Neg(Neg(x)) -> tostring(x)     // ***
   | Neg(x) -> "-" + tostring(x);;

System.Console.WriteLine(tostring(t));;  // if you haven't opened system


(*
??? How is Pattern Matching Different from If-Else or Switch Statements ???

If we only pattern matched variables against simple types such as
integers or strings, and only matched one value at a time, then it would
indeed not appear much different from a switch. But there are two
reasons that pattern matching is more than just syntactic sugar for
switch statements.

1. Deep pattern matching means that we can match against compound, nested
data structures, which is not possible without nested if statements, or
nested switches.  The structures that can be matched include tuples of
any length, which means that any number of values can be patterned matched
simultaneously.

2. Try this:

> let rec f = function
    | Num(n) -> n
    | Plus(x,Neg(y)) -> f(x) - f(y);;

or just this:

> let g = function
  | "zero" -> 0
  | "one" -> 1;;

What do you get?  You get a warning that the patterns are incomplete.
If you were to compile the program (fsharpc/fsc) then you will notice
that the warning is given at compile time.  Would you expect such a
warning if you left out a if-else clause or switch case?  (No kids,
you shouldn't - you'd be lucky if it was even a runtime error).  This
is an important difference between pattern matching and if-else and
similar constructs: IT IS MORE TYPE SAFE.  In order for the function g
above to be of type string -> int, it must cover all strings and not
just the two given.

   ??? How is Pattern Matching Different from Dynamic Dispatch ???

1. Dynamic dispatch has some of the same advantages over if-else and
switch, including improved type safety.  However, the object oriented
approach entails *subtyping* (inheritance).  Whenever A is a subtype
(subclass) of B, there is always the possibility that downcasting from
B to A might be needed, which means that not all object-oriented code
can be statically type checked.  But pattern matching does not rely on
inheritance: there is no dynamic dispatch and no type casting here.

2. Dynamic dispatch only works on one object at a time.  In a method call
of the form a.f(b,c,d): dynamic dispatch can only occur on a. But what
about b, c and d?  The visitor pattern (v.accept(this)...) allows
double dispatch, but it's already pushing the boundaries of what
dynamic dispatch is capable of: it only takes the need for one more
dispatch to break it entirely (see the number additions program, which
requires tripe dispatch).

2b. We can dispatch to the right version of a procedure based on the
type tag on a single object, but not on the structure of nested
objects.  In the F# tostring function above, note the case marked ***:
the tostring function treats a double negative (Neg(Neg x)) as a
positive.  This case demonstrates that pattern matching can expose the
entire, nested structure of data at runtime, much more than is
possible with dynamic dispatch (or with the is/instanceof check
combined with if-else), which can only check the structure of data at
the top level.  The closest analog of a discriminated union is an
abstract interface called 'expr' and four subclasses, Num, Neg, Plus
and Times.  Dynamic dispatch may be able to distinguish between these
four cases, but how could it distinguish between Neg(x) and
Neg(Neg(x))?

Even "multiple dispatch", as found in the language Julia, can only
look at the surface type of objects and does not have the power of
deep pattern matching.


****EXERCISE C1 (type up in editor and save as a .fs or .fsx file).
Modify the tostring function so that expressions such as 3 + -4 is
printed as (3 - 4). However, (5 + --2) should be printed as (5 + 2)
and not (5 - -2).  Make sure your function is recursive so you can
handle nested cases: (1 + ---1) should also print as (1 - 1).
Hint: you need to add two clauses. Pattern matching will execute the
first clause where the match occurs (observe that the special cases for
evaluating 0*n and n*0 occur before the more general case for Times; 
the special case for printing --n also appears before the more general
case for Neg).
*)

let t1 = Minus(Num(5),Neg(Num(2))); // 5 - -2
let t2 = Plus(Num(1),Neg(Neg(Neg(Num(1))))); // 1 + ---1
printfn "%s" (tostring t1);; // prints (5- -2) now, but should print (5 + 2)
printfn "%s" (tostring t2);; // prints (1 + -1) now, but should print (1 - 1)

(*
You can save the file with a .fs suffix and compile it with fsharpc
and execute (mono) the .exe, but you can also place it in a .fsx file
and load it into the interactive interpreter and experiment with its
definitions:

> #load "programname.fsx"
> open Programname

Loading programs this way is better for debugging purposes, as it will also
display the inferred types of all definitions.  Strong typing is still enforced
without compiling the program.  Production programs should be compiled.


   ??? Are There Any Disadvantages of Pattern Matching ???

Yes. It would be more difficult to extend the existing definition of
'expr' with another case in a modular way.  And it would not be
possible to add such a feature to the language without compromising
some degree of type safety (as in Scala).  Compared to object-oriented
programming, pure functional programming is more "tightly coupled."  You
must be reasonably sure that you know what an "expression" is before
you write code for it.  We may explore this problem in further
lectures, including another advanced feature called "active patterns".

Because there are trade-offs between the functional/pattern matching
style of programming and the OOP style, F# still contains the ability
to define classes, inheritance, and use dynamic dispatch.  But with
these come the possibility of losing static type safety, along with all the
existing limitations of OOP.  We will not look at how classes are defined
in F#, because they're hardly better than C#, and the syntax is esoteric.
It is also possible to import a .dll built with C# and use it in F#.
--- *)

(*        **** Creating a data structure of our own *****

You can pattern match just about any type, including referenced values.
One useful pattern is for lists.  A list such as [1;2;3] is equivalent
to 1::2::3::[].  Here, :: is "cons" and [] is the empty list.  The
:: operator associates to the right so 1::2::3::[] should be read as
1::(2::(3::[])).

You can pattern match against list patterns:

> let x = [2;3;4;5]  //(or some list)
> match x with
    | [] -> "none"
    | [y] -> "exactly one"  // [y] is same as pattern y::[]
    | (a::b::c) when a=b -> "the first two are the same"
    | (a::b::c::d) as y -> string(y)+" has at least three";;
    | _ -> "two different values"

There are several points to note here:

1. It is possible that a value could match multiple patterns: only the
   first clause that it matches will execute: so the above cannot
   print both "the first two are the same" and "at least three".

2. Note the use of the "when" condition to further discriminate the
   pattern: this is not exactly the same as using an if-else after the
   ->: if you wrote the third match clause as: 
      | (a::b::c) -> if a=b then "the first two are the same"
   it would not try to match against the fourth pattern if the "if" failed.
   (in fact, it won't compile without an else clause, since it must always 
   return a string)

3.  You (unfortunately) cannot use a variable more than once in a pattern,
    so | (a::a::c) -> ... would be invalid.  

4.  The 'as y' is used on the last line so the entire pattern can referenced
    more easily (and efficiently, without recreating the structure).

5.  The last clause uses _ as a wildcard: in this example, the case will
    only match for lists that contain exactly two different values.

We will use pattern matching on lists quite a bit in our programs.

One thing about lists is that it is always easier to add/delete from
the front end:

let push s stack = s::stack   // non-destructive cons onto to stack
let pop s stack =
  match stack with
    [] -> raise (Exception("stack underflow"))
    (a::b) -> b;;

But what if you wanted to add something to the end?  Without maintaining
a pointer to the end of the list, it would not be possible to do it in
constant time.  And even with such a pointer, it would still take O(n)
time to delete from the tail end (unless you have a doubly linked list).

Often when we visualize stacks, we want the top-of-stack to be
at the right end, since we read from left to right.  Thus it can be
helpful to redefine linked lists our own way, so that the 'car' and
the 'cdr' are reversed:
*)
type 'a stack = Emp | Apnd of 'a stack * 'a;; // 'Apnd' for "append"

let s = Apnd(Apnd(Apnd(Emp,2),3),5);; //instead of Cons(2,Cons(3,Cons(5,Emp)));

//Now we can "push and pop" from the right end in O(1) time.

let pushon a stk = Apnd(stk,a);;
let popoff = function
  | Emp -> raise (System.Exception("stack underflow"))
  | Apnd(s,a) -> a;;

//The following function will convert a stack to a F# list, reversing it in the process:

let rec tolist = function
  | Emp -> []
  | Apnd(s,a) -> a::tolist(s);;

//The above function is not tail recursive. The following function is 
//tail recursive and will not reverse the list:

let rec tolist2 ax = function
  | Emp -> ax    // ax is the accumulator
  | Apnd(s,a) -> tolist2 (a::ax) s;;

let sl = tolist2 [] s;;
(*
Note that tolist2 takes two arguments (curried): the first one is ax,
the accumulator, and the second one is the stack that's pattern
matched.

Theoretically, you don't have to use recursion, but with the pattern
matching style of programming it's often easier to use it.  Thus it is
really important to know when something is tail recursive, and to be
able to apply basic algorithmic analysis (T(n)=2T(n/2)+n can be
recursive, T(n-1)+1 should only be tail-recursive).  It is not
incorrect to say that you need to be an advanced computer science
student in order to get the most out of F#.

Is it possible to write the tolist function using a loop? Yes, but it
would look something like this:
*)
let tolist3 (M:'a stack) =
   let mutable L:'a list = []  // type annotation is needed here
   let mutable N = M
   while N <> Emp do   //  <> is "not equals"
      match N with
        | Apnd(s,x) ->
            L <- x::L
            N <- s
        | _ -> ()     // _ matches everything (use for default case)
   L;;  // I *think* this works
(*
Surely you agree this is bending over backwards just to use a loop.
tolist2 is just as efficient and easier to write.  Besides, why learn a
programming language at all if you're just going to use it like java.
Time to upgrade your programming skills.  You are not allowed to use
loops or if statements in the following exercises.  At least, you have
to write it without them first.

***EXERCISE C2 : Write a function to return the length (size) of a stack
***EXERCISE C3:  Write a function convert an F# list into a stack, without
reversing the elements.
*)
