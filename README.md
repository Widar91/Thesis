# Thesis

## Introduction to Theoretical Computer Science

From [University of Edinburgh Course - Introduction to Theoretical Computer Science](http://www.inf.ed.ac.uk/teaching/courses/itcs/lectures.html). Slides [here](http://www.inf.ed.ac.uk/teaching/courses/itcs/itcs-slides.pdf)

### Computability Theory & Complexity

Day | Sources | Comments
--- | --- | ---
22 Oct | | Exploration and understanding of where and what to start with
23 Oct | [Sipser](http://neerci.ist.utl.pt/neerci_shelf/LEIC/2%20Ano/1%20Semestre/Teoria%20da%20Computacao/Bibliografia/Introduction%20To%20The%20Theory%20Of%20Computation%20-%20Sipser.pdf): Ch 0, 1, 2 | Automata and Languages: </br><ul><li>Regular Languages</li><li>Context Free Languages</li></ul>
Mon, Oct 26 | [Sipser](http://neerci.ist.utl.pt/neerci_shelf/LEIC/2%20Ano/1%20Semestre/Teoria%20da%20Computacao/Bibliografia/Introduction%20To%20The%20Theory%20Of%20Computation%20-%20Sipser.pdf): Ch 3 | Church-Turing Thesis: </br> <ul><li>Turing Machines</li><li>Definition of Algorithm</li></ul>
Tue, Oct 27 | [Sipser](http://neerci.ist.utl.pt/neerci_shelf/LEIC/2%20Ano/1%20Semestre/Teoria%20da%20Computacao/Bibliografia/Introduction%20To%20The%20Theory%20Of%20Computation%20-%20Sipser.pdf): Ch 4, 5 (beginning), [Turing Paper](https://people.cs.umass.edu/~immerman/cs601/TuringPaper1936.pdf), [Turing Paper Summary](http://swizec.com/blog/week-1-turings-on-computable-numbers/swizec/6375) | <ul><li>Halting Problem & Proof</li><li>Decidability</li><li>Reducibility</li></ul>
Thu, Oct 29 | [Sipser](http://neerci.ist.utl.pt/neerci_shelf/LEIC/2%20Ano/1%20Semestre/Teoria%20da%20Computacao/Bibliografia/Introduction%20To%20The%20Theory%20Of%20Computation%20-%20Sipser.pdf): Ch 5, 7 | <ul><li>Finishing up on Reducibility</li><li>P - NP Problems</li><li>Cook - Leving Theorem</li></ul>

Author | Reference
--- | --- 
Sipser | [Introduction to the Theory of Computation](http://neerci.ist.utl.pt/neerci_shelf/LEIC/2%20Ano/1%20Semestre/Teoria%20da%20Computacao/Bibliografia/Introduction%20To%20The%20Theory%20Of%20Computation%20-%20Sipser.pdf)
Turing Paper | [On computable numbers with an application to the entscheidungsproblem](https://people.cs.umass.edu/~immerman/cs601/TuringPaper1936.pdf)
Turing Paper Summary | http://swizec.com/blog/week-1-turings-on-computable-numbers/swizec/6375

### Lambda Calculus
Day | Sources | Comments
--- | --- | ---
Fri, Oct 30 | | Research of material and understanding of how to proceed
Tue, Nov 3 | [Pierce](http://port70.net/~nsz/articles/book/pierce_types_and_programming_languages_2002.pdf): Ch 1, 2, 3 | <ul><li>Introduction</li><li>Mathematical Preliminaries</li><li>Untyped Arithmetic Expressions</li></ul>
Thu, Nov 5 | [Pierce](http://port70.net/~nsz/articles/book/pierce_types_and_programming_languages_2002.pdf): Ch 4, 5 |
Fri, Nov 6 | [Pierce](http://port70.net/~nsz/articles/book/pierce_types_and_programming_languages_2002.pdf): Ch 5, Y-Combinator | Untyped Lambda Calculus
Mon, Nov 9 - Tue, Nov 24 | - | Visitors + Influence -> No work has been conducted
Wed, Nov 25 | [Pierce](http://port70.net/~nsz/articles/book/pierce_types_and_programming_languages_2002.pdf): Ch 5, 6, 7 | Untyped Lambda Calculus
Thu, Nov 26 | [Pierce](http://port70.net/~nsz/articles/book/pierce_types_and_programming_languages_2002.pdf): Ch 8, 9, 10 | Symply Typed Lambda Calculus
Fri, Nov 27 | [Pierce](http://port70.net/~nsz/articles/book/pierce_types_and_programming_languages_2002.pdf): Ch 11 | Extending the Symply Typed Lambda Calculus
Mon, Nov 30 - Fri, Dec 4th | Write Yourserf a Scheme in 48 Hrs | Implementation of the [project](https://github.com/Widar91/SchemeInterpreter), refreshing of advanced Haskell concepts
... | [Pierce](http://port70.net/~nsz/articles/book/pierce_types_and_programming_languages_2002.pdf): Ch 12 to 23 | Type Systems (until Universal Typing)

Author | Reference
--- | --- 
Pierce | [Types and Programming Languages](http://port70.net/~nsz/articles/book/pierce_types_and_programming_languages_2002.pdf)
Y-Combinator | [Understanding, at last, the Y Combinator - a programmer-friendly perspective](http://hisham.hm/2011/04/04/understanding-at-last-the-y-combinator-a-programmer-friendly-perspective/comments/)
Lambda Calculus Recursion | [Harvard CS-152 Lecture 8: Lambda calculus encodings and Recursion](http://www.seas.harvard.edu/courses/cs152/2016sp/lectures/lec08-encodings.pdf)
University of Edinburgh| [Introduction to Theoretical Computer Science Slides](http://www.inf.ed.ac.uk/teaching/courses/itcs/itcs-slides.pdf)
Harvard University | [Harvard - CS 152: Programming Languages](http://www.seas.harvard.edu/courses/cs152/2016sp/schedule.html)

## Semantics

* [Harvard - CS 152: Programming Languages](http://www.seas.harvard.edu/courses/cs152/2016sp/schedule.html) - Intro to Semantics
* [University of Edinburgh - Language Semantics and Implementation](http://www.inf.ed.ac.uk/teaching/courses/lsi/) - Operational Semantics
* [[University of Edinburgh - Formal Programming Language Semantics](http://www.inf.ed.ac.uk/teaching/courses/fpls/)
* [Concepts in Programming Languages, John C. Mitchell](https://books.google.nl/books?id=AUUgAwAAQBAJ&pg=PA384&lpg=PA384&dq=concepts+in+programming+languages+mitchell+download&source=bl&ots=CIP2lnBg_z&sig=If9xZsI7zSO_R4o2bBcVLGiydiQ&hl=en&sa=X&ved=0ahUKEwiN-sm1pubLAhXFGQ8KHYeFCLsQ6AEISzAI#v=onepage&q=concepts%20in%20programming%20languages%20mitchell%20download&f=false) - Ch 4.3, Intro to Denotational Semantics
* [Illinois University](http://fsl.cs.illinois.edu/images/6/60/CS422-Spring-2010-04.pdf) - Intro to Denotational Semantics
* [Denotational Semantics - a Methodology for Language Development, David A. Schmidt ](http://www.bcl.hamilton.ie/~barak/teach/F2008/NUIM/CS424/texts/ds.pdf)
* [Denotational design with type class morphisms (extended version)](http://conal.net/papers/type-class-morphisms/type-class-morphisms-long.pdf)

## Functional Reactive Programmming
* [Functional Reactive Animation](http://conal.net/papers/icfp97/icfp97.pdf)
* [Fair Reactive Programming](http://cs.mcgill.ca/~acave1/papers/fair-reactive.pdf)
* [Categorical Semantics for Functional Reactive Programming with Temporal Recursion and Corecursion](http://arxiv.org/pdf/1406.2062.pdf)
* [Ultrametric Semantics of Reactive Programs](https://www.mpi-sws.org/~neelk/frp-lics11.pdf)
* [Temporal Logic and FRP](http://www.slideshare.net/SergeiWinitzki/temporal-logic-and-functional-reactive-programming)
* [Functional Reactive Programming from First Principles](http://haskell.cs.yale.edu/wp-content/uploads/2011/02/frp-1st.pdf)
* [Practical principled FRP: forget the past, change the future, FRPNow!](http://dl.acm.org/citation.cfm?id=2784752)

## Free Monads
* [Reflection without Remorse](http://okmij.org/ftp/Haskell/zseq.pdf)
* [Control.Monad.Free.Reflectable](https://hackage.haskell.org/package/reflection-without-remorse-0.9.5/docs/Control-Monad-Free-Reflectable.html)
* [Free Monads Just Got Cheaper](http://blog.timsears.com/posts/Free_Monads_Just_Got_Cheaper.html)

## Process Algebra
* [Introduction to Process Algebra](http://www.few.vu.nl/~wanf/BOOKS/procalg.pdf)
* [A gentle introfuction to Process Algebra (not so gentle!)](https://www.pst.ifi.lmu.de/Lehre/fruhere-semester/sose-2013/formale-spezifikation-und-verifikation/intro-to-pa.pdf)
* [Trace-based Process Algebras for Real-Time Probabilistic Systems](http://www.prismmodelchecker.org/papers/stefanosthesis.pdf)

## Others
* [Iteratees](http://okmij.org/ftp/Haskell/Iteratee/describe.pdf)
* [The Essence of the Iterator Pattern](http://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf)
* [Cheap (But Functional) Threads](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.84.4430&rep=rep1&type=pdf)
* [Cont Monad](https://www.imperialviolet.org/2007/07/04/continuation-monads-for-state-machines.html)
* [Cont Monad 2](https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style)
* [Pipes](http://jozefg.bitbucket.org/posts/2015-06-01-pipes.html)
* [Temporal Logic and FRP](http://www.slideshare.net/SergeiWinitzki/temporal-logic-and-functional-reactive-programming)
* [Continuation-based relative-time FRP](http://www.paolocapriotti.com/blog/2012/06/04/continuation-based-relative-time-frp/)
* [Event Calculus Explained](https://www.doc.ic.ac.uk/~mpsha/ECExplained.pdf)
