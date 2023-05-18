# CS-340-Haskell-Programming-Paradigms-and-Patterns-

This repository contains most of the labs from my CS 340 class. Although, I did lose some files amongst my quest to obtain my degree, so I will upload what was salvaged. 

I will provide a general description of the language below.

Haskell is based on the functional programming paradigm, which emphasizes writing programs using pure functions and avoiding mutable state and side effects.
Functions in Haskell are treated as first-class citizens, meaning they can be passed as arguments, returned as results, and stored in data structures.
Immutability is a fundamental aspect of Haskell, where variables cannot be modified once assigned a value. Instead, new values are created through function application.
This makes it challenging to write haskell programs since you have to think of how to approach problems recursively.

**Key Features and Benefits of Haskell:**

**Strong Static Typing:** Haskell has a strong type system that is checked at compile-time, helping to catch type-related errors before running the program. The type inference system allows many type annotations to be automatically deduced by the compiler.

**Purely Functional:** Haskell enforces immutability and purity by default. Pure functions eliminate side effects, making code easier to reason about, test, and parallelize. Referential transparency ensures that a function with the same inputs always produces the same outputs, aiding in debugging and optimization.

**Laziness (Non-Strict Evaluation):** Haskell uses lazy evaluation, meaning that expressions are evaluated on an as-needed basis. This allows for potentially infinite data structures and improves efficiency by avoiding unnecessary computations.

**Pattern Matching:** Pattern matching is a powerful feature in Haskell that allows you to deconstruct data structures and bind variables based on their shape. It enables concise and expressive code, especially when working with algebraic data types.

**Type Classes and Polymorphism:** Haskell employs type classes to enable polymorphism. Type classes define behavior for a set of types, allowing functions to operate on values of different types as long as they satisfy the required behavior. This promotes code reuse and abstraction.

**Concurrency and Parallelism:** Haskell provides abstractions for concurrent and parallel programming, making it easier to write scalable and efficient programs. Technologies like Software Transactional Memory (STM) and lightweight threads aid in managing concurrent computations.

**Large Ecosystem and Community:** Haskell has a thriving ecosystem with a wide range of libraries and tools available through the central package repository called Hackage. The Haskell community is known for its helpfulness and the strong emphasis on code quality.

**Strong Mathematical Foundation:** Haskell draws inspiration from lambda calculus and incorporates concepts from category theory, making it an excellent language for exploring and implementing mathematical ideas and algorithms.


## Overview and explanation of labs


**Data Types and HOFs (MP3)**


**Part 1: Binary Tree**

In the first section of the assignment, I learned about the concept of binary trees, a type of data structure that has a root and two subtrees, referred to as the left and right subtrees. I practiced building a variety of functions that could interact with this data type.

The functions I implemented included `treeRepeat`, `treeNats`, `treeVal`, `treeToList`, `treeFlip`, `treeFromList`, and `treeIterate`. Through implementing these functions, I was able to delve deeper into the core concepts of higher-order functions in functional programming. 

I grasped the principles of function creation and application, and explored recursion in a practical context. Furthermore, I learned to effectively manipulate my own data types and understood how to implement class instances for Functor and Applicative classes. 

Implementing these higher-order functions allowed me to experience first-hand how to conduct operations like applying a function to all values in a tree or combining two trees. This helped me understand the power and flexibility of functional programming.

**Part 2: Poker Hand Analysis**

In the second section, I developed my own type definitions to represent playing cards and wrote functions to determine which poker hand a list of five cards would create, as well as how to calculate aggregate hand statistics.

The process of creating a type to represent a deck of cards and defining various poker hands, such as Straight and FullHouse, improved my ability to create and interact with custom data types. I learned how to work with lists of custom data types and apply logic to identify different poker hands through implementing the `hand` function. 

I also created the `computeStats` function, which required me to analyze multiple hands and return a statistical summary. This not only solidified my knowledge of higher order functions but also allowed me to see their application in analyzing a list of hands and returning aggregate statistics.

In conclusion, this assignment helped me understand the core concepts of functional programming, such as the creation and use of custom data types, the definition and use of higher order functions, and the application of these principles to solve practical problems.


**Monadic Parsing (MP4)**
This assignment involves the implementation of a monadic parser in Haskell for a subset of the C language. In this context, a monadic parser is a program that analyzes input conforming to a specific grammar, which is represented using monads— a functional programming concept that deals with sequencing of computations.

the task of this assignment is to parse valid (in terms of syntax) C function definitions using a monadic parser. The subset of C being parsed includes function definitions that have types, function names, parameters, local variables, assignment operations, and a return statement. The only permitted types are `int` and `char`.

The function definitions being parsed should follow a specific format, and must adhere to certain rules, such as all local variable declarations occurring before any assignments and the return statement appearing only once and as the last statement in the function.

The parser to be implemented, `funcDef`, should return a tuple with:
1. The name of the function,
2. A list of the names of the parameters of the function,
3. A list of the names of the local variables declared within the function,
4. The variable name or integer value returned by the function (as a string), or an empty string if there's no return statement.

If an invalid function is parsed, the parser should return `Nothing`.

Essentially, we built and sequenced smaller parsers for different parts of a function definition to ultimately create the function parser. Some of the suggested smaller parsers include `typeName` (for recognizing "int" or "char"), `paramList` (for parsing a parameter list), `assignment` (for parsing a single assignment statement), and `varDecls` (for parsing variable declarations of a given type).

What we learned from this assignment includes:
- Understanding and implementation of monadic parsing in Haskell,
- Basics of the C language grammar,
- Building complex parsers by sequencing smaller parsers,
- Parsing and manipulation of string inputs,
- Error handling in parsers.

Monads are a kind of abstract data type used in functional programming to handle side effects. They originate from the mathematical field of category theory and have been popularized mainly by their use in Haskell programming, but they are used in various forms in many other functional languages too.

In simple terms, monads can be thought of as a design pattern to handle certain kinds of problems in functional programming, like computations that include operations like input/output, state changes, exceptions, computations with failure, etc...


**Monadic Parsing (MP5)**

**Part 1: Best First Search - Knight's Tour**

In the first part of this assignment, I dealt with the fascinating problem of the Knight's Tour. This involves finding a sequence of moves for a knight on a chessboard such that it visits every square exactly once. The challenge here was not just about defining the steps or moves but also about formulating a heuristic for choosing the optimal move at each step.

I learned to devise a function that represents a step along the knight's tour and ascertain valid moves from each position. A key concept I got familiar with is Warnsdorff's rule, an heuristic approach that suggests prioritizing moves which minimize the number of possible subsequent moves. By implementing this, I could build a search algorithm using Best First Search that can efficiently solve the Knight's Tour problem.

Understanding and implementing the Best First Search algorithm was a critical learning outcome of this assignment. It reinforced my comprehension of heuristic-driven search algorithms, their efficiency, and their practical use in solving problems that involve finding an optimal path in a discrete system.

**Part 2: Adversarial Search - Reversi**

In the second section of the assignment, I tackled the adversarial search problem through the board game of Reversi (or Othello). The goal here was to build a function that could determine the optimal move in a given board state, taking into account potential subsequent moves by the opponent.

The task was to model the game, identify valid moves, and develop a scoring heuristic that could guide the decision-making process of the AI player. I learned to create a function that could effectively compute the game's state after each move and determine the optimal decision for the current player, using the minimax algorithm.

This experience enhanced my understanding of adversarial search strategies, specifically the minimax algorithm, and gave me the opportunity to practically implement it in a game scenario. 

Additionally, this assignment also highlighted the importance of selecting suitable data structures for the problem at hand, like single/multi-dimensional arrays for the board, and exposed me to libraries that could assist with random value generation, list manipulation, and managing complex data structures.

Overall, this machine problem enhanced my problem-solving skills, deepened my understanding of search strategies, and reinforced the practical use of algorithmic concepts like heuristic search and adversarial search in real-world problems.











<img width="962" alt="image" src="https://user-images.githubusercontent.com/78235399/187097845-98061b1c-80fa-4fd4-bb72-158f57e350ff.png">


Me crying for a few points below (not literally)(relax)

Machine Problem 1 (Report)

<img width="876" alt="image" src="https://user-images.githubusercontent.com/78235399/187097937-9cc081bf-98fa-4e61-b17b-17d4e7901282.png">
<img width="838" alt="image" src="https://user-images.githubusercontent.com/78235399/187097966-c7970d54-ba4b-4030-8d4a-4c70f499f226.png">

Machine Problem 2 (Report)

<img width="847" alt="image" src="https://user-images.githubusercontent.com/78235399/187098138-94c2e62b-c275-4d3f-9e33-8f4b2ee0e38f.png">


Machine Problem 3 (Report)

<img width="592" alt="image" src="https://user-images.githubusercontent.com/78235399/187098048-d65b159b-996e-46ca-b0ec-f7be3d73193a.png">


Machine Problem 4 (Report)

<img width="828" alt="image" src="https://user-images.githubusercontent.com/78235399/187098099-eae8738b-4b4b-4ab4-9c46-f430da006083.png">


Note: Machine Problem 5 had no report because that was a final. Although I got 100%
