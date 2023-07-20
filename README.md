# CS-340-Haskell-Programming-Paradigms-and-Patterns-

This repository contains most of the labs from my CS 340 class. Although, I did lose some files amongst my quest to obtain my degree, so I will upload what was salvaged. 


## Table of Contents
1. [About Haskell](#about-haskell)
2. [Overview and Explanation of Labs](#overview-and-explanation-of-labs)
    - [Data Types and HOFs (MP3)](#data-types-and-hofs-mp3)
        - [Part 1: Binary Tree](#part-1-binary-tree)
        - [Part 2: Poker Hand Analysis](#part-2-poker-hand-analysis)
    - [Monadic Parsing (MP4)](#monadic-parsing-mp4)
    - [Monadic Parsing (MP5)](#monadic-parsing-mp5)
        - [Part 1: Best First Search - Knight's Tour](#part-1-best-first-search---knights-tour)
        - [Part 2: Adversarial Search - Reversi](#part-2-adversarial-search---reversi)
        - [Reversi Explanation and Demo](#reversi-explanation-and-demo)
3. [Machine Problem Reports](#machine-problem-reports)



## About Haskell

Haskell is based on the functional programming paradigm, which emphasizes writing programs using pure functions and avoiding mutable state and side effects.
Functions in Haskell are treated as first-class citizens, meaning they can be passed as arguments, returned as results, and stored in data structures.
Immutability is a fundamental aspect of Haskell, where variables cannot be modified once assigned a value. Instead, new values are created through function application. This makes it challenging to write Haskell programs since you have to think of how to approach problems recursively.

**Key Features and Benefits of Haskell:**

**Strong Static Typing:** Haskell has a strong type system that is checked at compile-time, helping to catch type-related errors before running the program. The type inference system allows many type annotations to be automatically deduced by the compiler.

**Purely Functional:** Haskell enforces immutability and purity by default. Pure functions eliminate side effects, making code easier to reason about, test, and parallelize. Referential transparency ensures that a function with the same inputs always produces the same outputs, aiding in debugging and optimization.

**Laziness (Non-Strict Evaluation):** Haskell uses lazy evaluation, meaning that expressions are evaluated on an as-needed basis. This allows for potentially infinite data structures and improves efficiency by avoiding unnecessary computations.

**Pattern Matching:** Pattern matching is a powerful feature in Haskell that allows you to deconstruct data structures and bind variables based on their shape. It enables concise and expressive code, especially when working with algebraic data types.

**Type Classes and Polymorphism:** Haskell employs type classes to enable polymorphism. Type classes define behavior for a set of types, allowing functions to operate on values of different types as long as they satisfy the required behavior. This promotes code reuse and abstraction.

**Concurrency and Parallelism:** Haskell provides abstractions for concurrent and parallel programming, making it easier to write scalable and efficient programs. Technologies like Software Transactional Memory (STM) and lightweight threads aid in managing concurrent computations.

**Large Ecosystem and Community:** Haskell has a thriving ecosystem with a wide range of libraries and tools available through the central package repository called Hackage. The Haskell community is known for its helpfulness and the strong emphasis on code quality.

**Strong Mathematical Foundation:** Haskell draws inspiration from lambda calculus and incorporates concepts from category theory, making it an excellent language for exploring and implementing mathematical ideas and algorithms.


## Overview and Explanation of Labs


**Data Types and HOFs (MP3)**


#### Part 1: Binary Tree

In the first section of the assignment, I learned about the concept of binary trees, a type of data structure that has a root and two subtrees, referred to as the left and right subtrees. I practiced building a variety of functions that could interact with this data type.

The functions I implemented included `treeRepeat`, `treeNats`, `treeVal`, `treeToList`, `treeFlip`, `treeFromList`, and `treeIterate`. Through implementing these functions, I was able to delve deeper into the core concepts of higher-order functions in functional programming. 

I grasped the principles of function creation and application, and explored recursion in a practical context. Furthermore, I learned to effectively manipulate my own data types and understood how to implement class instances for Functor and Applicative classes. 

Implementing these higher-order functions allowed me to experience first-hand how to conduct operations like applying a function to all values in a tree or combining two trees. This helped me understand the power and flexibility of functional programming.

#### Part 2: Poker Hand Analysis

In the second section, I developed my own type definitions to represent playing cards and wrote functions to determine which poker hand a list of five cards would create, as well as how to calculate aggregate hand statistics.

The process of creating a type to represent a deck of cards and defining various poker hands, such as Straight and FullHouse, improved my ability to create and interact with custom data types. I learned how to work with lists of custom data types and apply logic to identify different poker hands through implementing the `hand` function. 

I also created the `computeStats` function, which required me to analyze multiple hands and return a statistical summary. This not only solidified my knowledge of higher order functions but also allowed me to see their application in analyzing a list of hands and returning aggregate statistics.

In conclusion, this assignment helped me understand the core concepts of functional programming, such as the creation and use of custom data types, the definition and use of higher order functions, and the application of these principles to solve practical problems.


#### Monadic Parsing (MP4)
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

#### Part 1: Best First Search - Knight's Tour

In the first part of this assignment, I dealt with the fascinating problem of the Knight's Tour. This involves finding a sequence of moves for a knight on a chessboard such that it visits every square exactly once. The challenge here was not just about defining the steps or moves but also about formulating a heuristic for choosing the optimal move at each step.

I learned to devise a function that represents a step along the knight's tour and ascertain valid moves from each position. A key concept I got familiar with is Warnsdorff's rule, a heuristic approach that suggests prioritizing moves that minimize the number of possible subsequent moves. By implementing this, I could build a search algorithm using Best First Search that can efficiently solve the Knight's Tour problem.

Understanding and implementing the Best First Search algorithm was a critical learning outcome of this assignment. It reinforced my comprehension of heuristic-driven search algorithms, their efficiency, and their practical use in solving problems that involve finding an optimal path in a discrete system.

#### Part 2: Adversarial Search - Reversi

In the second section of the assignment, I tackled the adversarial search problem through the board game of Reversi (or Othello). The goal here was to build a function that could determine the optimal move in a given board state, taking into account potential subsequent moves by the opponent.

The task was to model the game, identify valid moves, and develop a scoring heuristic that could guide the decision-making process of the AI player. I learned to create a function that could effectively compute the game's state after each move and determine the optimal decision for the current player, using the minimax algorithm.

This experience enhanced my understanding of adversarial search strategies, specifically the minimax algorithm, and gave me the opportunity to practically implement it in a game scenario. 

Additionally, this assignment also highlighted the importance of selecting suitable data structures for the problem at hand, like single/multi-dimensional arrays for the board, and exposed me to libraries that could assist with random value generation, list manipulation, and managing complex data structures.

Overall, this machine problem enhanced my problem-solving skills, deepened my understanding of search strategies, and reinforced the practical use of algorithmic concepts like heuristic search and adversarial search in real-world problems.

## Reversi Explanation and Demo

Reversi, also known as Othello, is a strategic board game played on an 8x8 grid by two players, typically one using black pieces and the other using white. Each player's turn involves placing a disc of their color on the board, in a position that 'outflanks' (vertically or diagonally) one or more of the opponent's discs. The outflanked discs are then flipped over to the player's color. For example, below, the white player placed the white disk on the far right-hand side. This move allowed the white player to flip the three flanked black disks to their own color. 

<p align="center">
  <img src="https://daroolz.com/wp-content/uploads/2022/12/Untitled.png?ezimgfmt=ng:webp/ngcb1" alt="Image description">
</p>


The goal of the game is to end with the majority of the discs displaying your color when the board is filled up, or when there are no valid moves left for either player. The player with the majority of their color discs on the board wins, while the other player loses.

Now that you got an idea of how the game is played, let's move on to the AI implementation for this specific game design.

<p align="center">
  <img src="https://media.tenor.com/FZwj52f8oKYAAAAC/letsflippinggo-flipping.gif" alt="Image description">
</p>


**Implementation:**

For this Reversi AI implementation, I've built an AI player for the board game using a mobility heuristic. The AI Mobility heuristic evaluates the difference in legal moves between the AI and its opponent. The black player, also known as the user, aims to maximize their possible moves, or amount of points for greater game control, this ultimately depends on the way the user wants to play. Meanwhile, the AI (white player) uses the Mobility heuristic to maximize its own moves while minimizing the opponent's moves, thereby restricting their choices and maneuverability. This heuristic assesses game control by evaluating the difference in legal moves between the AI and the player.

The Minimax algorithm guides the AI's decision-making, simulating all possible moves and their outcomes under the assumption of optimal opponent play.  Efficiency in this process is heightened by the depth-limited search technique, which reduces the number of evaluations in the game tree to improve performance.

Let's dive into the details of the minimax algorithm by examining an in-game example below. Here, we have a game tree representing the current state of the board (outlined in gold), where the white player is about to make a move, and its five potential actions are circled in green. You can also think of this like a tree, the root node is outlined in gold and the children are circled in green. (Note: the five actions depict the new board states, and the move played is represented by the capital 'W' circled in blue).

**Image depicting five actions from a current state**
<p align="center">
  <img width="491" alt="white_moves_colored" src="https://github.com/g-tapia/CS-340_Programming-Paradigms-and-Patterns/assets/78235399/c0f75b63-937a-4e19-81af-1f7e1f7715ba">
</p>

How does the AI decide which move to take? This is where the Minimax algorithm steps in and evaluates each action, considering optimal play from both sides. In this case, the algorithm is determining which deed will produce the most amount of valid moves.

**Below is a sample visualization demonstrating the concept of valid moves, given a particular board state:**

<p align="center">
<img width="350" alt="image" src="https://github.com/g-tapia/CS-340_Programming-Paradigms-and-Patterns/assets/78235399/fbff0ffe-86cc-470e-ad0a-e4e4482f4fb7">
</p>

The decision-making process is subject to a constraint known as the "pruning level." In this case, it's set to 1. This limitation implies that the AI will only consider the immediate outcomes of its current possible moves, not delving into the subsequent layers of potential responses to these moves. This is similar to playing against someone in real life who doesn't consider any future moves you might make.

On the other hand, when the pruning level is set to a value greater than one, the AI starts to consider its opponent's future moves. Take a look at the image below, it's derived from the previous image **"Image depicting five actions from a current state"**, but this time, the pruning level is set to two. Thus, you can notice that from the five actions, there are other subsequent actions circled in pink. Meaning that if the AI took action one circled in green, then the black player can make set of different moves for each green action. Thus, the game tree has grown by a depth of two.

<p align="center">
<img width="491" alt="white_moves_colored" src="https://github.com/g-tapia/CS-340_Programming-Paradigms-and-Patterns/assets/78235399/287e129e-ac28-4a90-a5bb-dcbe4fc41a46">
</p>

This might be a lot to consider, so let's examine a simpler case instead. Below, the max player (which represents the AI) is at the root. It can take two actions from its current state. However, before doing so, it will consider the potential reactions of its opponent min (the user or another AI). In other words, the AI is forward-thinking. It anticipates one move ahead and assumes that the opponent will aim to minimize the number of moves the AI can make, thus picking the action that results in the fewest moves for the AI. After determining which moves the user is most likely to make, the AI then decides which action will allow it to maximize its own moves.

<p align="center">
  <img src="https://www.baeldung.com/wp-content/uploads/2017/07/minimax.png">
</p>

Overall, we have built the intuition of how the AI thinks ahead, and now, you can imagine that the further ahead the AI calculates, the more costly it can be since the tree will grow bigger. 

## Demo
### User vs AI
Now that you have an understanding of how the underlying implementation works, it is time to move on to the demo. In this part of the demo, we'll examine how our AI interacts with a human opponent, at pruning level 3.

**Pruning Level 3:** At this stage, the AI delves even further into potential future outcomes, reflecting an even more advanced level of strategic thinking and game planning. I stopped the video short when I got further into the game. The reason for this is that the AI became slower due to the more actions it had to consider. I did not want to wait too long. 

https://github.com/g-tapia/CS-340_Programming-Paradigms-and-Patterns/assets/78235399/39152a85-e2c3-4de2-9aec-9627685d5c0c

### AI vs AI
Now, let's consider an all-AI match. This scenario provides a unique opportunity to witness the interaction between two AI opponents with the same decision-making capacities. Here, we'll observe the changes in gameplay as we vary the pruning levels from 1 to 3.

**Pruning Level 1:** Here, we'll observe how the AI reacts when it only considers the immediate consequences of its current possible moves, without anticipating any future moves the user might make. You can notice that this game goes by fast since both of the AI are not considering future moves.


https://github.com/g-tapia/CS-340_Programming-Paradigms-and-Patterns/assets/78235399/7d11f795-f630-477d-a692-ae0f96df6200



**Pruning Level 2:** With a deeper level of pruning, the AI begins to contemplate the possible responses to its current actions, allowing for more strategic play. In this case, both are almost tied.


https://github.com/g-tapia/CS-340_Programming-Paradigms-and-Patterns/assets/78235399/500b2a08-4588-4f61-b824-bdabfeb616ec


**Pruning Level 3:** At this stage, the AI delves even further into potential future outcomes, reflecting an even more advanced level of strategic thinking and game planning. Though, this game took way longer than the previous games. I had to cut it short since I would have been waiting a long time due to the additional computations both AI have to make.

https://github.com/g-tapia/CS-340_Programming-Paradigms-and-Patterns/assets/78235399/7957aaff-ea34-4516-934d-c727ee316d39



## Reversi (Othello) Heuristics

Reversi, also known as Othello, is a strategy game where two players compete to finish the game with more of their colored pieces on the board than their opponent. In order to implement an AI opponent for Reversi, we employ heuristics to help evaluate board positions and make optimal moves. Here, we present four primary heuristics as discussed in this [research paper](https://courses.cs.washington.edu/courses/cse573/04au/Project/mini1/RUSSIA/Final_Paper.pdf).

#### Disc Count
The Disc Count heuristic is one of the simplest. It calculates the difference between the number of the player's discs and the opponent's discs. This heuristic is especially beneficial during the end game when the goal is to maximize the number of one's own discs on the board.

#### Mobility
Mobility is a key factor in Reversi. The Mobility heuristic evaluates the number of legal moves a player can make. A player with more legal moves is generally in a superior position since they have more options to control the game. The heuristic is calculated as the difference between the number of legal moves for the player and the opponent.

#### Corner Occupancy
Corner squares are significant in Reversi due to their stability -- once occupied, they cannot be outflanked. The Corner Occupancy heuristic calculates the difference in the number of corners occupied by the player and the opponent. Holding more corners often translates into an advantage.

#### Stability
Stability refers to discs that can't be flipped for the remainder of the game. While this heuristic can be complex to calculate, it is extremely useful because stable discs are guaranteed points and enhance the player's control of the board.

Each of these heuristics provides a unique lens to evaluate a given board configuration. For a sophisticated AI player, these heuristics can be weighted differently depending on the stage of the game (opening, midgame, endgame) to craft a more nuanced and effective evaluation function.

---

For more details on the implementation of these heuristics and their effectiveness, refer to the original [research paper](https://courses.cs.washington.edu/courses/cse573/04au/Project/mini1/RUSSIA/Final_Paper.pdf). Stay tuned for more updates on this project.




## Machine Problem Reports


<img width="962" alt="image" src="https://user-images.githubusercontent.com/78235399/187097845-98061b1c-80fa-4fd4-bb72-158f57e350ff.png">


Me crying for a few points below (not literally)(relax)

Machine Problem 1 (Report)

<img width="876" alt="image" src="https://user-images.githubusercontent.com/78235399/187097937-9cc081bf-98fa-4e61-b17b-17d4e7901282.png">
<img width="838" alt="image" src="https://user-images.githubusercontent.com/78235399/187097966-c7970d54-ba4b-4030-8d4a-4c70f499f226.png">

Machine Problem 3 (Report)

<img width="592" alt="image" src="https://user-images.githubusercontent.com/78235399/187098048-d65b159b-996e-46ca-b0ec-f7be3d73193a.png">


Machine Problem 4 (Report)

<img width="828" alt="image" src="https://user-images.githubusercontent.com/78235399/187098099-eae8738b-4b4b-4ab4-9c46-f430da006083.png">


Note: Machine Problem 5 had no report because that was a final. Although I got 100%
