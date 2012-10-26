-----
title: A neat trick to distinguish integers
date: 2012-10-25
-----

Something neat from Hector Pasten's talk at the CYMS seminar:
suppose that we want to give a formula for the statement
“$x \neq y$” where $x$ and $y$ are integers, but we want to do so in a
*positive existential* system, i.e., using only the defined arithmetical
operations $(+,\times,=)$ and the existential quantifier $\exists$. 
In particular, we can’t use negation or the universal quantifier
$\forall$. 

At first glance, this might seem difficult – how do we express the
concept of “not equal” without using “not”? However, there is a very
nice trick. First, observe that
$$
x \neq y \iff x-y-1 \ge 0 \vee x-y-1 \le 0.
$$
We’ve changed the problem from defining $x \neq y$ to the problem of
defining “$x \ge 0$” (in a positive existential way).
Recall Lagrange’s Four Square Theorem:

### Theorem (Lagrange’s Four Square Theorem)
Every nonnegative integer $n \in \mathbb{N}$ 
may be written as a sum of four squares:
$$
n = x_1^2 + x_2^2 + x_3^2 + x_4^2 \quad x_i \in \mathbb{N}.
$$

So, we can write:
$$
x  \ge 0 \iff \exists x_1,\ldots,x_4 ( x = x_1^2 + x_2^2 + x_3^2 + x_4^2)
$$
which, in turn, gives us our desired positive existential formula for 
$x \neq y$.
