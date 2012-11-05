-----
title: Elliptic Curves, Lattices, and the Upper Half-Plane
date:  2012-10-31
-----

This post is a rough sketch at writing down my understanding of the
correspondence found on page 36 of Silverman’s *Advanced Topics in the
Arithmetic of Elliptic Curves* (ATAEC), following material there and in
*The Arithmetic of Elliptic Curves* (AEC):
$\DeclareMathOperator{\Ell}{Ell}$

$$
\begin{array}
  \mathrm{Ell}_\mathbb{C} & \longleftrightarrow &
  \mathcal{L}/\mathbb{C}^\times & \longleftrightarrow &
  \Gamma(1)\backslash \mathfrak{h} & \longleftrightarrow &
  \mathbb{C} \\
  \left\{ E_\Lambda \right\} & &
  \left\{ \Lambda_\tau \right\} & &
  \tau & &
  j(\tau)
\end{array}
$$

We go from right to left.

## Elliptic curves and lattices

Here, we write
$\Ell_\mathbb{C}$ for the set of elliptic curves defined
over $\mathbb{C}$, up to $\mathbb{C}$-isomorphism, and
$\mathcal{L}$ for the set of lattices in $\mathbb{C}$.

Fix a lattice $\Lambda$. Consider the [Weierstrass $\wp$-function][wp-fn] of the lattice,
defined as
$$
\wp(z;\Lambda) = \frac{1}{z^2} + \sum_{\lambda \in \Lambda}^{*}
\left( \frac{1}{(z+\lambda)^2} - \frac{1}{\lambda^2} \right).
$$
This is evidently $\Lambda$-periodic, so we can also consider it as a
function on $\mathbb{C}/\Lambda$ instead of just on $\mathbb{C}$, 
and it is a fact that it satisfies
the differential equation
$$
(\wp'(z))^2 = 4(\wp(z))^3 + g_2\wp(z) + g_3,
$$
where
$$
g_2 = 60G_4 = 60\sum_{\lambda \in \Lambda}^{*} \lambda^{-4} \\
g_3 = 140G_6 = 140\sum_{\lambda \in \Lambda}^{*} \lambda^{-6}
$$
are the *modular invariants* of the lattice, and the asterisk in the sum
denotes the omission of the zero term. We want to identify the elliptic
curve $E : y^2 = 4x^3 + g_2x + g_3$ with the torus $\mathbb{C}/\Lambda$.
To do this we need two facts: first, that that curve is actually an elliptic
curve, and second, that the map
$ z \mapsto (\wp(z),\wp'(z)) $ is an isomorphism of Riemann surfaces
that preserves the group structure. These facts are provided in
AEC.VI.3.6.

Now, we have associated to each lattice an elliptic curve over
$\mathbb{C}$. Suppose we have two lattices, $\Lambda_1$ and $\Lambda_2$.
Then their associated elliptic curves $E_1$ and $E_2$ are isomorphic
over $\mathbb{C}$ if and only if the lattices $\Lambda_1$ and $\Lambda_2$ are
homothetic, i.e., $\alpha\Lambda_1 \subseteq \Lambda_2$ for some $\alpha
\in \mathbb{C}^\times$. This is AEC.VI.4.1.1.
Finally, we have

### Theorem (Uniformization Theorem)

Let $A, B \in \mathbb{C}$ be complex numbers with $4A^3 - 27B^2 \neq 0$.
Then there exists a lattice $\Lambda \subset \mathbb{C}$ with
$g_2(\Lambda) = A$, $g_3(\Lambda) = B$.

Thus, for every elliptic curve $E$ over $\mathbb{C}$, there is a lattice
so that $E \cong \mathbb{C}/\Lambda$. Putting everything together, we
see that there is a bijection between $\Ell_\mathbb{C}$, elliptic curves
over $\mathbb{C}$, and $\mathcal{L}/\mathbb{C}^\times$, the set of
lattices up to homothety.

## Lattices and the upper half-plane

We now wish to describe the correspondence between
$\mathcal{L}/\mathbb{C}^\times$ and the upper half-plane
$\mathfrak{h}$. 
Pick a lattice $\Lambda$, and some basis $\Lambda = \omega_1
\mathbb{Z} \oplus \omega_2 \mathbb{Z}$. We can choose the basis to be
*oriented*, i.e., so that $\tau = \omega_1 / \omega_2$ has positive imaginary
part. Now since the lattices are only determined up to homothety, we may
normalize the basis, multiplying by $\frac{1}{\omega_2}$ to get
$\Lambda = \mathbb{Z} \oplus \tau \mathbb{Z}$.

### Fact (ATAEC.I.1.2):

If $\psi_1, \psi_2$ and $\omega_1, \omega_2$ are two bases for the same
lattice $\Lambda$, then they differ by an element of $SL_2(\mathbb{Z})$.
Also, for $\tau_1, \tau_2 \in \mathfrak{h}$, we have that the associated
lattices $\Lambda_{\tau_1}, \Lambda_{\tau_2}$ are homothetic if
and only if $\tau_1 = \gamma\tau_2$ for some $\gamma \in \Gamma(1) = 
PSL_2(\mathbb{Z})$. (Here we use the usual
action by Möbius transformations).

From this fact and the preceding paragraph, we see that there is a bijection
between $\mathcal{L}/\mathbb{C}^\times$ and $\Gamma(1)\backslash
\mathfrak{h}$, giving the second bijection.

The last bijection is given by the [$j$-invariant][jinv],
$j(\tau) = 1728g_2^3/\Delta$, where $\Delta = g_2^3 - 27g_3^2$. The fact
that this is a bijection can be seen in e.g., Koblitz’s *Modular Forms
and Elliptic Curves*, III.2.11.

[wp-fn]: http://en.wikipedia.org/wiki/Weierstrass's_elliptic_functions
[jinv]: http://en.wikipedia.org/wiki/J-invariant

