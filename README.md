Formally verified FFT and DFT implementation
============================================

This package contains a DFT and FFT implementation, with inverse,
written in the haskell SBV DSL.
Both implementations have been verified using several known properties
when dealing with transformations and inverse functions.
SBV provides an easy to use SMT solver interface and has been used with the z3 SMT solver.

Implementation
--------------
There are three main parts of the implementation: constraints regarding primtive unit roots, DFT with inverse and FFT with inverse.
The discrete fourier transform accepts some primtive root of unity, of degree n, and transforms an input vector l of size n.
A primtive root of unity w, where w is a complex number, is defined as follows:

 * w^n = 1
 * w^k != 1, for all integer k, 0 < k < n

These properties are modelled as constraints acting upon on a free complex variable.

The DFT and FFT implementations have been generically written over all possible input lists, with the FFT implementation being limited to input lists of size power of two.


Properties
----------
It's currently work in progress to verify larger input lists, but the following properties have been proved:

* exists x. primitive unit root x, forall l. l list of size n ==> DFT^-1(DFT(l, x), x) == l
  with l consisting of 1 to 7 elements.
* forall x. primitive unit root x, forall l. l list of size n ==> DFT^-1(DFT(l, x), x) == l, FFT^-1(FFT(l, x), x) == l, FFT^-1(DFT(l, x), x) == l
  with l consisting of 1, 2 or 4 elements.

Unfortunantely the current implementation does not allow larger lists when proving properties with the FFT implementation, hence there is no real assurance in the implementation at the moment. Quickcheck support is somewhat lacking as well since modelling irrational complex numbers turned out to be problematic.
