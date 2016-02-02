---
title: Adventues in physics software: Minuit
date: 2016-02-02
math: true
---

If you ask a mathematician to give you a method that allows you to optimize an
arbitrary differentiable scalar function $f(\vec{x})$, they might point you to
a [variety](https://en.wikipedia.org/wiki/Broyden%E2%80%93Fletcher%E2%80%93Goldfarb%E2%80%93Shanno_algorithm)
[of](https://en.wikipedia.org/wiki/Stochastic_gradient_descent)
[algorithms](https://en.wikipedia.org/wiki/Davidon%E2%80%93Fletcher%E2%80%93Powell_formula)
that have been developed over the last decades. Or they might explain to you
that this is a very difficult problem in general, still a subject of active
research and that no algorithm can give you a satisfactory result for all
problems.

If you ask a particle physicist the same thing, they could tell you that the debate
has basically been settled around 1975 and that the answer is *Minuit*.

[Minuit](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.158.9157&rep=rep1&type=pdf)
is a highly robust library for optimizing (and analyzing the shape of) differentiable
scalar functions.
Basically, you provide it with $f$, which you've defined as a function in your code,
some initial values for $\vec{x}$, possibly some boundaries as well, and it will
generally do a pretty good job of finding the minimum of your function while
regularly printing out some status information.

The original version of Minuit is a Fortran program written many many years ago at CERN by Frank James.
It uses the
[Davidon-Fletcher-Powell](https://en.wikipedia.org/wiki/Davidon%E2%80%93Fletcher%E2%80%93Powell_formula)
algorithm under the hood (for the MIGRAD routine).
It makes use of the gradient of the objective function either by calculating it using the
[finite difference](https://en.wikipedia.org/wiki/Finite_difference_method)
method, or by using a user-provided gradient. There's also a more recent translation of Minuit into C++ called
[Minuit2](https://project-mathlibs.web.cern.ch/project-mathlibs/sw/Minuit2/html/index.html).

Although you might be put off by the dusty impression that the library makes
(the assumption that it's steered by the use of punched cards runs pretty deep in the codebase),
you might want to grant it a second look: It's actually a neat piece of numerical software.
Great care has been taken to make sure that it's optimization is numerically stable and that 
it won't prematurely claim to find a function minimum.

In fact, it's so good that particle physicists have used it pretty much exclusively since its inception.
Take any statistical result published by experimental particle physicists, and it's pretty safe to
assume that Minuit has been involved at some point of the analysis.
In fact, it's been in use for so long that most people use it like a black-box without spending a lot of time thinking about how it works.

![](/img/bill.jpg)

To make matters worse, Minuit is often hidden behind one (or more) layers of statistics analysis frameworks
that might hard-code important optimization parameters.
So sometimes, researchers might hit problems that would ideally be solved by adjusting one of the Minuit parameters,
but they might instead look for faults in their statistical model or in the way they use their statistics framework.
(Or they might simply accept the fact that their fit doesn't work)

As an example, one possible *faux pas* that might occur is using Minuit with a function that returns a floating point number with *single* precision.
(Why would you want to do this? For example, to evaluate a model quickly on your cheap consumer GPU, which only supports single precision)

When this is done naively, Minuit probably won't be able to optimize the function well at all, and might have serious problems to converge on a result.
The problem is that Minuit assumes a certain step size $h$ for its finite difference method.
$h$ can't be too large, or the gradient calculation will be too imprecise, and it can't be too small, or you will basically divide a small number by another small number and suffer numerical imprecision.
This means that Minuit has to choose an optimal value, and by default this value is optimized for double precision floating point numbers.
When the objective function returns single precision results, the additional rounding-off errors will cause Minuit to perform terribly.[^1]

[^1]: Nowadays, it would probably be best to use [automatic differentiation](https://en.wikipedia.org/wiki/Automatic_differentiation), an alternative numeric differentiation technique that makes use of the internals of the expression that needs to be evaluated.
      Using automatic differentiation, derivatives can be computed faster and to a higher precision than with the finite difference method.
      In the case of a scalar function, reverse-mode automatic differentiation is particularly effective.

In this case, as with many other possible problems, Minuit assumes that the user knows what they are doing (or that they've at least read the manual).
In fact, this problem is described in detail in the [Minuit manual](http://seal.web.cern.ch/seal/documents/minuit/mnusersguide.pdf) (C++ version):

> MINUIT is entirely based on C++ `double` precision. The actual floating point precision
> of `double` precision (32–bit or 64–bit) is platform dependent and can even vary
> on the same platform, depending on whether a floating point number is read from
> memory a CPU register.
> The argument of the user’s implementation of `FCNBase::operator()` is therefore a
> `std:vector<double>`. MINUIT expects that the calculations inside FCN will be performed
> approximately to the same accuracy.
> The accuracy MINUIT expects is called machine precision (`MnMachinePrecision`, see
> 4.5) and can be printed on demand using `std::cout`. If the user fools MINUIT by
> making internal FCN computations in single precision, MINUIT will interpret roundoff
> noise as significant and will usually either fail to find a minimum, or give incorrect
> values for the parameter errors.
> It is therefore recommended to make sure that all computations in FCN, as well as all
> methods and functions called by FCN, are done in double precision. If for some reason
> the computations cannot be done to a precision comparable with that expected by
> MINUIT , the user must inform MINUIT of this situation with setting a different
> machine precision via the `MnMachinePrecision::setPrecision(double)` method.
> With reduced precision, the user may find that certain features sensitive to first and
> second differences (HESSE, MINOS, CONTOURS) do not work properly, in which case the
> calculations must be performed in higher precision

When using advanced numerical tools like Minuit, it's important to have some understanding of what's going on underneath,
otherwise easily avoidable problems like this one might become unavoidable obstacles.
This also highlights the need for writing precise and extensive documentation whenever you write advanced software.

Interested in taking Minuit for a spin?
The easiest way to try out Minuit for yourself is probably to install the very nice [iminuit](https://github.com/iminuit/iminuit/)
library, which allows you to call Minuit from Python:

```bash
$ pip install --user iminuit
```
(This will also install Minuit itself)

It can be used like this:
```python
from iminuit import Minuit

def f(x, y):
    return (1 - x)**2 + 100 * (y - x**2)**2

m = Minuit(f)
m.migrad()
print(m.values) # {'y': 0.9912808347030864, 'x': 0.9956629805442004}
```
Here, we're finding the minimum of the famous [Rosenbrock function](https://en.wikipedia.org/wiki/Rosenbrock_function),
which for this case is roughly at $(1, 1)$.

