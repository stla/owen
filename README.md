# owen

Evaluation of the Owen T-function and the Owen Q-function.

The Owen T-function is evaluated by summation of a series.

There are two implementations of the Owen Q-function.
The first one simply evaluates the function by numerical integration.
The second one, better, follows the procedure given by Owen for an integer number of
degrees of freedom. The results are theoretically exact when the number of
degrees of freedom is even.
When odd, the procedure resorts to the Owen T-function.

Also evaluates the cumulative distribution function of the noncentral Student
distribution with integer number of degrees of freedom.
The results are theoretically exact when the number of degrees of freedom is even.
