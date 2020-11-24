# bayesnec 1.0.1

- If link functions are not specified in bnec, then the default link function is used; previous versions of bayesnec used the identity link.

- An additional family has been added betabinomial2 for over dispersered binomial data.

- The package supports using link functions for generalized modelling, which appears to be more stable and is also in line with more typical generalised modelling approaches.

- There are multiple options for model weights calculation from the loo package. The default is "pseudobma BB".

- There is now a compare_posterior function that also includes a bootstrapping procedure. This can be used to compare model fits across different datasets, or even different model sets for the same dataset (ie nec v ecx models). Please see the vignette for examples of usage.

- There is a vignette detailing the models available in bayesnec. Note that not all models are suitable for all families, and also depending if link functions are used.

- A new check_chains function has been added to allow chain plotting in base R and that works more smoothly with plotting chains for multiple fits for bayesmanec objects.
