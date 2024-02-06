


# This file defines any magic numbers or constants used in the package.

# The default number of chains to use when sampling from a Stan model.
# This is the default set by the `cmdstanr` package however we need to fix it
# as if they were to change it it would break our code as we create the number
# of initial values based on this number.
#
# Unfortunately the default value is in a method of an unexported object (only the constructor
# is exported) so there is no way for us to access it without digging through the
# internals of `cmdstanr`.
CMDSTAN_DEFAULT_CHAINS <- 4
