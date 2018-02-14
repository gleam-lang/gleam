-module(gleam_native).
% Natively implemented functions used in the stdlib.

-export([identity/1]).

identity(X) -> X.
