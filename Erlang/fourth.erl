-module(fourth).
-compile(export_all).

gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).

gcd2(A, B) ->
    Gcd = fun(X, 0, _) -> X;
             (X, Y, GcdFv) -> GcdFv(Y, X rem Y, GcdFv)
          end,
    Gcd(A, B, Gcd).