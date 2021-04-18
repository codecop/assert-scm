# assert-scm

Minimalist xUnit test framework for Scheme R5RS.

## Usage

`assert.scm` is a minimalist (Gambit) R5RS implementation of xUnit in Scheme style.
There are several assertions available, e.g. `(assert=)` for numbers, `(assert-string=)` and `(assert-raise)`.

To run the self-test:

    gsi test/assert-test.scm
