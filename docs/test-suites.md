# Testing

During the development if an issue occurs, we try to program the type system
to reject code which could lead to similar issues in the future. The second
defense line is to write quick check properties that check against the
assumptions we have at the moment.

As the test suite grows it is necessary to make a separation between the
tests which we run during the development cycle and those tests that should
be run on the continuous integration. We call it **smoke suite**,
the second one we can omit the 'smoke' prefix, or it can be called the **large suite**.

## Smoke test suite

The number and the runtime of the test cases should be kept at minimum
in the smoke test suite, but those tests should test as many requirements
as possible.

## Larger test suite

Test cases marked with `Large` tag in the test suite run only in the
the large test suite, which should run on the CI. This test
suite uses different parameters for the HSpec test runner, such as
running more test cases, and run quickcheck test with larger size
parameter. Also includes the test cases with the `Large` tag.

## Future test suites

In the future, integration, stress, and UI based tests could be written
which should be run before releases.
