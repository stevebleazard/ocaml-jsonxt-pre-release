# Jsonxt test suite

The Jsonxt test suite covers standards compliance, extension support and
read-write-read tests.

The tests can be run using the dune command

```
dune build @runtests
```

## Compliance tests
Jsonxt supports multiple levels of compliance with the RFC 8259
standard. The compliance tests validate that each compliance level
(Strict, Basic, Extended, Yojson.Safe etc) correctly parses a number
of test files.  The validation process checks that each module
correctly parse or returns an error for each file.

Run compliance only checks with

```
dune build @compliance_tests
```

## Validation tests
Validation checks ensure that the parser are correctly processing
various JSON expressions.  The tests perform standard parses
and comparing with expected result using `sexp`.  In addition,
read-write-read test are performed to ensure writers work
correctly.  Again, `sexp` is used to validate the result.

Run validation only checks with

```
dune build @validation_tests
```

## JSONTestSuite tests
The suite set of tests validates the parses against the freely available
[JSONTestSuite](https://github.com/nst/JSONTestSuite). To run the
tests first clone [JSONTestSuite](https://github.com/nst/JSONTestSuite),
then, assuming the current directory is the same directory as this README.md,

```
dune build
JXTESTER=<path-to-jxtester.exe>
JSONTESTSUITE=<path-to-JSONTestSuite-directory>
$JXTESTER suite std $JSONTESTSUITE/test_parsing/[iny]_*
$JXTESTER suite stream $JSONTESTSUITE/test_parsing/[iny]_*
$JXTESTER suite monad $JSONTESTSUITE/test_parsing/[iny]_*
```

The results are reported as follows:
- pass: The JSON was handled correctly as defined by the test suite
- fail: The parser either succeeded when it should have failed or vica-versa
- OKpass: The JSON was parsed but the suite defines this case as indeterminate
- OKfail: The parser failed to parse the JSON but the suite defines this case as indeterminate

There should be no fails, OKpass and OKfail are both fine as the result is implentation
specific
