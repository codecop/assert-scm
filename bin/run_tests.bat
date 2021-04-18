@rem
@rem Unit test framework for Scheme R5RS, Gambit extension.
@rem Copyright (c) 2015, Peter Kofler, https://www.code-cop.org/
@rem BSD licensed.
@rem
@rem run all test files:
@setlocal EnableDelayedExpansion
@color 02
@for /F "usebackq delims=?" %%a in (`dir /s /b *-test.scm`) do @call run_test %%a
@endlocal
