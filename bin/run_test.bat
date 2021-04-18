@rem run an idividual test file:
@echo --------------------------------------------------------------------------------
@echo %1
@gsi -:s %~f1
@if errorlevel 1 @color 04 && @echo FAILED: !errorlevel!
@if not errorlevel 1 @echo OK
