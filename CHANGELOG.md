# Changelog

## Unreleased

### Bug fixes

* `daemon` and `standalone False` no longer terminate the process with
  SIGTERM as soon as it is launched.
* The pipe APIs no longer raise a "Broken pipe" `IOException` when the
  process exits or closes its stdin before reading all of the input. The
  rest of the input is not consumed and the exit code of the process
  determines the result, like in a shell pipeline.
* `pipeChunksEitherWith` in the internal module no longer fails with
  "Not reachable" when stdout is inherited using `inheritStdout`.
* `setSession` now replaces the session behavior set by an earlier
  `setSession`, e.g. `setSession InheritSession . setSession NewSession`
  inherits the session instead of creating a new one.
## 0.4.1 (Apr 2026)

* Rename quotedWord to shellWord

## 0.4.0 (Sep 2025)

* Remove buffering from the pipe chunked APIs.
* Add a `toBytesWith` function to modify process configuration

## 0.3.1 (Dec 2023)

* Allow streamly-0.10.0 and streamly-core-0.2.0
* Fix a bug in quote escaping in the Command module
* Add APIs in System.Process and System.Command module with ability to set
  process attributes.

## 0.3.0 (Apr 2023)

* Added a `Streamly.System.Command` module
* `toChunks` and `toBytes` now make the stdin available to the process being
  executed.
* Allow streamly 0.9.0
* Signature changes - removed the IsStream constraint, now all APIs use the
  monomorphic `Stream` type.

## 0.2.0.1 (Mar 2022)

* Fix the test suite.
* Allow streamly 0.8.2

## 0.2.0 (Dec 2021)

* The process in following APIs is now terminated with SIGTERM if an exception
  is thrown or if the process is garbage collected before it could terminate.
  - `toBytes`
  - `processChunks`
  - `processBytes`

## 0.1.0 (Jul 2021)

* Initial version.
