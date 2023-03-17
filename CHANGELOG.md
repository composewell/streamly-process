# Changelog

## 0.3.0 (Mar 2023)

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
