# Functional Log Analyzer (Haskell)

This project implements a log analysis system using the Functional Programming paradigm.
It is written in Haskell and supports both batch processing and streaming processing of log files.

The project was developed as part of the Functional Programming course (TSI) and demonstrates
practical usage of pure functions, immutability, higher-order functions, and lazy evaluation.

## Project Features
- Pure functional core implemented in Haskell
- Batch log processing for small and medium log files
- Streaming log processing for very large files (memory-efficient)
- Text output and JSON report generation
- Automated tests using Hspec
- Clear separation between pure logic and I/O
- Docker support (optional)

## Project Structure
.
├── Main.hs
├── LogAnalyzer.hs
├── Streaming.hs
├── LogAnalyzerSpec.hs
├── StreamingSpec.hs
├── fp-log-analyzer.cabal
├── Dockerfile
└── README.md

## Requirements
- GHC (Glasgow Haskell Compiler)
- Cabal
- (Optional) Docker

## Build Project
cabal build

## Run – Batch Mode
cabal run analyzer -- --file log.txt

## Run – Streaming Mode
cabal run analyzer -- --file log_big.txt --stream

Streaming mode processes the file line by line and does not load the entire file into memory.
This allows efficient processing of very large log files (hundreds of megabytes).

## JSON Output
To generate a JSON report:
cabal run analyzer -- --file log.txt --json --out report.json

For streaming mode:
cabal run analyzer -- --file log_big.txt --stream --json --out report_stream.json

## Tests
Run all automated tests:
cabal test

The test suite verifies:
- Correct parsing of log entries
- Correct aggregation logic
- Correct behavior of batch and streaming modes
- Proper handling of invalid input and errors

## Functional Programming Concepts Demonstrated
- Pure Functions – all core logic is side-effect free
- Immutability – data is never modified in place
- Higher-Order Functions – extensive use of map, filter, fold
- Function Composition – analysis pipelines built from small functions
- Lazy Evaluation – enables streaming processing of large files
- Separation of Pure and Impure Code – I/O isolated in Main.hs

## Author
Student: Basuns Artjoms
Course: Functional Programming
Institution: TSI
Year: 2025

## License
This project is created for educational purposes.
