---
name: run-benchmarks
description: "Run GC and memory allocation benchmarks comparing Kotlin/JVM, Rust, GraalVM, and Haskell compute-average-income implementations via Gradle. Use when running performance benchmarks, comparing language execution times, profiling Haskell memory usage, or testing compute-average-income implementations across dataset sizes (1K to 1M employees)."
---

# run-benchmarks

## Workflow

1. **Set JAVA_HOME** to a Java 23+ installation for Gradle compatibility:
   ```bash
   export JAVA_HOME=$(/usr/libexec/java_home -v 23 2>/dev/null || echo "$JAVA_HOME")
   ```

2. **Run benchmarks** using Gradle as the orchestrator:

   - All languages: `./gradlew computeAverageIncome`
   - Kotlin/JVM only: `./gradlew computeAverageIncomeKotlin`
   - Rust only: `./gradlew computeAverageIncomeRust`
   - Haskell only: `./gradlew computeAverageIncomeHaskell`
   - GraalVM only: `./gradlew computeAverageIncomeGraalVM` (requires `GRAALVM_HOME`)

3. **Profile Haskell** memory and GC behavior:
   ```bash
   ./gradlew profileAverageIncomeHaskell
   ```

## Environment Requirements

- **Gradle 9.2.1**: Uses JDK 21 toolchain, auto-provisioned via foojay
- **GraalVM**: Set `GRAALVM_HOME` environment variable before running GraalVM benchmarks
- **Haskell**: GHC 9.10.3 (LTS-24.25) via Stack
- **Rust**: Compiled with `cargo --release`
