# run-benchmarks

Use this skill when the user wants to run performance benchmarks, compare languages, or test the compute-average-income implementations.

## Instructions

1. Set JAVA_HOME to Java 23 for Gradle compatibility:
   ```bash
   export JAVA_HOME=/Users/tnfink/Library/Java/JavaVirtualMachines/openjdk-23.0.1/Contents/Home
   ```

2. Run all benchmarks with:
   ```bash
   ./gradlew computeAverageIncome
   ```

   Or run individual language benchmarks:
   - Kotlin: `./gradlew computeAverageIncomeKotlin`
   - Rust: `./gradlew computeAverageIncomeRust`
   - Haskell: `./gradlew computeAverageIncomeHaskell`

3. For Haskell profiling:
   ```bash
   ./gradlew profileAverageIncomeHaskell
   ```

## Notes
- Gradle 9.2.1 uses JDK 21 toolchain (auto-provisioned via foojay)
- Haskell uses GHC 9.10.3 (LTS-24.25)
- Rust uses cargo with --release flag
