# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

GcRustVsJvm is a comparative benchmarking project that measures garbage collection and memory allocation performance across three programming languages: Kotlin/JVM, Rust, and Haskell. The project accompanies blog articles published on the Akquinet blog.

The benchmark generates random Employee objects (with nested Address structures) and computes the average income, measuring execution time across dataset sizes of 1K, 10K, 100K, and 1M employees.

## Build Commands

### Run All Benchmarks
```bash
./gradlew computeAverageIncome
```

### Individual Language Benchmarks
```bash
./gradlew computeAverageIncomeKotlin    # Kotlin/JVM benchmark
./gradlew computeAverageIncomeRust      # Rust benchmark (release build)
./gradlew computeAverageIncomeHaskell   # Haskell benchmark (6 cores)
./gradlew profileAverageIncomeHaskell   # Haskell with profiling
```

### Direct Language Commands

**Rust** (from `src/main/Rust/compute_average_income/`):
```bash
cargo run --release
```

**Haskell** (from `src/main/Haskell/compute-average-income/`):
```bash
stack build
stack exec compute-average-income-exe
stack build --profile && stack exec --profile -- compute-average-income-profile-exe +RTS -s
```

## Architecture

### Language Implementations

| Language | Location | Build System | Key Files |
|----------|----------|--------------|-----------|
| Kotlin | `src/main/kotlin/` | Gradle | `ComputeAverageIncome.kt` |
| Rust | `src/main/Rust/compute_average_income/` | Cargo | `main.rs`, three variant modules |
| Haskell | `src/main/Haskell/compute-average-income/` | Stack | `ComputeAverageIncome.hs`, `ComputeAverageIncomeStrict.hs` |

### Design Variations

**Rust** has three RNG strategy variants (for performance comparison):
- `standard_random.rs` - Standard ThreadRng
- `randy_random.rs` - SmallRng with trait object wrapping
- `small_random.rs` - SmallRng seeded from thread_rng

**Haskell** has two evaluation strategies:
- `ComputeAverageIncome.hs` - Lazy evaluation (uses `foldr`)
- `ComputeAverageIncomeStrict.hs` - Strict evaluation (`{-# LANGUAGE Strict #-}`, uses `foldl'`)

### Build Orchestration

Gradle (7.0.2) serves as the primary orchestrator, invoking Cargo and Stack via Exec tasks for Rust and Haskell respectively. The Kotlin implementation compiles directly through Gradle's Kotlin plugin.

## Toolchain Requirements

- **Kotlin**: Kotlin 2.0.21, JVM 1.8+
- **Rust**: rustc 1.46.0+, Cargo
- **Haskell**: Stack with GHC 9.6.6 (resolver lts-22.43)

## Benchmark Results

Measurement data is stored in `src/measurements/measurements.numbers` (Apple Numbers format).