# What's this?

This code contains examples used in these blog articles:
* [Yes, Rust has garbage collection, and a Fast One](https://blog.akquinet.de/2020/10/09/yes-rust-has-garbage-collection-and-a-fast-one/)
* [Kotlin/JVM, Rust and Randy Random](https://blog.akquinet.de/2020/11/22/kotlin-jvm-rust-and-randy-random/)  
* [Haskell is faster than Rust! Wait a Sec!](https://blog.akquinet.de/2021/01/03/haskell-is-faster-than-rust-wait-a-sec/)

# Kotlin
The Kotlin example is built and run using Gradle.

    ./gradlew computeAverageIncome

# Rust
You need at least the version 1.46.0 of rustc 
to build this example. 

To build and run the Rust application 
you have to move to its project directory:
    
    cd src/main/rust/compute_average_income

Running the slow development Rust version:

    cargo run

Running the fast Rust version:

    cargo run --release

# Haskell

You need an installation of [Stack](https://www.haskellstack.org).

To build and run the Haskell benchmark:

    ./gradlew computeAverageIncomeHaskell

# Run all benchmarks

First you have to install everything needed for the individual benchmarks. Then you can start all at once:

    ./gradlew computeAverageIncome
