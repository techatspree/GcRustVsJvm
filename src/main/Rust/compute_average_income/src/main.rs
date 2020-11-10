mod randy_random;
mod standard_random;
mod small_random;

fn main() {
    standard_random::benchmark();
    randy_random::benchmark();
    small_random::benchmark();
}
