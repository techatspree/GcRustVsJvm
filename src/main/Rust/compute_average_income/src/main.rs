mod randy_random;
mod standard_random;

fn main() {
    standard_random::benchmark();
    randy_random::benchmark();
}
