use std::time::Instant;

use rand::{distributions::Uniform, Rng, RngCore, rngs::SmallRng, SeedableRng};

#[derive(Debug)]
struct Address {
    street: String,
    postal_code: String,
    city: String,
    country: String,
}

#[derive(Debug)]
struct Employee {
    first_name: String,
    last_name: String,
    address: Address,
    salary: u64,
}

struct RandyRandom {
    rng: Box<dyn RngCore>,
    pool: Vec<char>,
    dist: Uniform<usize>,
}

impl RandyRandom {
    fn new(rng: Box<dyn RngCore>) -> RandyRandom {
        let pool: Vec<_> =
            ('a'..'z')
                .chain('A'..'Z')
                .chain('0'..'9')
                .collect();

        let dist = Uniform::from(0..pool.len());
        RandyRandom { rng, pool, dist }
    }

    fn create_random_employee(&mut self) -> Employee {
        Employee {
            first_name: self.create_random_string_of_80_chars(),
            last_name: self.create_random_string_of_80_chars(),
            address: Address {
                street: self.create_random_string_of_80_chars(),
                postal_code: self.create_random_string_of_80_chars(),
                city: self.create_random_string_of_80_chars(),
                country: self.create_random_string_of_80_chars(),
            },
            salary: 1000,
        }
    }

    fn create_random_string_of_80_chars(&mut self) -> String {
        (0..80)
            .map(|_| self.pool[self.rng.sample(self.dist)])
            .collect()
    }
}

fn lookup_all_employees(
    number_of_all_employees: u64,
    r: &mut RandyRandom,
) -> impl Iterator<Item=Employee> + '_ {
    (0..number_of_all_employees).map(move |_| r.create_random_employee())
}

fn compute_average_income_of_all_employees(employees: impl Iterator<Item=Employee>) -> f64 {
    let (num_of_employees, sum_of_salaries) = employees
        .fold((0u64, 0u64), |(counter, sum), employee| {
            (counter + 1, sum + employee.salary)
        });
    (sum_of_salaries as f64) / (num_of_employees as f64)
}

pub fn benchmark() {
    println!("Benchmarking Randy Random");
    let nrs_of_employees = [1000u64, 10000, 100000, 1000000];
    let mut r = RandyRandom::new(Box::new(SmallRng::from_entropy()));
    for nr_of_employees in &nrs_of_employees {
        let start_time = Instant::now();
        let average =
            compute_average_income_of_all_employees(lookup_all_employees(*nr_of_employees, &mut r));
        let end_time = Instant::now();
        let duration = end_time.duration_since(start_time);
        println!(
            "n={} Average = {} Duration = {}ms",
            nr_of_employees,
            average,
            duration.as_millis()
        );
    }
    println!();
}

