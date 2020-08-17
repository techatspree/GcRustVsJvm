use rand::Rng;

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
    salary: u32,
}


fn createRandomStringOf80Chars(char_pool: &Vec<char>) -> String {
    return (0..80)
        .map(|_| { char_pool[rand::thread_rng().gen_range(0, char_pool.len())] })
        .into_iter().collect();
}

fn create_random_employee(char_pool: &Vec<char>) -> Employee {
    return Employee {
        first_name: createRandomStringOf80Chars(char_pool),
        last_name: createRandomStringOf80Chars(char_pool),
        address: Address
        {
            street: createRandomStringOf80Chars(char_pool),
            postal_code: createRandomStringOf80Chars(char_pool),
            city: createRandomStringOf80Chars(char_pool),
            country: createRandomStringOf80Chars(char_pool),
        },
        salary: 1000,
    };
}

fn lookup_all_employees(number_of_all_employees: i64, char_pool: &Vec<char>) -> Vec<Employee> {
    return
        (0..number_of_all_employees)
            .map(|n| { return create_random_employee(char_pool); })
            .collect();
}

fn main() {
    let v = (1u64..11).collect::<Vec<_>>();
    let char_pool = ('a'..'z').collect::<Vec<_>>();
    let number_of_all_employees: i64 = 1000000;

    println!("random string: {:#?}", createRandomStringOf80Chars(&char_pool));
    let employee = create_random_employee(&char_pool);
    dbg!(employee);
    //println!("{:#?}", employee)
    let empl = lookup_all_employees(5, &char_pool);
    dbg!(empl);
}
