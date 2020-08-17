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


fn create_random_string_of_80_chars(char_pool: &Vec<char>) -> String {
    return (0..80)
        .map(|_| { char_pool[rand::thread_rng().gen_range(0, char_pool.len())] })
        .into_iter().collect();
}

fn create_random_employee(char_pool: &Vec<char>) -> Employee {
    return Employee {
        first_name: create_random_string_of_80_chars(char_pool),
        last_name: create_random_string_of_80_chars(char_pool),
        address: Address
        {
            street: create_random_string_of_80_chars(char_pool),
            postal_code: create_random_string_of_80_chars(char_pool),
            city: create_random_string_of_80_chars(char_pool),
            country: create_random_string_of_80_chars(char_pool),
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
    let char_pool = ('a'..'z').collect::<Vec<_>>();

    println!("random string: {:#?}", create_random_string_of_80_chars(&char_pool));
    let employee = create_random_employee(&char_pool);
    dbg!(employee);
    //println!("{:#?}", employee)
    let employees = lookup_all_employees(5, &char_pool);
    dbg!(employees);
}
