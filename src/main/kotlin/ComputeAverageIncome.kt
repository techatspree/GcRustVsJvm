import kotlin.system.measureTimeMillis

data class Address(val street: String, val postalCode: String, val city: String, val country: String)

data class Employee(val firstName: String, val lastName: String, val address: Address, val salary: Long)

class EmployeeServices {

    companion object {
        private const val numberOfAllEmployees = 10000000L
        private val charPool: List<Char> = ('a'..'z') + ('A'..'Z') + ('0'..'9')

        private fun lookupAllEmployees(): Sequence<Employee> {
            return (1L..numberOfAllEmployees)
                .asSequence()
                .map { createRandomEmployee() }
        }

        private fun createRandomEmployee(): Employee =
            Employee(
                createRandomStringOf80Chars(), createRandomStringOf80Chars(), Address(
                    createRandomStringOf80Chars(),
                    createRandomStringOf80Chars(), createRandomStringOf80Chars(),
                    createRandomStringOf80Chars()
                ), 1000L
            )

        private fun createRandomStringOf80Chars() =
            (1..80)
                .map { kotlin.random.Random.nextInt(0, charPool.size) }
                .map(charPool::get)
                .joinToString("")


        fun computeAverageIncomeOfAllEmployees(): Double {
            val (nrOfEmployees, sumOfSalaries) = lookupAllEmployees()
                .fold(Pair(0L, 0.0),
                    { (counter, sum), employee ->
                        Pair(counter + 1, sum + employee.salary)
                    })
            return sumOfSalaries / nrOfEmployees
        }
    }
}

fun main() {
    val timeNeeded = measureTimeMillis {
        EmployeeServices.computeAverageIncomeOfAllEmployees()
        Runtime.getRuntime().gc()
    }
    println("timeNeeded = $timeNeeded")
}
