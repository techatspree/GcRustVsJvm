import kotlin.system.measureTimeMillis

data class Address(val street: String, val postalCode: String, val city: String, val country: String)

data class Employee(val firstName: String, val lastName: String, val address: Address, val salary: Long)

class EmployeeServices {

    companion object {
        private const val numberOfAllEmployees = 1000000L
        private val charPool: List<Char> = ('a'..'z') + ('A'..'Z') + ('0'..'9')

        public fun lookupAllEmployees(): Sequence<Employee> {
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


        fun computeAverageIncomeOfAllEmployees(lookupEmployees : () -> Sequence<Employee>): Double {
            val (nrOfEmployees, sumOfSalaries) = lookupEmployees()
                .fold(Pair(0L, 0L),
                    { (counter, sum), employee ->
                        Pair(counter + 1, sum + employee.salary)
                    })
            return sumOfSalaries.toDouble() / nrOfEmployees.toDouble()
        }
    }
}

fun main() {
    val timeNeeded = measureTimeMillis {
        EmployeeServices.computeAverageIncomeOfAllEmployees(EmployeeServices.Companion::lookupAllEmployees)
        Runtime.getRuntime().gc()
    }
    println("timeNeeded = $timeNeeded ms")
}
