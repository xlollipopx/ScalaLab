package tf.util

import tf.domain.employee.Employee
import tf.dto.employee.EmployeeDto

object ModelMapper {
  def employeeDomainToDto(employee: Employee) = {
    EmployeeDto(
      employee.birthday.toString,
      employee.firstName.value,
      employee.lastName.value,
      employee.salary.amount.toString(),
      employee.salary.currency.toString,
      employee.position.value,
      employee.is_archived
    )
  }
}
