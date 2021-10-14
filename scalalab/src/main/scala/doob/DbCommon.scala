package doob

import java.util.UUID

object DbCommon {

  val edit: String =
    """DROP TABLE employees;""".stripMargin

  val createTableEmployeesSql: String =
    """CREATE TABLE employees (
      |  id UUID PRIMARY KEY,
      |  birthday VARCHAR(100) NOT NULL,
      |  firstName VARCHAR(100) NOT NULL,
      |  lastName VARCHAR(100) NOT NULL,
      |  salary VARCHAR(100) NOT NULL,
      |  position VARCHAR(20) NOT NULL,
      |  is_archived BOOLEAN NOT NULL DEFAULT FALSE
      |  );""".stripMargin

  val populateDataSql: String =
    s"""
       |INSERT INTO employees (id, birthday, firstName, lastName, salary, position) VALUES
       |  ('1', '1990-08-14', 'Din', 'Don', '2000 USD', 'manager'),
       |  ('2', '1990-03-12', 'Ivan', 'Urgant', '1500 USD', 'middle');
       |""".stripMargin

}
