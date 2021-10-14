package tf.db

object DbCommon {

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

}
