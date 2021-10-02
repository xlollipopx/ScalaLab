package tf.service

trait Service[F[_], T, E] {
  def all: F[Map[Long, T]]
  def create(
    birthday:  String,
    firstName: String,
    lastName:  String,
    salary:    String,
    currency:  String,
    position:  String
  ): F[Either[E, T]]
  def update(item: T):    F[Either[E, Boolean]]
  def find(id:     Long): F[Option[T]]
  def delete(id:   Long): F[Boolean]
}
