package tf.services

trait Service[F[_], T, E, DTO] {
  def all: F[List[T]]
  def create(item: DTO): F[Either[E, T]]
  def update(id:   Long, item: DTO): F[Either[E, Boolean]]
  def find(id:     Long): F[Option[T]]
  def delete(id:   Long): F[Boolean]
}
