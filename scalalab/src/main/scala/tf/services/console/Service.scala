package tf.services.console

import java.util.UUID

trait Service[F[_], T, E, DTO] {
  def all: F[List[DTO]]

  def create(item: DTO): F[Either[E, DTO]]

  def update(id: UUID, item: DTO): F[Either[E, Boolean]]

  def find(id: UUID): F[Option[DTO]]

  def delete(id: UUID): F[Boolean]
}
