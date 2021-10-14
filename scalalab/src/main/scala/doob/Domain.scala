package doob

import java.time.{LocalDate, Year}
import java.util.UUID

object Domain {
  final case class Author(id: UUID, name: String, birthday: LocalDate)

  final case class Book(id: UUID, authorId: UUID, title: String, year: Year)

  final case class BookWithAuthor(id: UUID, author: Author, title: String, year: Year) {
    override def toString: String = s"$title ($year) by ${author.name}"
  }
}
