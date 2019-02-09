package ru.pavkin.booking.booking.service

import java.time.temporal.ChronoUnit
import java.time.{Duration, Instant}
import java.util.concurrent.TimeUnit

import cats.Monad
import cats.data.NonEmptyList
import cats.effect.{Clock, Sync}
import cats.effect.concurrent.Ref
import cats.implicits._
import ru.pavkin.booking.booking.entity.ConcertState
import ru.pavkin.booking.booking.service.TicketReservationService._
import ru.pavkin.booking.common.models._

class StubConfirmationService[F[_]: Monad](clock: Clock[F],
                                           state: Ref[F, Map[ConcertId, ConcertState]])
    extends TicketReservationService[F] {

  val expireAfter: Duration = Duration.of(6, ChronoUnit.HOURS)

  def reserve(bookingId: BookingKey,
           concertId: ConcertId,
           seats: NonEmptyList[Seat]): F[Either[ReservationFailure, Reservation]] =
    clock
      .realTime(TimeUnit.MILLISECONDS)
      .map(Instant.ofEpochMilli)
      .flatMap(
        now =>
          state.modify[Either[ReservationFailure, Reservation]](
            concerts =>
              concerts.get(concertId) match {
                case None => concerts -> Left(UnknownSeats)
                case Some(concertState) =>
                  concertState
                    .book(bookingId, seats)
                    .fold(e => concerts -> Left(e), {
                      case (c, t) =>
                        concerts.updated(concertId, c) -> Right(
                          Reservation(t, Some(now.plus(expireAfter)))
                        )
                    })

            }
        )
      )

  def release(bookingId: BookingKey): F[Either[ReleaseFailure, Unit]] =
    state.modify[Either[ReleaseFailure, Unit]](
      concerts =>
        Either
          .fromOption(concerts.find(_._2.bookedSeats.contains(bookingId)), UnknownBooking)
          .flatMap {
            case (concertId, concertState) =>
              concertState.release(bookingId).map(concertId -> _)
          } match {
          case Left(value)                  => concerts -> Left(value)
          case Right((concertId, newState)) => concerts.updated(concertId, newState) -> Right(())
      }
    )
}

object StubConfirmationService {

  def apply[F[_]: Sync](clock: Clock[F],
                        initial: Map[ConcertId, ConcertState]): F[StubConfirmationService[F]] =
    Ref.of(initial).map(new StubConfirmationService(clock, _))

}
