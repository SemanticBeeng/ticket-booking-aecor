package ru.pavkin.booking.booking.entity

import cats.data.NonEmptyList
import ru.pavkin.booking.booking.service.TicketReservationService._
import ru.pavkin.booking.common.models.{BookingKey, Money, Seat, Ticket}

case class ConcertState(prices: Map[Seat, Money],
                          availableSeats: Set[Seat],
                          bookedSeats: Map[BookingKey, NonEmptyList[Seat]]) {

    def book(
      bookingId: BookingKey,
      seats: NonEmptyList[Seat]
    ): Either[ReservationFailure, (ConcertState, NonEmptyList[Ticket])] =
      if (bookedSeats.contains(bookingId)) Left(SeatsAlreadyBooked)
      else if (!seats.forall(availableSeats)) Left(SeatsAlreadyBooked)
      else if (!seats.forall(prices.contains)) Left(UnknownSeats)
      else
        Right(
          copy(
            availableSeats = availableSeats.diff(seats.toList.toSet),
            bookedSeats = bookedSeats.updated(bookingId, seats)
          ) -> seats.map(s => Ticket(s, prices(s)))
        )

    def release(bookingId: BookingKey): Either[ReleaseFailure, ConcertState] =
      bookedSeats.get(bookingId) match {
        case Some(booked) =>
          Right(
            copy(
              availableSeats = availableSeats ++ booked.toList.toSet,
              bookedSeats = bookedSeats - bookingId
            )
          )
        case None => Left(UnknownBooking)
      }
  }
