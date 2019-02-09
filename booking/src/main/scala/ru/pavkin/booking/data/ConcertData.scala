package ru.pavkin.booking.data

import ru.pavkin.booking.common.models.{ConcertId, Money, Seat}
import ru.pavkin.booking.booking.entity.ConcertState

object ConcertData {

  val frontRows: Range.Inclusive = 1 to 10
  val backRows: Range.Inclusive = 11 to 30

  val rowSize = 50
  val seatNumbers: Range.Inclusive = 1 to rowSize

  val frontRowPrices: Map[Seat, Money] =
    frontRows
      .flatMap(r => seatNumbers.map(r -> _))
      .map((Seat.seat _).tupled)
      .map(_ -> Money(500))
      .toMap

  val backRowPrices: Map[Seat, Money] =
    backRows
      .flatMap(r => seatNumbers.map(r -> _))
      .map((Seat.seat _).tupled)
      .map(_ -> Money(300))
      .toMap

  val prices: Map[Seat, Money] = frontRowPrices ++ backRowPrices

  val concertData = Map(
    ConcertId("concertA") -> ConcertState(prices, prices.keys.toSet, Map.empty),
    ConcertId("concertB") -> ConcertState(prices, prices.keys.toSet, Map.empty)
  )

}
