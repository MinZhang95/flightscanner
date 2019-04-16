#' Insert rows into a table.
#' @description Insert rows into a table.
#' If the table has key, it will ignore the duplicated rows indexed by the key.
#' It assumes that the table has been created beforehand.
#'
#' @param conn A \code{\link[DBI:DBIConnection-class]{DBIConnection}} object,
#' as returned by \code{\link[DBI:dbConnect]{dbConnect()}}.
#' @param name Name of the table.
#' @param value A data frame of values.
#' The column names must be consistent with those in the target table in the database.
#' @param ... Other arguments used by individual methods.
#'
#' @import DBI
#' @export
#'
#' @examples
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' dbAppendTableNew(con, "iris", iris)
dbAppendTableNew <- function(conn, name, value, ...) {
  value <- value %>% ListUnpack(mutate = T)

  sum(sapply(1:NROW(value), function(i) {
    tryCatch(dbAppendTable(conn, name, value[i, , drop = F], ...), error = function(e) 0)
  }))
}


#' Connect to a SQLite database and create tables.
#' @description Connect to a SQLite database file. And check it the following tables exists:
#' price, itinerary, leg, segment, carrier, agent, and place. If not, create them.
#'
#' @param conn A \code{\link[RSQLite:SQLiteDriver-class]{SQLiteDriver}}
#' or \code{\link[RSQLite:SQLiteConnection-class]{SQLiteConnection}}.
#' @param dbname The path to the database file. Don't use two exceptions:
#' \itemize{
#'   \item{
#'   \code{""} will create a temporary on-disk database.
#'   The file will be deleted when the connection is closed.
#'   }
#'   \item{
#'   \code{":memory:"} or \code{"file::memory:"} will create a temporary in-memory database.
#'   }
#' }
#'
#' @return An object of class \code{\link[RSQLite:SQLiteConnection-class]{SQLiteConnection}}.
#' If can't connect to SQLite driver, return 1.
#' @export
#'
#' @examples
#' \dontrun{
#' dbCreateDB(dbname = "flight.db")
#' unlink("flight.db")
#' }
dbCreateDB <- function(conn = RSQLite::SQLite(), dbname = "flight.db") {
  if (inherits(conn, "SQLiteConnection")) {
    con <- conn
  } else if (inherits(conn, "SQLiteDriver") && dbCanConnect(conn)) {
    con <- dbConnect(conn, dbname = dbname)
  } else return(1)

  if (!dbExistsTable(con, "price")) {
    dbCreateTable(con, SQL("price"), c(SearchTime = "TEXT NOT NULL",
                                       OutboundLegId = "TEXT NOT NULL",
                                       InboundLegId = "TEXT NOT NULL",
                                       PricingOptions = "BLOB"))
  }

  if (!dbExistsTable(con, "itinerary")) {
    fields <- c(OutboundLegId = "TEXT NOT NULL",
                InboundLegId = "TEXT NOT NULL")
    columns <- paste(paste(dbQuoteIdentifier(con, names(fields)), fields), collapse = ",\n  ")
    query <- paste0("CREATE TABLE itinerary (\n  ", columns,
                    ",\n  PRIMARY KEY (`OutboundLegId`, `InboundLegId`)\n)")
    dbExecute(con, query)
  }

  if (!dbExistsTable(con, "leg")) {
    dbCreateTable(con, SQL("leg"), c(Id = "TEXT PRIMARY KEY NOT NULL",
                                     SegmentIds = "BLOB NOT NULL",
                                     OriginId = "INTEGER NOT NULL",
                                     DestinationId = "INTEGER NOT NULL",
                                     DepartureTime = "TEXT NOT NULL",
                                     ArrivalTime = "TEXT NOT NULL",
                                     Duration = "INTEGER NOT NULL",
                                     No.Stops = "INTEGER NOT NULL",
                                     Directionality = "TEXT NOT NULL",
                                     Stops = "BLOB"))
  }

  if (!dbExistsTable(con, "segment")) {
    dbCreateTable(con, SQL("segment"), c(Id = "TEXT PRIMARY KEY NOT NULL",
                                         OriginId = "INTEGER NOT NULL",
                                         DestinationId = "INTEGER NOT NULL",
                                         DepartureTime = "TEXT NOT NULL",
                                         ArrivalTime = "TEXT NOT NULL",
                                         Duration = "INTEGER NOT NULL",
                                         CarrierId = "INTEGER NOT NULL",
                                         OperatingCarrierId = "INTEGER NOT NULL",
                                         FlightNumber = "TEXT NOT NULL",
                                         Directionality = "TEXT NOT NULL"))
  }

  if (!dbExistsTable(con, "carrier")) {
    dbCreateTable(con, SQL("carrier"), c(Id = "INTEGER PRIMARY KEY NOT NULL",
                                         Code = "TEXT NOT NULL",
                                         Name = "TEXT NOT NULL",
                                         ImageURL = "TEXT"))
  }

  if (!dbExistsTable(con, "agent")) {
    dbCreateTable(con, SQL("agent"), c(Id = "INTEGER PRIMARY KEY NOT NULL",
                                       Name = "TEXT NOT NULL",
                                       Type = "TEXT NOT NULL",
                                       ImageURL = "TEXT"))
  }

  if (!dbExistsTable(con, "place")) {
    dbCreateTable(con, SQL("place"), c(Id = "INTEGER PRIMARY KEY NOT NULL",
                                       ParentId = "INTEGER",
                                       Code = "TEXT NOT NULL",
                                       Type = "TEXT NOT NULL",
                                       Name = "TEXT NOT NULL"))
  }

  con
}












