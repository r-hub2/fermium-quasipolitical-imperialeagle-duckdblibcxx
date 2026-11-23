#' Read a parquet file and its key-value metadata the way cancensus does
#' @param path Path to a parquet file.
#' @param allow_extensions Passed to duckdb::duckdb(), NULL as in cancensus 0.6.2, FALSE as in the fix.
read_parquet_file <- function(path, allow_extensions = NULL) {
  drv <- if (is.null(allow_extensions)) duckdb::duckdb(shared_home = FALSE)
         else duckdb::duckdb(shared_home = FALSE, allow_extensions = allow_extensions)
  con <- DBI::dbConnect(drv)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  source <- DBI::dbQuoteString(con, normalizePath(path, winslash = "/"))
  list(data = DBI::dbGetQuery(con, paste0("SELECT * FROM read_parquet(", source, ")")),
       metadata = DBI::dbGetQuery(con, paste0("SELECT decode(key) AS k, decode(value) AS v FROM parquet_kv_metadata(",
                                              source, ")")))
}
