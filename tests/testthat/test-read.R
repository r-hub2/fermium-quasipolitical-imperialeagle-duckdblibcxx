write_parquet <- function() {
  path <- tempfile(fileext = ".parquet")
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE, allow_extensions = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, paste0("COPY (SELECT 1 AS x) TO ", DBI::dbQuoteString(con, path),
                             " (FORMAT parquet, KV_METADATA {note: 'hello'})"))
  path
}

record <- function(code) {
  messages <- list()
  value <- withCallingHandlers(code, message = function(m) {
    messages[[length(messages) + 1]] <<- m
    invokeRestart("muffleMessage")
  })
  cat("\n  classes:", if (length(messages)) paste(class(messages[[1]]), collapse = ", ") else "none", "\n")
  for (m in messages) cat("  message:", conditionMessage(m), "\n")
  list(value = value, messages = messages)
}

test_that("build info", {
  cat("\nduckdb", as.character(packageVersion("duckdb")), "| platform:", R.version$platform,
      "| CXX:", system2(file.path(R.home("bin"), "R"), c("CMD", "config", "CXX"), stdout = TRUE), "\n")
  succeed()
})

test_that("cancensus 0.6.2 way: default allow_extensions", {
  path <- write_parquet()
  for (i in 1:2) {
    cat("\n  default, call", i)
    r <- record(read_parquet_file(path))
    expect_equal(r$value$data$x, 1)
    # expected to FAIL on libc++ Linux builds, which shows the message in the check log
    expect_length(r$messages, 0)
  }
})

test_that("fixed way: allow_extensions = FALSE shows no message and still reads parquet", {
  path <- write_parquet()
  for (i in 1:2) {
    cat("\n  allow_extensions = FALSE, call", i)
    r <- record(read_parquet_file(path, allow_extensions = FALSE))
    expect_length(r$messages, 0)
    expect_equal(r$value$data$x, 1)
    expect_equal(r$value$metadata$v[r$value$metadata$k == "note"], "hello")
  }
})
