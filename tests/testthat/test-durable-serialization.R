test_that("durable serialized fields are CSV-safe and retain legacy compatibility", {
  value <- list(
    text = "comma, quote \" and newline\nare preserved",
    matrix = matrix(c(1, 0.25, 0.25, 1), nrow = 2L),
    nested = list(enabled = TRUE, values = c(1, 2, 3))
  )
  encoded <- tradesimr:::.serialize_field(value)

  expect_match(encoded, "^hex:[0-9a-f]+$")
  expect_false(grepl("[\r\n\",]", encoded))
  expect_equal(tradesimr:::.unserialize_field(encoded), value)

  legacy <- rawToChar(serialize(value, NULL, ascii = TRUE))
  expect_equal(tradesimr:::.unserialize_field(legacy), value)
})

test_that("exchange configuration round-trips through a CSV-safe durable field", {
  exchange <- sim_exchange_new(list(
    cash = 1000,
    user_metadata = list(note = "CSV, quote \" and newline\nremain intact")
  ))
  path <- tempfile("tradesimr-durable-config-")
  sim_exchange_save(exchange, path)

  config <- data.table::fread(file.path(path, "exchange_config.csv"))
  expect_match(config$config[[1L]], "^hex:[0-9a-f]+$")

  restored <- sim_exchange_load(path)
  expect_equal(restored$config$user_metadata, exchange$config$user_metadata)
})
