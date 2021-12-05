up <- ping_service("cts")
test_that("cts_compinfo()", {
  skip_on_cran()

  skip_if_not(up, "CTS service down")

  expect_true(is.na(cts_compinfo("xxx")))

  o1 <- suppressWarnings(cts_compinfo("XEFQLINVKFYRCS-UHFFFAOYSA-N"))
  o2 <- suppressWarnings(cts_compinfo(c("XEFQLINVKFYRCS-UHFFFAOYSA-N",
                                        "XEFQLINVKFYRCS-UHFFFAOYSA-X")))
  expect_equal(suppressWarnings(cts_compinfo("XEFQLINVKFYRCS-UHFFFAOYSA-X"))[[1]], NA)
  expect_true(is.na(cts_compinfo("XEFQLINVKFYRCS-UHFFFAOYSA-X")))
  expect_length(o1[[1]], 11)
  expect_equal(round(o1[[1]][["molweight"]], 3), 289.542)
  expect_length(o2, 2)
  expect_true(is.na(o2[[2]]))
})


test_that("cts_convert()", {
  skip_on_cran()
  skip_if_not(up, "CTS service down")

  comp <- c('Triclosan', 'Hexane')
  expect_error(cts_convert(comp, c('Chemical Name', 'CAS'), 'CAS'))
  expect_error(cts_convert('Triclosan', 'CAS'))
  expect_true(is.na(cts_convert('xxxx', 'Chemical Name', 'inchikey')))

  o1 <- cts_convert(comp, 'Chemical Name', 'inchikey', match = "first")
  expect_length(o1, 2)

  #Test below currently returns NA but it shouldn't.
  #Seems to be an issue with the webservice.
  #expect_equal(o1[[1]], 'XEFQLINVKFYRCS-UHFFFAOYSA-N')

  #edit tests and function note if test below no longer returns NA.
  expect_equal(cts_convert("triclosan", "chemical name", "inchikey")$triclosan,
               NA)

  expect_equal(cts_convert(NA, from = "Chemical Name", to = "inchikey"),
               list(NA), ignore_attr = TRUE)


})

test_that("fromto", {
  skip_on_cran()
  skip_if_not(up, "CTS service down")
  to <- cts_to()
  from <- cts_from()

  expect_type(to, "character")
  expect_type(from, "character")
})
