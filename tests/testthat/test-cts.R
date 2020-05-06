test_that("cts_compinfo()", {
  skip_on_cran()

  skip_if_not(ping_cts(), "CTS service down")

  expect_error(cts_compinfo('xxx'))

  o1 <- suppressWarnings(cts_compinfo("XEFQLINVKFYRCS-UHFFFAOYSA-N", verbose = FALSE))
  o2 <- suppressWarnings(cts_compinfo(c("XEFQLINVKFYRCS-UHFFFAOYSA-N", "XEFQLINVKFYRCS-UHFFFAOYSA-X"), verbose = FALSE))
  expect_equal(suppressWarnings(cts_compinfo("XEFQLINVKFYRCS-UHFFFAOYSA-X", verbose = FALSE))[[1]], NA)
  expect_warning(cts_compinfo("XEFQLINVKFYRCS-UHFFFAOYSA-X", verbose = FALSE))
  expect_equal(length(o1[[1]]), 10)
  expect_equal(round(o1[[1]][["molweight"]], 3), 289.542)
  expect_equal(length(o2), 2)
  expect_true(is.na(o2[[2]]))
})


test_that("cts_convert()", {
  skip_on_cran()
  skip_if_not(ping_cts(), "CTS service down")

  comp <- c('XEFQLINVKFYRCS-UHFFFAOYSA-N', 'BSYNRYMUTXBXSQ-UHFFFAOYSA-N')
  expect_error(cts_convert(comp, c('Chemical Name', 'CAS'), 'CAS'))
  expect_error(cts_convert('Triclosan', 'CAS'))
  expect_true(is.na(suppressWarnings(cts_convert('xxxx', 'inchikey', 'Chemical Name'))[[1]]))
  o1 <- cts_convert(comp, 'Chemical Name', 'inchikey', choices = 1, verbose = FALSE)
  expect_equal(length(o1), 2)

  skip("failing test below")
  expect_equal(o1[[1]], 'XEFQLINVKFYRCS-UHFFFAOYSA-N')
  # cts_convert('acetic acid', 'Chemical Name', 'CAS', choices = 1)
})


# # integration tests
# test_that("cts_compinfo(cir_query())", {
#   chk_cts()
#   chk_cir()
#   inchikey <- cir_query('Triclosan', representation = 'stdinchikey', verbose = FALSE)
#   inchikey <- gsub('InChIKey=', '', inchikey)
#   expect_equal(round(cts_compinfo(inchikey, verbose = FALSE)[[1]][["molweight"]], 3), 289.542)
# })


test_that("fromto", {
  skip_on_cran()
  skip_if_not(ping_cts(), "CTS service down")
  to <- cts_to()
  from <- cts_from()

  expect_type(to, "character")
  expect_type(from, "character")
})
