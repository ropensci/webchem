# tests for ping
context("ping")

test_that("pubchem_ping returns the correct value", {
  skip_on_cran()

  expect_true(pubchem_ping())
  expect_false(pubchem_ping(503))
  expect_error(pubchem_ping("content"))
})
