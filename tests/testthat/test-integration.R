# These all might occasionally fail because cts_translate() is currently somewhat unreliable.

fn_up <- ping_service("fn")
up <- ping_service("cts")

test_that("autotranslate works when no translation needed", {
  skip_on_cran()
  skip_if_not(fn_up, "Flavornet down!")
  skip_if_not(up, "CTS service down")

  CASs <- c("75-07-0",  "64-17-5")
  a <- autotranslate(query = CASs, from = "cas", .f = "fn_percept", .verbose = TRUE)
  b <- fn_percept(CASs)
  expect_equal(a, b)
})

etox_up <- ping_service("etox")
test_that("autotranslate translates", {
  skip_on_cran()
  skip_if_not(etox_up, "ETOX down!")
  skip_if_not(up, "CTS service down")

  x <- autotranslate(query = "XDDAORKBJWWYJS-UHFFFAOYSA-N", from = "inchikey", .f = "get_etoxid")
  y <- get_etoxid(query = "1071-83-6", from = "cas")
  expect_equal(x, y)
})


test_that("has_entry() function works", {
  skip_on_cran()
  skip_if_not(fn_up)
  skip_if_not(etox_up)
  skip_if_not(up, "CTS service down")

  out <- has_entry(c("triclosan", NA, "balloon"),
                        from = "name",
                        sources = c("etox", "fn"))
  df <- tibble(query = c("triclosan", NA, "balloon"),
               etox = c(TRUE, FALSE, FALSE),
               fn = c(FALSE, FALSE, FALSE))
  expect_equivalent(out, df)
})

