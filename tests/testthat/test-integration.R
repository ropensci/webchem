# These all might occasionally fail because cts_translate() is currently somewhat unreliable.

fn_up <- ping_service("fn")
test_that("autotranslate works when no translation needed", {
  skip_if_not(fn_up, "Flavornet down!")
  CASs <- c("75-07-0",  "64-17-5")
  a <- autotranslate(query = CASs, from = "cas", .f = "fn_percept", .verbose = FALSE)
  b <- fn_percept(CASs)
  expect_equal(a, b)
})

etox_up <- ping_service("etox")
test_that("autotranslate translates", {
  skip_if_not(etox_up, "ETOX down!")
  x <- autotranslate("XDDAORKBJWWYJS-UHFFFAOYSA-N", from = "inchikey", .f = "get_etoxid")
  y <- get_etoxid("1071-83-6", from = "cas")
  expect_equal(x, y)
})


test_that("coverge function works", {
  skip_if_not(fn_up)
  skip_if_not(etox_up)
  out <- check_coverage(c("triclosan", NA, "balloon"),
                        from = "name",
                        sources = c("etox", "fn"))
  df <- tibble(query = c("triclosan", NA, "balloon"),
               etox = c(TRUE, FALSE, FALSE),
               fn = c(FALSE, FALSE, FALSE))
  expect_equivalent(out, df)
})

