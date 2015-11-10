context("physprop")

chk_physprop <- function(){
  qurl <- 'http://esc.syrres.com/fatepointer/webprop.asp?CAS=50000'
  Sys.sleep(0.2)
  cont <- try(getURL(qurl, .encoding = 'UTF-8', .opts = list(timeout = 3)),
              silent = TRUE)
  if (inherits(cont, 'try-error'))
    skip("Server is down!")
}



test_that("physprop returns correct results", {
  chk_physprop()

  xx <- physprop('xxxxx')
  fl <- physprop('50-00-0')

  expect_error(physprop(c('xxxxx', 'xxxxx')))
  expect_equal(fl$cas, "50-00-0")
  expect_equal(fl$cname, "FORMALDEHYDE")
  expect_equal(fl$prop$value[fl$prop$variable == 'Water Solubility'], 400000)
  expect_equal(length(fl), 4)
  expect_true(is.data.frame(fl$prop))
  expect_equal(xx, NA)
  expect_equal(names(fl$prop), c("variable", "value", "unit", "temp", "type", "ref"))
})



