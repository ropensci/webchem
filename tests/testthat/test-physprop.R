context("physprop")

require(RCurl)
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

  xx <- pp_query('xxxxx')
  fl <- pp_query('50-00-0')
  fl2 <- pp_query('55-38-9')
  fl3 <- pp_query('50-29-3')
  fl4 <- pp_query('4151-50-2')


  expect_error(pp_query(c('xxxxx', 'xxxxx')))
  expect_equal(fl$cas, "50-00-0")
  expect_equal(fl$cname, "FORMALDEHYDE")
  expect_equal(fl$prop$value[fl$prop$variable == 'Water Solubility'], 400000)
  expect_equal(length(fl), 5)
  expect_true(is.data.frame(fl$prop))
  expect_equal(xx, NA)
  expect_equal(names(fl$prop), c("variable", "value", "unit", "temp", "type", "ref"))

  expect_equal(fl2$cas, "55-38-9")
  expect_equal(fl3$cas, "50-29-3")
  expect_true(is.na(fl4$prop$type[2]))
})



