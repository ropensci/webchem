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
  fl5 <- pp_query(c('55-38-9', '50-29-3', '4151-50-2', 'xxxxx'))

  expect_equal(fl[[1]]$cas, "50-00-0")
  expect_equal(fl[[1]]$cname, "FORMALDEHYDE")
  expect_equal(fl[[1]]$prop$value[fl[[1]]$prop$variable == 'Water Solubility'], 400000)
  expect_equal(length(fl[[1]]), 5)
  expect_true(is.data.frame(fl[[1]]$prop))
  expect_equal(xx[[1]], NA)
  expect_equal(names(fl[[1]]$prop), c("variable", "value", "unit", "temp", "type", "ref"))

  expect_equal(length(fl5), 4)
  expect_equal(fl5[[1]]$cas, "55-38-9")
  expect_equal(fl5[[2]]$cas, "50-29-3")
  expect_true(is.na(fl5[[3]]$prop$type[2]))
  expect_true(is.na(fl5[[4]]))
})



