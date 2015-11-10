context("alanwood")

require(RCurl)
chk_alanwood <- function(){
  qurl <- 'http://www.alanwood.net/pesticides/index_cn.html'
  Sys.sleep(0.2)
  cont <- try(getURL(qurl, .encoding = 'UTF-8', .opts = list(timeout = 3)),
              silent = TRUE)
  if (inherits(cont, 'try-error'))
    skip("Server is down!")
}

test_that("alanwood, commonname", {
  chk_alanwood()

  fl <- alanwood('Fluazinam', type = 'commonname')
  sm <- alanwood("S-Metolachlor", type = 'commonname')
  xx <- alanwood('xxxxx', type = 'commonname')

  expect_error(alanwood(c('Fluazinam', 'xxx'), type = 'commonname'))

  expect_equal(fl$cas, "79622-59-6", verbose = FALSE)
  expect_equal(length(sm$inchikey), 2)
  expect_equal(length(sm$inchi), 2)
  expect_equal(xx, NA, verbose = FALSE)
  expect_equal(length(fl), 10)
})


test_that("alanwood, cas", {
  chk_alanwood()

  cs <-  alanwood("79622-59-6", type = 'cas')
  xx <- alanwood('xxxxx', type = 'cas')

  expect_equal(cs$cas, "79622-59-6", verbose = FALSE)
  expect_equal(cs$cname, "fluazinam", verbose = FALSE)
  expect_equal(xx, NA, verbose = FALSE)
})
