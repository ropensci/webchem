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

  fl <- aw_query('Fluazinam', type = 'commonname')
  sm <- aw_query("S-Metolachlor", type = 'commonname')
  xx <- aw_query('xxxxx', type = 'commonname')
  xx2 <- aw_query('xxxxx', type = 'cas')

  expect_error(aw_query(c('Fluazinam', 'xxx'), type = 'commonname'))

  expect_equal(fl$cas, "79622-59-6", verbose = FALSE)
  expect_equal(length(sm$inchikey), 2)
  expect_equal(length(sm$inchi), 2)
  expect_equal(xx, NA, verbose = FALSE)
  expect_equal(xx2, NA, verbose = FALSE)
  expect_equal(length(fl), 11)
})


test_that("alanwood, cas", {
  chk_alanwood()

  cs <-  aw_query("79622-59-6", type = 'cas')
  xx <- aw_query('xxxxx', type = 'cas')

  b1 <- aw_query('12071-83-9', type = 'cas')  # BUG: failed because of missing inchi / inchikey
  expect_equal(b1$cas, '12071-83-9')
  expect_equal(b1$inchikey, NA)

  expect_equal(cs$cas, "79622-59-6", verbose = FALSE)
  expect_equal(cs$cname, "fluazinam", verbose = FALSE)
  expect_equal(xx, NA, verbose = FALSE)
})
