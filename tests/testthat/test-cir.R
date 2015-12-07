context("cir")

chk_cir <- function(){
  qurl <- 'http://cactus.nci.nih.gov/chemical/structure/Triclosan/cas/xml'
  Sys.sleep(1)
  cont <- try(getURL(qurl, .encoding = 'UTF-8', .opts = list(timeout = 2)),
              silent = TRUE)
  if (inherits(cont, 'try-error'))
    skip("Server is down!")
}

test_that("cir()", {
  chk_cir()

  expect_equal(cir('Triclosan', 'mw', verbose = FALSE), '289.5451')
  expect_error(cir(c('Triclosan', 'Benzol'), 'mw'))
  expect_equal(cir('xxxxxxx', 'mw', verbose = FALSE), NA)
  expect_warning(cir(NA, 'mw', verbose = FALSE))
  chk_cir()
  expect_equal(cir("3380-34-5", 'stdinchikey', resolver = 'cas_number', verbose = FALSE), "InChIKey=XEFQLINVKFYRCS-UHFFFAOYSA-N")
  chk_cir()
  expect_true(length(cir('Triclosan', 'cas', verbose = FALSE)) > 1)
  chk_cir()
  expect_equal(length(cir('Triclosan', 'cas', first = TRUE, verbose = FALSE)), 1)
})
