up <- ping_service("chebi")
test_that("examples in the article are unchanged", {
  skip_on_cran()
  skip_if_not(up, "CHEBI service is down")

  data("lc50", package = "webchem")
  cas_rns <- lc50[order(lc50$value)[1:3], "cas"]
  chebiids <- get_chebiid(cas_rns)
  comp <- chebi_comp_entity(chebiids$chebiid)
  pars <- lapply(comp, function(x) {
    with(x, parents[parents$type == "has role", ])
  })

  expect_equal(cas_rns, c("563-12-2", "96182-53-5", "3383-96-8"))
  expect_equal(chebiids$chebiid, c("CHEBI:38663", "CHEBI:38951", "CHEBI:38954"))
  expect_equal(chebiids$chebiasciiname, c("ethion", "tebupirimfos", "temephos"))
  expect_equal(pars$`CHEBI:38663`$chebiName,
               c("insecticide", "environmental contaminant",
                 "EC 3.1.1.7 (acetylcholinesterase) inhibitor", "acaricide",
                 "agrochemical"))
  expect_equal(pars$`CHEBI:38951`$chebiName,
               "EC 3.1.1.7 (acetylcholinesterase) inhibitor")
  expect_equal(pars$`CHEBI:38954`$chebiName,
               c("EC 3.1.1.7 (acetylcholinesterase) inhibitor", "acaricide",
                 "agrochemical", "ectoparasiticide"))
})

test_that("chebi returns correct results", {
  skip_on_cran()
  skip_if_not(up, "CHEBI service is down")
  a <- get_chebiid("Glyphosate", from = "all")
  b <- get_chebiid(c("triclosan", "glyphosate", "balloon", NA))
  A <- chebi_comp_entity("CHEBI:27744")
  B <- chebi_comp_entity("27732")

  expect_is(a, "data.frame")
  expect_is(b, "data.frame")
  expect_is(A, "list")
  expect_is(B, "list")

  expect_equal(names(a)[2], "chebiid")
  expect_length(names(a), 5)
  expect_length(names(b), 5)
  expect_equal(A$`CHEBI:27744`$regnumbers$data[1], "1071-83-6")
  expect_equal(B$`27732`$properties$chebiasciiname, "caffeine")
  expect_equal(B$`27732`$properties$entitystar, "3")
})

test_that("get_chebiid() handles special characters in SMILES",{
  skip_on_cran()
  skip_if_not(up, "CHEBI service is down")

  expect_equal(get_chebiid("C#C", from = "smiles")$chebiid, "CHEBI:27518")
})
