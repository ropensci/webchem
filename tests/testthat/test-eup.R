skip_on_cran()
skip_on_ci()

if (Sys.getenv("RUN_ONLINE_TESTS") == "true") {
  db_download_foodb(verbose = TRUE)
}

db_exists <- function() {
  tryCatch(
    {
      con <- connect_eup()
      on.exit(DBI::dbDisconnect(con))
      TRUE
    },
    error = function(e) FALSE
  )
}

test_that("eup_list_entries() works", {
  skip_if_not(
    db_exists(),
    message = "Offline database not available"
  )

  substance_id <- eup_list_entries(idtype = "substance_id")
  substance_name <- eup_list_entries(idtype = "substance_name")
  as_cas_number <- eup_list_entries(idtype = "as_cas_number")
  pesticide_residue_id <- eup_list_entries(idtype = "pesticide_residue_id")
  pesticide_residue_name <- eup_list_entries(idtype = "pesticide_residue_name")

  expect_true(length(substance_id) > 1000)
  expect_true(length(substance_name) > 1000)
  expect_true(length(as_cas_number) > 1000)
  expect_true(length(pesticide_residue_id) > 500)
  expect_true(length(pesticide_residue_name) > 500)
  expect_equal(length(substance_id), length(substance_name))
  expect_equal(length(pesticide_residue_id), length(pesticide_residue_name))
  expect_true("2-Propanol" %in% substance_name)
  expect_true("1-Decanol " %in% pesticide_residue_name) # TODO flag whitespace to db maintainers
})

test_that("eup_convert() works with active_substances", {
  skip_if_not(
    db_exists(),
    message = "Offline database not available"
  )

  q1 <- eup_convert(1313, from = "substance_id", to = "substance_name")
  q2 <- eup_convert("Monolinuron", from = "substance_name", to = "as_cas_number")
  q3 <- eup_convert("142459-58-3", from = "as_cas_number", to = "substance_id")

  expect_equal(q1$substance_name, "Flufenacet (formerly fluthiamide)")
  expect_equal(q2$as_cas_number, "1746-81-2")
  expect_true(1313 %in% q3$substance_id)
})

test_that("eup_convert() works with residues", {
  skip_if_not(
    db_exists(),
    message = "Offline database not available"
  )

  # Test conversion from pesticide_residue_name to pesticide_residue_id
  q1 <- eup_convert(
    "1,1-dichloro-2,2-bis(4-ethylphenyl)ethane  (F) ",
    from = "pesticide_residue_name",
    to = "pesticide_residue_id",
    resource = "residues"
  )
  expect_equal(q1$pesticide_residue_id, 1L)
  expect_equal(q1$pesticide_residue_name, "1,1-dichloro-2,2-bis(4-ethylphenyl)ethane  (F) ")

  # Test conversion with another residue name
  q2 <- eup_convert(
    "1-Decanol ",
    from = "pesticide_residue_name",
    to = "pesticide_residue_id",
    resource = "residues"
  )
  expect_equal(q2$pesticide_residue_id, 400L)
  expect_equal(q2$pesticide_residue_name, "1-Decanol ")
})

test_that("eup_convert() handles multiple queries", {
  skip_if_not(
    db_exists(),
    message = "Offline database not available"
  )

  q1 <- eup_convert(c(1313, 1314), from = "substance_id", to = "substance_name")
  expect_equal(nrow(q1), 2L)
  expect_true(all(c("substance_id", "substance_name") %in% names(q1)))
  expect_true("Glutaraldehyde (aka glutardialdehyde)" %in% q1$substance_name)
})

test_that("eup_convert() validates input types", {
  skip_if_not(
    db_exists(),
    message = "Offline database not available"
  )

  expect_error(
    eup_convert("balloon", from = "substance_id", to = "substance_name"),
    "query must be a vector of numbers"
  )
  expect_error(
    eup_convert(123, from = "substance_name", to = "substance_id"),
    "query must be a vector of strings"
  )
})

test_that("eup_query() returns complete active substance data", {
  skip_if_not(
    db_exists(),
    message = "Offline database not available"
  )

  q <- eup_query(query = 1313, resource = "active_substances")

  expect_true(inherits(q, "data.frame"))
  expect_true(nrow(q) > 0)
  expect_true(all(c("substance_id", "substance_name", "as_cas_number") %in% names(q)))
  expect_equal(q$substance_id[1], 1313L)
  expect_equal(q$substance_name[1], "Flufenacet (formerly fluthiamide)")
  expect_equal(q$as_cas_number[1], "142459-58-3")
})

test_that("eup_convert() stops when input is invalid", {
  msg1 <- capture_error(
    eup_convert(1313, from = "substance_id", to = "substance_id")
  )
  msg2 <- capture_error(
    eup_convert(1313, from = "substance_id", to = "substance_name", mode = "ws")
  )

  expect_equal(
    msg1$message,
    "From and to identifier types must be different."
  )
  expect_equal(
    msg2$message,
    "Web service mode is not implemented. Please use mode = 'offline'."
  )
})

test_that("eup_query() stop when input is invalid", {
  msg1 <- capture_error(
    eup_query("balloon", resource = "active_substances")
  )
  msg2 <- capture_error(
    eup_query(query = c(1, 2), resource = "residues", mode = "ws")
  )

  expect_equal(
    msg1$message,
    "query must be a vector of numbers."
  )
  expect_equal(
    msg2$message,
    "Web service mode is not implemented. Please use mode = 'offline'."
  )
})

test_that("eup_query() returns complete residue data", {
  skip_if_not(
    db_exists(),
    message = "Offline database not available"
  )

  q <- eup_query(query = 1, resource = "residues")

  expect_true(inherits(q, "data.frame"))
  expect_true(nrow(q) > 0)
  expect_true(all(c("pesticide_residue_id", "product_code") %in% names(q)))
  expect_true("0243990" %in% q$product_code)
})

test_that("eup_query() handles multiple queries", {
  skip_if_not(
    db_exists(),
    message = "Offline database not available"
  )

  q <- eup_query(query = c(1, 2), resource = "residues")

  expect_true(inherits(q, "data.frame"))
  expect_true(nrow(q) > 0)
  expect_true(all(c(1,2) %in% q$pesticide_residue_id))
})

