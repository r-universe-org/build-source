test_that("get_authors_info() parses all Authors@R persons, maintainer included", {
  dir <- local_description(authors_r_lines)
  res <- get_authors_info(dir)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 3)
  expect_equal(res$name, c("Jeroen Ooms", "Duncan Temple Lang", "rOpenSci"))
  expect_equal(res$email, c("jeroenooms@gmail.com", NA, NA))
  expect_equal(res$orcid, c("0000-0002-4035-0289", NA, NA))
  expect_equal(res$role, list(c("aut", "cre", "cph"), c("aut", "cph"), "fnd"))
})

test_that("get_authors_info() falls back to Maintainer without Authors@R", {
  dir <- local_description(c("Author: Jane Doe", "Maintainer: Jane Doe <Jane@example.org>"))
  res <- get_authors_info(dir)
  expect_equal(nrow(res), 1)
  expect_equal(res$name, "Jane Doe")
  expect_equal(res$email, "jane@example.org")
  expect_true(is.na(res$orcid))
  expect_equal(res$role, list("cre"))
})

test_that("get_authors_info() errors on malformed Authors@R", {
  dir <- local_description("Authors@R: c(person(\"Jane\", ")
  expect_error(get_authors_info(dir))
})

test_that("get_authors_info() serialises to the expected JSON", {
  dir <- local_description(authors_r_lines)
  json <- jsonlite::toJSON(get_authors_info(dir))
  parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_equal(parsed[[1]], list(
    name = "Jeroen Ooms",
    email = "jeroenooms@gmail.com",
    orcid = "0000-0002-4035-0289",
    role = list("aut", "cre", "cph")
  ))
  expect_equal(parsed[[2]], list(name = "Duncan Temple Lang", role = list("aut", "cph")))
  expect_equal(parsed[[3]], list(name = "rOpenSci", role = list("fnd")))
})

test_that("get_maintainer_info() returns name, email and ORCID of the cre", {
  dir <- local_description(authors_r_lines)
  info <- with_no_maintainer_env(get_maintainer_info(dir))
  expect_equal(info, list(
    name = "Jeroen Ooms",
    email = "jeroenooms@gmail.com",
    orcid = "0000-0002-4035-0289"
  ))
})

test_that("get_maintainer_info() has no orcid when the cre has none", {
  dir <- local_description("Authors@R: person(\"Jane\", \"Doe\", , \"jane@example.org\", role = c(\"aut\", \"cre\"))")
  info <- with_no_maintainer_env(get_maintainer_info(dir))
  expect_equal(info, list(name = "Jane Doe", email = "jane@example.org"))
})

test_that("get_maintainer_info() works with a plain Maintainer field", {
  dir <- local_description(c("Author: Jane Doe", "Maintainer: Jane Doe <jane@example.org>"))
  info <- with_no_maintainer_env(get_maintainer_info(dir))
  expect_equal(info, list(name = "Jane Doe", email = "jane@example.org"))
})

test_that("get_maintainer_info() does not fail on malformed Authors@R", {
  dir <- local_description("Authors@R: c(person(\"Jane\", ")
  info <- with_no_maintainer_env(get_maintainer_info(dir))
  expect_equal(info, list(name = "Invalid DESCRIPTION", email = "authors@r.error"))
})
