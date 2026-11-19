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
