context("apply linting rules")

if (requireNamespace("lintr", quietly = TRUE)) {
  context("test linting")
  test_that("Package Style", {
    lintr::expect_lint_free()
  })
}
