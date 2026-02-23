
test_that("setup() errors when not run interactively", {
  skip_if(interactive(), "This test is intended for non-interactive sessions.")
  expect_error(
    setup(),
    "setup\\(\\) must be run in an interactive R session",
    fixed = FALSE
  )
})
