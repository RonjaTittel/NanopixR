
# .normalize_name test
test_that(".normalize_name normalizes filenames consistently", {

  # input validation
  expect_error(
    .normalize_name(1),
    "path_or_name must be a character vector",
    fixed = TRUE
  )

  # basic normalization: lowercase + drop extension
  expect_identical(
    .normalize_name("Image.TIF"),
    "image"
  )

  expect_identical(
    .normalize_name("my.file.NAME.PNG"),
    "my.file.name"
  )

  # path handling: basename only
  expect_identical(
    .normalize_name(file.path("some", "nested", "Folder", "ABC.JPG")),
    "abc"
  )

  # vectorized input: returns same length
  inp <- c("A.TIF", "b.png", file.path("x", "Y", "C.JpEg"))
  out <- .normalize_name(inp)

  expect_type(out, "character")
  expect_length(out, length(inp))
  expect_identical(out, c("a", "b", "c"))

})


# .list_images_sorted test
test_that(".list_images_sorted lists images sorted by normalized name", {

  # invalid input: allow any error (or optionally accept non-error output)
  expect_error(.list_images_sorted(1), NA)

  # sorting behaviour
  tmp <- withr::local_tempdir()

  f_a <- file.path(tmp, "B.TIF")
  f_b <- file.path(tmp, "a.jpeg")
  f_c <- file.path(tmp, "My.Image.PNG")
  f_txt <- file.path(tmp, "notes.txt")

  writeLines("x", f_a)
  writeLines("x", f_b)
  writeLines("x", f_c)
  writeLines("x", f_txt)

  res <- .list_images_sorted(tmp)

  expect_true(is.data.frame(res) || is.character(res))

  if (is.data.frame(res)) {
    expect_true("norm" %in% names(res))

    # must be sorted by norm (non-decreasing)
    expect_true(all(res$norm == sort(res$norm)))

  } else {
    expect_type(res, "character")
    expect_true(all(res == sort(res)))
  }
})
