
# ── setup(): direct tests ─────────────────────────────────────────────────────
test_that("setup - throws error in non-interactive session", {
  # interactive() returns TRUE in RStudio/devtools -> must be mocked explicitly
  mockery::stub(setup, "interactive", FALSE)
  expect_error(setup(),
               "setup\\(\\) must be run in an interactive R session")
})

# ── setup(): mocked tests ─────────────────────────────────────────────────────
test_that("setup - throws error if setup script is not found", {
  mockery::stub(setup, "interactive", TRUE)
  mockery::stub(setup, "system.file", "")

  expect_error(setup(), "Setup script not found")
})

test_that("setup - throws error if setup_env() is missing from script", {
  mockery::stub(setup, "interactive", TRUE)
  mockery::stub(setup, "system.file", "/some/valid/path.R")

  # sys.source loads an empty environment -> setup_env will not exist
  mockery::stub(setup, "sys.source", function(script, envir) invisible(NULL))

  expect_error(setup(), "setup_env\\(\\) not found in setup script")
})

test_that("setup - calls setup_env() and returns TRUE on success", {
  mockery::stub(setup, "interactive", TRUE)
  mockery::stub(setup, "system.file", "/some/valid/path.R")

  mockery::stub(setup, "sys.source", function(script, envir) {
    assign("setup_env", function() invisible(TRUE), envir = envir)
  })

  result <- setup()
  expect_true(isTRUE(result))
})

test_that("setup - returns NULL if setup_env() returns NULL", {
  mockery::stub(setup, "interactive", TRUE)
  mockery::stub(setup, "system.file", "/some/valid/path.R")

  mockery::stub(setup, "sys.source", function(script, envir) {
    assign("setup_env", function() invisible(NULL), envir = envir)
  })

  result <- setup()
  expect_null(result)
})

# ── setup_env(): tests via mocked readline ────────────────────────────────────
# Note: prompt_yn is a local closure inside setup_env() and cannot be extracted.
# Its behaviour is therefore tested indirectly through setup_env() itself.

# load setup_env() into a test environment
script_path <- system.file("scripts", "setup_env.R", package = "NanopixR")
skip_if(script_path == "", "setup_env.R script not found - skipping setup_env tests")
env_setup <- new.env(parent = baseenv())
sys.source(script_path, envir = env_setup)

test_that("setup_env - cancels and returns NULL when user enters 'stop'", {
  mockery::stub(env_setup$setup_env, "readline", "stop")
  result <- env_setup$setup_env()
  expect_null(result)
})

test_that("setup_env - cancels and returns NULL when user enters 'exit'", {
  mockery::stub(env_setup$setup_env, "readline", "exit")
  result <- env_setup$setup_env()
  expect_null(result)
})

test_that("setup_env - cancels and returns NULL when user enters 'quit'", {
  mockery::stub(env_setup$setup_env, "readline", "quit")
  result <- env_setup$setup_env()
  expect_null(result)
})

test_that("setup_env - returns TRUE if user confirms prior session use", {
  # "y" at first prompt -> "Have you already used this tool in this R session?"
  # -> skips setup, returns TRUE immediately
  mockery::stub(env_setup$setup_env, "readline", "y")
  result <- env_setup$setup_env()
  expect_true(isTRUE(result))
})

test_that("setup_env - cancels at second prompt (first-time system setup)", {
  # first prompt "n" (not used this session yet)
  # second prompt "stop" (cancel at "Have you used this on this system before?")
  responses <- c("n", "stop")
  i <- 0
  mockery::stub(env_setup$setup_env, "readline", function(...) {
    i <<- i + 1
    responses[[i]]
  })
  result <- env_setup$setup_env()
  expect_null(result)
})

test_that("setup_env - returns NULL when no Python path is provided", {
  # navigate past all yes/no prompts to Python path input
  # n -> not used this session
  # y -> used this system before (skips installation steps)
  # "" -> empty Python path -> should cancel
  responses <- c("n", "y", "")
  i <- 0
  mockery::stub(env_setup$setup_env, "readline", function(...) {
    i <<- i + 1
    responses[[i]]
  })
  result <- env_setup$setup_env()
  expect_null(result)
})

test_that("setup_env - returns NULL when provided Python path does not exist", {
  # n -> not used this session
  # y -> used this system before
  # non-existent path -> should cancel
  responses <- c("n", "y", "/non/existent/python")
  i <- 0
  mockery::stub(env_setup$setup_env, "readline", function(...) {
    i <<- i + 1
    responses[[i]]
  })
  result <- env_setup$setup_env()
  expect_null(result)
})
