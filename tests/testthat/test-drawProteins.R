test_that("Able to retrieve random protein", {
  k <- getRandomProtein("9964")
  expect_true(!is.null(k))
  expect_true(length(k) > 0)
})

test_that("Able to remotly download protein",{
  k <- getRemote("Q04206")
  expect_true(!is.null(k))
  expect_true(length(k) > 0)
})

test_that("getProtein function remote access operation works",{
  k <- getProtein("Q04206", FALSE)
  expect_true(!is.null(k))
  expect_true(length(k) > 0)
})

test_that("getProtein function random works",{
  k <- getProtein("random", FALSE)
  expect_true(!is.null(k))
  expect_true(length(k) > 0)
})

test_that("getProtein function random number operation works",{
  k <- getProtein("random number:2", FALSE)
  expect_true(!is.null(k))
  expect_true(length(k) > 0)
})

test_that("getProtein function random orgid function works",{
  k <- getProtein("random orgid:10090", FALSE)
  expect_true(!is.null(k))
  expect_true(length(k) > 0)
})

test_that("getProtein function random operation fully works",{
  k <- getProtein("random number:2 orgid:10090", FALSE)
  expect_true(!is.null(k))
  expect_true(length(k) > 0)
})

test_that("drawProtein function works",{
  f <- drawProtein(proteins = list(type = c("Q04206", "random orgid:10090 number:5"), colors = c("red", "random number:4", "green")),
                   types = list(type = c("domain", "region of interest"), colors = c("black", "pink")),
                   descriptionSearch = "phos",
                   offSetFeatures = list(type = c("strand", "helix", "turn"), colors = c("green", "orange", "purple")),
  )
  expect_true(!is.null(f))
})
