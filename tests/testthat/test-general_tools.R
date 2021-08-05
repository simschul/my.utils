test_that("zero_range works", {
  expect_equal(
    zero_range(c(1,1 + 1E-12)), 
    scales::zero_range(c(1,1 + 1E-12))
  )
})

test_that("rescaling of variable works", {
  expect_equal(
    rescale(scale(1:10)), 
    1:10
  ) 
  expect_equal(
    rescale(scale(matrix(1:10, ncol = 1))), 
    1:10
  )  
})


test_that("normalisation  works", {
  expect_equal(
    renormalize(normalize(1:10)), 
    1:10
  )
  expect_equal(
    renormalize(normalize(matrix(1:10, ncol = 1))), 
    matrix(1:10, ncol = 1)
  )
})


test_that("is inverse fun works", {
  expect_true(
    is_inverse_function(sqrt, function(x) x^2)
  )
  expect_false(
    is_inverse_function(function(x) x + 1, function(x) x - 2)
  )
})


test_that("lookup tools work", {
  expect_equal(
    common_elements(LETTERS[1:10], letters[1:10], ignore.case = TRUE), 
    letters[1:10]
  )
  expect_equal(
    common_elements(LETTERS[1:10], letters[1:10], ignore.case = FALSE), 
    character(0)
  )
  expect_equal(
    common_elements(LETTERS[1:10], LETTERS[8:18], ignore.case = FALSE), 
    LETTERS[8:10]
  )  
  expect_equal(
    non_common_elements(LETTERS[1:10], LETTERS[8:18], ignore.case = FALSE), 
    LETTERS[1:7]
  )  
  expect_equal(
    substr_right('lkjdsfbsdft', 3), 
    'dft'
  )
})




