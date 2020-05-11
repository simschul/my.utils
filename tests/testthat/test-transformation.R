test_that("transformation and re-transformation works", {
  x <- runif(1:10, min = 0.01, max = 1)
  # Standardization
  expect_equal(x %>% scale %>% rescale, x)
  # Normalization
  expect_equal(x %>% normalize %>% renormalize, x)
  # Transformation
  expect_equal(x %>% transform %>% retransform, x)
  expect_equal(x, x %>% 
                 transform(., fun = log, fun_inverse = exp) %>% 
                 retransform)
  expect_equal(x, x %>% 
                 transform(., fun = log) %>% 
                 retransform(inverse_fun = exp))
  expect_equal(x, x %>% 
                 transform(., fun = log, scale = TRUE) %>% 
                 retransform(inverse_fun = exp))
  expect_equal(x, x %>% 
                 transform(., fun = log, normalize = TRUE) %>% 
                 retransform(inverse_fun = exp))
  
  # is inverse function
  expect_true(is_inverse_function(exp, log))
  expect_true(is_inverse_function(log, exp))
  expect_true(is_inverse_function(sqrt, function(x)x^2))  
})


