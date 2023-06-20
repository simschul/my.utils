library(data.table)
data <- c('C', 'C10', 'C11-13', 'C11-C13', 'C11_12', 'C11_C12', 'ABC12', 'ABC_12', 'C1_2')
type <- c('level1', 'level2', 'combination_bar', 'combination_bar', 'combination_underscore','combination_underscore', 'none', 'none', 'none')


test_that("NACErev2 combination detection works", {
  expect_equal(
    data[is_combination_bar(data)],
    data[type == 'combination_bar'] 
  )
  
  expect_equal(
    data[is_combination_underscore(data)],
    data[type == 'combination_underscore'] 
  )
  
  expect_equal(
    data[is_combination(data)],
    data[type %in% c('combination_bar', 'combination_underscore')]
  )
})


test_that('NACErev2 detection works', {
  expect_equal(
    data[is_NACE_level1(data)], 
    data[type == 'level1']
  )
  expect_equal(
    data[is_NACE_level2(data)], 
    data[type == 'level2']
  )
})


test_that('NACErev2 disaggregation works', {
  expect_equal(
    split_NACE_combinations(data.table(source = c('C11_C12'), 
                                       target = c('a')), 1), 
    data.table(source = c('C11', 'C12'), 
               target = rep('a',2))
  )
  
  expect_equal(
    split_NACE_combinations(data.table(source = c('C11_12'), 
                                       target = c('a')), 1), 
    data.table(source = c('C11', 'C12'), 
               target = rep('a',2))
  )
  
  expect_equal(
    split_NACE_combinations(data.table(source = c('C11-C13'), 
                                       target = c('a')), 1), 
    data.table(source = c('C11', 'C12', 'C13'), 
               target = rep('a',3))
  )
  
  expect_equal(
    split_NACE_combinations(data.table(source = c('C11-13'), 
                                       target = c('a')), 1), 
    data.table(source = c('C11', 'C12', 'C13'), 
               target = rep('a',3))
  )
  
  expect_equal(
    split_NACE_combinations(data.table(source = c('C11'), 
                                       target = c('a')), 1), 
    data.table(source = c('C11'), 
               target = c('a'))
  )
  
  expect_equal(
    split_NACE_combinations(data.table(source = c('ABC_11'), 
                                       target = c('a')), 1), 
    data.table(source = c('ABC_11'), 
               target = c('a'))
  )
  
})


