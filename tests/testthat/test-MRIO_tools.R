mat <- matrix(1:9, 3,3)
mat[1,3] <- NA
mats <- as.sparse.matrix(mat)
mats2 <- copy(mats)
mats2[, row := as.character(row)]
mats2[, col := as.character(col)]

test_that("from dense to sparse matrix works", {
  expect_equal(as.sparse.matrix(mat)$value, 
               as.numeric(na.exclude(as.numeric(mat))))
  expect_equal(ncol(as.sparse.matrix(mat)), 
               3)
  expect_equal(
    ncol(as.sparse.matrix(mat, 
                     colnames = data.table(Var1 = LETTERS[1:3], Var2 = letters[1:3]), 
                     rownames = data.table(Var1 = LETTERS[4:6], Var2 = letters[4:6]))), 
    5
  )
 expect_equal(
   as.dense.matrix(as.sparse.matrix(mat)), 
   mat
 )  
})


test_that('IO visualations work', {
  expect_silent(IOvisualize(mat))
  expect_silent(IOvisualize(mats))
  expect_silent(IOvisualize(mats, attributes = list('row' = mats2[, 1], 
                                                    'col' = mats2[,2])))  
})


