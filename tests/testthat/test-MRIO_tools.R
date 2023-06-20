mat <- matrix(1:9, 3,3)
mat[1,3] <- NA
mats <- as.sparse.matrix(mat)
mats2 <- copy(mats)
mats2[, row := as.character(row)]
mats2[, col := as.character(col)]

dense_mat_w_names <-  matrix(1:9, 3,3)
colnames(dense_mat_w_names) <- LETTERS[1:3]
rownames(dense_mat_w_names) <- letters[1:3]

#reshape2::melt(dense_mat_w_names) %>% as.data.table


test_that("from dense to sparse matrix works", {
  expect_equal(as.sparse.matrix(mat, na.rm = TRUE)$value, 
               as.numeric(na.exclude(as.numeric(mat))))
  expect_equal(ncol(as.sparse.matrix(mat)), 
               3)
  expect_equal(
    ncol(as.sparse.matrix(mat, 
                     colnames = data.table(Var1 = LETTERS[1:3], Var2 = letters[1:3]), 
                     rownames = data.table(Var1 = LETTERS[4:6], Var2 = letters[4:6]))), 
    7
  )
 expect_equal(
   as.dense.matrix(as.sparse.matrix(mat)), 
   mat
 )  
 expect_equal(
   as.sparse.matrix(dense_mat_w_names)$row, 
   rep(letters[1:3], 3)
 )
 expect_equal(
   as.sparse.matrix(dense_mat_w_names)$col, 
   rep(LETTERS[1:3], each = 3)
 )
 
})


test_that('IO visualations work', {
  expect_silent(IOvisualize(mat))
  expect_silent(IOvisualize(mats))
  expect_silent(IOvisualize(mats, attributes = list('row' = mats2[, 1], 
                                                    'col' = mats2[,2])))  
})


