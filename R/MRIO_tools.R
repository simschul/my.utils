#' Title
#'
#' @param mat a numeric matrix
#' @param threshold
#' @param maxpoints
#' @param cex either "absolut" (default), "increasing" or "decreasing". Gives the size of the bubbles. 
#' @param attributes a list with two elements "row" and "col". Each element should be a data.frame with the same number of rows as ncol(mat) or nrow(mat), respectively. If not given and the matrix has colnames and rownames these are taken. 
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
IOvisualize <- function (mat, threshold, maxpoints = 10000, cex = "absolut",
                         attributes = NULL 
                         , ...)  {
  # maybe mat is a sparse matrix?
  if (!is.matrix(mat)) mat <- as.dense.matrix(mat)
  # threshold argument not needed if matrix is (i) not big enough or (ii) does not have enough non-NA values
  if (maxpoints > (ncol(mat) * nrow(mat)) | length(mat[is.na(mat)]) < maxpoints) {
    min_threshold <- 0
  } else {
    suppressWarnings(min_threshold <- mat %>% abs %>%
                       fsort(.,decreasing = TRUE, na.last = TRUE) %>%
                       .[maxpoints])
  }
  
  if (missing(threshold)) {
    threshold <- min_threshold
  }
  if (min_threshold > threshold) {
    warning(paste0("maxpoints = ", maxpoints, " reached: ",
                   "threshold taken to ", min_threshold))
    threshold <- min_threshold
  }
  mat[mat < threshold & mat > -threshold] <- NA
  res <- mat %>% as.sparse.matrix
  
  # Adding adidtional attributes (optional)
  if (is.null(attributes)) {
    attributes <- list("row" = data.table(rownames = rownames(mat)), 
                                                "col" = data.table(colnames = colnames(mat)))
  } 
  if (!(exists("row", attributes) & exists("col", attributes))) {
    warning("attributes needs to have both arguments col and row!")
  } else if (nrow(attributes$row) == nrow(mat) & 
             nrow(attributes$col) == ncol(mat)) {
      # either: attributes are given, or matrix has both row- and colnames
      attributes <- lapply(attributes, function(x) {
        x[,"id" := 1:.N]
      })
      res <- merge(res, attributes$row,
                   by.x = "row", by.y = "id",
                   suffixes = c(".row",".col")) %>%
        merge(., attributes$col, by.x = "col",
              by.y = "id", suffixes = c(".row", ".col"))
  }

  ##################################################
  res <- res %>% .[, `:=`(row, -row)] %>% sf::st_as_sf(coords = c("col",
                                                                  "row"),
                                                       remove = FALSE)
  res$row <- -res$row
  if (cex == "absolut") {
    res[["abs_value"]] <- abs(res$value)
    cex <- "abs_value"
  }  else if (cex == "increasing") {
    cex <- "value"
  } else if (cex == "decreasing") {
    res[["dec_value"]] <- -(res$value)
    cex <- "dec_value"
  }
  mapview::mapview(res, alpha = 0.3, lwd = 0, cex = cex,
                   color = viridis::viridis,
                   zcol = "value", layer.name = "value" ,...)
}

#' Title
#' 
#' @param mat
#'
#' @return
#' @export
#'
#' @examples
as.sparse.matrix <- function(mat) {
  mat <- data.table::as.data.table(mat)
  colnames(mat) <- paste0(1:ncol(mat))
  #cat(is.data.table(mat))
  mat <- mat[, "row" := 1:.N]
  # mat <- cbind(mat, row = 1:nrow(mat))
  mat <- data.table::melt(mat, id.vars = "row", na.rm = TRUE,
                          variable.name = "col") %>%
    .[, col := as.integer(col)] %>%
    .[]
  return(mat)
}


#' Title
#'
#' @param x a sparse matrix in the form a either a data.frame or data.table. Needs to have 3 columns. Default order: row | col | value. If order differs please specify `row`, `col` and `value` arguments. Row and col columns can be either integers (representing the location in the matrix) or character.
#' @param row which column of x represent the row-index? default 1
#' @param col which column of x represent the column-index? default 2
#' @param value which column of x represent the value? default 3
#' @param keep.names only considered if the `row` and `col` columns of `x` are of type character. 
#'
#' @return
#' @export
#'
#' @examples

as.dense.matrix <- function(x, row = 1, col = 2, value = 3, 
                            keep.names = TRUE) {
  if (mode(x) != "data.frame") x <- as.data.frame(x)
  
  mat <- matrix(NA, ncol = length(unique(x[,col])), 
                nrow = length(unique(x[,row])))
  mat[cbind(as.factor(x[,row]), as.factor(x[,col]))] <- x[,value] # as.factor needed to also work with non-integers row/col IDs
  
  if(isTRUE(keep.names) & (is.character(x[,row]) | is.character(x[,col]))) {
    rownames(mat) <- levels(as.factor(x[,row]))
    colnames(mat) <- levels(as.factor(x[,col]))
  }
  
  return(mat)
}




#' Title
#'
#' @param xy
#'
#' @return
#' @export
#'
#' @examples
point2polygon <- function (xy) {
  x <- c(xy[1]-1, xy[1])
  y <- c(xy[2]-1, xy[2])
  poly <- matrix(c(x[1],y[1],
                   x[1],y[2],
                   x[2],y[2],
                   x[2],y[1],
                   x[1],y[1]),
                 nrow = 5, byrow = TRUE)
  return(st_polygon(list(poly)))
}



#' Title
#'
#' @param Z 
#' @param Y 
#' @param va 
#' @param L 
#'
#' @return
#' @export
#'
#' @examples
calculate_x <- function(Z, Y, va, L) {
  if(!is.null(dim(Y))) Y <- apply(Y, 1, sum) # if Y is matrix
  if(missing(L)) {
    # check if mass balanced
    if(!all.equal(apply(Z, 1, sum) + Y, apply(Z, 2, sum) + va)) {
      stop("IO system is not mass balanced !!")
    }
    # calculate output
    x <- apply(Z, 1, sum) + Y  
  } else {
    x <- L %*% Y
  }
  return(x)
}


#' Title
#'
#' @param Z 
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
calculate_A <- function(Z, x) {
  # calculate A-matrix
  x_hat <- diag(1/x)
  x_hat[is.infinite(x_hat)] <- 0
  A <- Z %*% x_hat
  return(A)
}
#' Title
#'
#' @param A 
#'
#' @return
#' @export
#'
#' @examples
calculate_L <- function(A) {
  # calculate Leontief inverse
  I_mat <- diag(rep(1, nrow(A)))
  L <- solve(I_mat - A)
  return(L)
}
#' Title
#'
#' @param E 
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
calculate_S <- function(E, x) {
  # calculate Stressor matrix
  x_hat <- diag(1/x)
  x_hat[is.infinite(x_hat)] <- 0
  S <- E %*% x_hat
  return(S)
}

#' Title
#'
#' @param Z 
#' @param Y 
#' @param va 
#' @param E 
#'
#' @return
#' @export
#'
#' @examples
IO_creator <- function(Z, Y, va, E) {
  x <- calculate_x(Z, Y, va)
  A <- calculate_A(Z, x)
  S <- calculate_S(E, x)
  L <- calculate_L(A)
  return(list("A" = A, "L" = L, "S" = S))
}

#' Title
#'
#' @param S 
#' @param L 
#' @param Y 
#' @param B 
#' @param d 
#' @param f 
#' @param detailed 
#'
#' @return
#' @export
#'
#' @examples
IO_calculator <- function(S, L, Y, B, d, f, detailed = TRUE) {
  if(missing(Y)) Y <- (B %*% d) * as.numeric(f)
  x <- as.numeric(L %*% Y)
  if(detailed) B <- S %*% diag(x)
  else B <- S %*% x
  return(B)
}

#' Title
#'
#' @param n.industries 
#' @param n.emissions 
#' @param n.fdcats 
#' @param A 
#'
#' @return
#' @export
#'
#' @examples
create_random_IOtable <- function(n.industries, n.emissions, n.fdcats, A = FALSE) {
  x0 <- list("S" = matrix(runif(n.industries*n.emissions), n.emissions, n.industries), 
             "L" = matrix(runif(n.industries^2), n.industries, n.industries), 
             "Y" = matrix(runif(n.industries * n.fdcats), n.industries, n.fdcats))
  if(A) x0[["A"]] <- matrix(runif(n.industries^2), n.industries, n.industries)
  return(x0)
}

#' Title
#'
#' @param A_mat 
#' @param n 
#'
#' @return
#' @export
#'
#' @examples
leontief_series_expansion <- function(A_mat, n) {
  list <- vector(mode = "list", length = n)
  list[[1]] <- diag(1, nrow = nrow(A_mat), ncol = ncol(A_mat))
  for(i in 2:n) {
    list[[i]] <- list[[i-1]] %*% A_mat
  }
  return(list)
}


# 2. Sectoral Footprint Functions ----------------------------------------------
#' Title
#'
#' @param S_mat 
#' @param L_mat 
#' @param y_vec 
#' @param index 
#'
#' @return
#' @export
#'
#' @examples
.calc.sector.fp.direct <- function(S_mat, L_mat, y_vec, index) {
  if(missing(index)) {
    fp <- IO_calculator(S_mat, L_mat, y_vec) 
  } else {
    # only for 1 sector
    fp <- S_mat[,index] %*% (L_mat[index,] %*% y_vec)
  }
  return(fp)
}

#' Title
#'
#' @param S_mat 
#' @param L_mat 
#' @param x 
#' @param index 
#'
#' @return
#' @export
#'
#' @examples
.calc.sector.fp.indirect <- function(S_mat, L_mat, x, index) {
  diag(L_mat) <- 0 # all diagonal entries (e.g. input of cars into car industry) are already considered in the direct footprint calculations
  if(missing(index)) {
    fp <- S_mat %*% L_mat %*% diag(as.numeric(x))
  } else {
    fp <- S_mat %*% L_mat[,index] %*% x[index]  
  }
  return(fp)
}


#' Title
#'
#' @param L_mat 
#' @param S_mat 
#' @param y_vec 
#' @param index the index of the sector footprints are to be calculated. If missing results for ALL sectors are returned (higher computational expenses)
#' @param detailed shall footprints be returned split up by direct + indirect emissions?
#'
#' @return
#' @export
#'
#' @examples
calc_footprint_sector <- function(L_mat, S_mat, y_vec, index,  
                                  detailed = FALSE) {
  direct <- .calc.sector.fp.direct(S_mat = S_mat, L_mat = L_mat, 
                                   y_vec = y_vec, index = index)
  x <- calculate_x(Y = y_vec, L = L_mat)
  indirect <- .calc.sector.fp.indirect(S_mat = S_mat, L_mat = L_mat, 
                                       x = x, index = index)
  if(detailed) {
    fp <- list("direct" = direct, "indirect" = indirect)
  } else {
    fp <- direct + indirect  
  }
  return(fp)
}

#' Title
#'
#' @param n number of layers. recommendation >= 8 
#' @param L_mat 
#' @param A_mat 
#' @param y_vec 
#' @param S_mat 
#' @param index see ?calc_footprint_sector
#'
#' @return
#' @export
#'
#' @examples
SPA_footprint_sector <- function(n = 8, L_mat, A_mat, y_vec, S_mat, index) {
  L_series <- leontief_series_expansion(A_mat, n)
  fp <- vector(mode = "list", length = n)
  fp[[1]] <- .calc.sector.fp.direct(index = index, S_mat = S_mat, 
                                    L_mat = L_mat, y_vec = y_vec)
  
  if(missing(index)) {
    # total output
    x <- calculate_x(L = L_mat, Y = y_vec) %>% as.numeric
    for(i in 2:n) {
      fp[[i]] <- S_mat %*% L_series[[i]] %*% diag(x) 
    }
  } else {
    # output of sector i
    x <- L_mat[index,] %*% y_vec
    for(i in 2:n) {
      fp[[i]] <- S_mat %*% L_series[[i]][,index] %*% x
    }  
  }
  return(fp)
}