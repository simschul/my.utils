#' Title
#'
#' @param mat 
#' @param threshold 
#' @param maxpoints 
#' @param cex 
#' @param attributes 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' 
IOvisualize <- function(mat, threshold, maxpoints = 1E4, 
                        cex = "absolut", attributes,  ...) {
  # TODO: include color scales to visulize < and > 0
  if (maxpoints > (ncol(mat) * nrow(mat))) {
    min_threshold <- 0
  } else {
    suppressWarnings(min_threshold <- mat %>%
                       abs %>% 
                       fsort(., decreasing = TRUE, na.last = TRUE) %>%
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
  if (!missing(attributes)) {
    if (sum(c("row", "col") %in% colnames(attributes)) != 2) {
      warning("attributes needs to have both arguments col and row!")
    }
    res <- merge(res, attributes[, -"col"], by = "row", 
                 suffixes = c(".row", ".col")) %>% 
      merge(., attributes[, -"row"], by = "col", 
            suffixes = c(".row", ".col"))
  }  
  res <- res %>% 
    .[, row := -row] %>% 
    st_as_sf(coords = c("col", "row"), 
             remove = FALSE) 
  res$row <- -res$row
  if (cex == "absolut") {
    res[["abs_value"]] <- abs(res$value)
    cex <- "abs_value"
  } else if (cex == "increasing") {
    cex <- "value"
  } else if (cex == "decreasing") {
    res[["dec_value"]] <- -(res$value)
    cex <- "dec_value"
  }
  mapview(res, alpha = 0.3, lwd = 0, cex = cex, 
          color = (viridis), zcol = "value", ...)
}

as.sparse.matrix <- function(mat) {
  mat <- mat %>% as.data.table %>% 
    .[, row := 1:.N] %>% 
    melt(id.vars = "row", na.rm = TRUE, 
         variable.name = "col") %>% 
    .[, col := substring(col, 2) %>% as.integer] %>% 
    .[]
  return(mat)
}
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