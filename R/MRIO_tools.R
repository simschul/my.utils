
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
IOvisualize <- function (mat, threshold, maxpoints = 10000, cex = "absolut",
                         attributes, ...)  {
  if (maxpoints > (ncol(mat) * nrow(mat))) {
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
  if (!missing(attributes) | (!is.null(colnames(mat) &
                                       !is.null(rownames(mat))))) {
    # either: attributes are given, or matrix has both row- and colnames
    if (missing(attributes)) {
      # matrix has row- and colnames -> take them as attributes
      attributes <- list()
      attributes[["row"]] <- data.table(rownames = rownames(mat))
      attributes[["col"]] <- data.table(colnames = colnames(mat))
    } else {
      if (!(c("row", "col") %in% names(attributes))) {
        warning("attributes needs to have both arguments col and row!")
      }
    }
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
                   zcol = "value", ...)
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

