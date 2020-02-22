#' @title Judge Subset
#' @description Whether data set a is a subject of data set A.
#' @param a see argument x in \code{\link[set]{toVector}}
#' @param A see argument x in \code{\link[set]{toVector}}
#' @return logical result.
#' @export
#'
#' @examples
#' a <- c("a","b","c")
#' A <- c("a","b","c","d")
#' is.sub(a,A)

is.sub <- function(a,A){
    a=toVector(a)
    a=unique(a)
    A=toVector(A)
    A=unique(A)
    all(a %in% A)
}
