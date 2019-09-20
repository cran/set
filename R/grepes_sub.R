#' @title Subset
#' @description Whether data set a is a subject of data set A.
#' @param a subset, which can be numbers, characters, vectors, one dataframe, one matrix or one list.
#' @param A data set, which can be numbers, characters, vectors, one dataframe, one matrix or one list.
#' @return logical result.
#' @export
#'
#' @examples
#' A <- c("a","b","c")
#' B <- c("a","b","c","d")
#' A %sub% B

"%sub%" <- function(a,A){
    a=toVector(a)
    a=unique(a)
    A=toVector(A)
    A=unique(A)
    all(a %in% A)
}
