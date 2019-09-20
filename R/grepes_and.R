#' @title Get Intersection Set for Two Sets
#' @description Get intersection set for two sets, which can be numbers, characters, vectors even dataframe, matrix or list.
#' @param a can be numbers, characters, vectors, one dataframe, one matrix or one list.
#' @param b can be numbers, characters, vectors, one dataframe, one matrix or one list.
#' @return intersection set
#' @export
#'
#' @examples
#' A <- c("a","b","c")
#' B <- c("a","b","c","d")
#' C <- c("a","e","h")
#' A %and% B
#' A %and% B %and% C

"%and%" <- function(a,b){
    a=toVector(a)
    a=unique(a)
    b=toVector(b)
    b=unique(b)
    COMMON(a,b)
}
