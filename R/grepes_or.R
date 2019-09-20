#' @title Get Union Set for Two Sets
#' @description Get union set for two sets.
#' @param a can be numbers, characters, vectors, one dataframe, one matrix or one list.
#' @param b can be numbers, characters, vectors, one dataframe, one matrix or one list.
#' @return union set
#' @export
#'
#' @examples
#' A <- c("a","b","c")
#' B <- c("a","b","c","d")
#' C <- c("a","e","h")
#' A %or% B
#' A %and% B %or% C

"%or%" <- function(a,b){
    a=toVector(a)
    a=unique(a)
    b=toVector(b)
    b=unique(b)
    unique(c(a,b))
}
