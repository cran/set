#' @title Get Elements only Existed in Dataset a
#' @description Get elements only existed in dataset a.
#' @param a can be numbers, characters, vectors, one dataframe, one matrix or one list.
#' @param b can be numbers, characters, vectors, one dataframe, one matrix or one list.
#' @return elements only existed in dataset a
#' @export
#'
#' @examples
#' A <- c("a","b","c")
#' B <- c("a","b","c","d")
#' not(B, A)
not <- function(a,b){
    a=toVector(a)
    a=unique(a)
    b=toVector(b)
    b=unique(b)
    a[!(a %in% b)]
}
