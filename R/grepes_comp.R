#' @title Get Complementary Elements
#' @description Get complementary elements between data set a and b.
#'
#' @param a must be a subset of b, which can be numbers, characters, vectors, one dataframe, one matrix or one list.
#' @param b can be numbers, characters, vectors, one dataframe, one matrix or one list.
#' @return complementary elements in data set b
#' @export
#'
#' @examples
#' A <- c("a","b","c")
#' B <- c("a","b","c","d")
#' A %comp% B

"%comp%" <- function(a,b){
    a=toVector(a)
    a=unique(a)
    b=toVector(b)
    b=unique(b)
    if (a %sub% b){
        b[!(b %in% a)]
    }else{
        stop("a must be a subject of b")
    }
}
