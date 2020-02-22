#' @title Get Union Set for Sets
#' @description Get union set for sets.
#' @param ... see argument x in \code{\link[set]{toVector}}
#'
#' @return union elements
#' @export
#'
#' @examples
#' A <- c("a","b","c")
#' B <- c("a","b","c","d")
#' C <- c("a","e","h")
#' or(A, B)
#' or(A, B, C)
or <- function(...){
    x<-list(...)
    if (length(x)==1) return(x)
    for (i in 1:length(x)) {
        x.i=toVector(x)
        x.ii=unique(x.i)
        if (i==1){
            res=x.ii
        }else{
            res=res %or% x.ii
        }
    }
    return(res)
}
