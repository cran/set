#' @title Get Intersection Set for Sets
#' @description Get intersection set for sets.
#' @param ... see argument x in \code{\link[set]{toVector}}
#'
#' @return intersection elements
#' @export
#'
#' @examples
#' A <- c("a","b","c")
#' B <- c("a","b","c","d")
#' C <- c("a","e","h")
#' and(A, B)
#' and(A, B, C)
and <- function(...){
    x<-list(...)
    if (length(x)==1) return(x)
    for (i in 1:length(x)) {
        x.i=toVector(x)
        x.ii=unique(x.i)
        if (i==1){
            res=x.ii
        }else{
            res=res %and% x.ii
        }
    }
    return(res)
}


