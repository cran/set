#' @title Get Elements only Existed in Dataset a
#' @description Get elements only existed in dataset a.
#' @param ... see argument x in \code{\link[set]{toVector}}
#' @return elements only existed in dataset a
#' @export
#'
#' @examples
#' A <- c("a","b","c")
#' B <- c("a","b","c","d")
#' not(B, A)
not <- function(...){
    x<-list(...)
    if (length(x)==1) return(x)
    for (i in 1:length(x)) {
        x.i=toVector(x[[i]])
        x.ii=unique(x.i)
        if (i==1){
            res=x.ii
        }else{
            res=res %not% x.ii
        }
    }
    return(res)
}

