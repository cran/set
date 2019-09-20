#' @title Get Union Set for Sets
#' @description Get union set for sets.
#' @param ... can be number, character, vector, dataframe, matrix
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
    for (i in length(x):1) {
        if (i==length(x)){
            y=list(unique(toVector(x[i])))
        }else{
            y=c(y,list(unique(toVector(x[i]))))
        }
    }
    for (j in 1:length(y)) {
        if (j==1){
            result=y[j]
        }else{
            result=y[j] %or% result
        }
    }
    result=unique(result)
    return(result)
}
