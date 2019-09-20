#' @title Get Intersection Set for Sets
#' @description Get intersection set for sets
#' @param ... can be number, character, vector, dataframe, matrix
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
            result=y[j] %and% result
        }
    }
    result=unique(result)
    return(result)
}
