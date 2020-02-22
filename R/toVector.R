#' Convet to Character
#' @description Convert dataframe, matrix, list, array or vector to character vector.
#'
#' @param x can be vector, dataframe, matrix, list, array
#'
#' @return a character vector
#' @export
#'
#' @examples
#' df=data.frame(a=c(1,2,3))
#' toVector(df)
toVector <- function(x){
    if (is.data.frame(x)){
        df=x
        for (i in 1:ncol(df)) {
            if (i==1) x=NULL
            if (is.factor(df[,i])) df[,i]=as.character(df[,i])
            x=c(x,df[,i])
        }
    }else if(is.matrix(x)){
        x=as.vector(x)
    }else if(is.list(x)){
        x=unlist(x)
    }else if(is.array(x)){
        x=as.vector(x)
    }
    names(x)=NULL
    as.character(x)
}
