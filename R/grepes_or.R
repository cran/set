#' @title Get Union Set for Two Sets
#' @description Get union set for two sets.
#' @param a see argument x in \code{\link[set]{toVector}}
#' @param b see argument x in \code{\link[set]{toVector}}
#' @return union set
#' @name or2
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

#' @rdname or2
#' @export
"%r%" <- `%or%`
#' @rdname or2
#' @export
"%R%" <- `%or%`

#' @rdname or2
#' @export
"|" <- function(a,b) {
    check=tryCatch(expr = {base::`|`(a,b)},
                   error=function(e) 'thiswrong')
    if (check[1]=='thiswrong') {
        a %or% b
    }else{
        check
    }
}
