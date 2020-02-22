#' @title Get Intersection Set for Two Sets
#' @description Get intersection set for two sets, which can be numbers, characters, vectors even dataframe, matrix or list.
#' @param a see argument x in \code{\link[set]{toVector}}
#' @param b see argument x in \code{\link[set]{toVector}}
#' @return intersection set
#' @name and2
#' @rdname and2
#' @export
#'
#' @examples
#' A <- c("a","b","c")
#' B <- c("a","b","c","d")
#' C <- c("a","e","h")
#' A %and% B
#' A %and% B %and% C

"%and%" <- function(a,b){
    a=toVector(a)
    a=unique(a)
    b=toVector(b)
    b=unique(b)
    a[a %in% b]
}

#' @rdname and2
#' @export
"%a%" <- `%and%`
#' @rdname and2
#' @export
"%A%" <- `%and%`

#' @rdname and2
#' @export
"&" <- function(a,b) {
    check=tryCatch(expr = {base::`&`(a,b)},
                   error=function(e) 'thiswrong')
    if (check[1]=='thiswrong') {
        a %and% b
    }else{
        check
    }
}
