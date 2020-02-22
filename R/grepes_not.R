#' @title Get Elements only Existed in Dataset a
#' @description Get elements only existed in dataset a.
#' @param a see argument x in \code{\link[set]{toVector}}
#' @param b see argument x in \code{\link[set]{toVector}}
#' @return elements only existed in dataset a
#' @export
#' @name not2
#' @rdname not2
#' @examples
#' A <- c("a","b","c")
#' B <- c("a","b","c","d")
#' B %not% A

"%not%" <- function(a,b){
    a=toVector(a)
    a=unique(a)
    b=toVector(b)
    b=unique(b)
    a[!(a %in% b)]
}

#' @rdname not2
#' @export
"%n%" <- `%not%`
#' @rdname not2
#' @export
"%N%" <- `%not%`

#' @rdname not2
#' @export
"/" <- function(a,b) {
    check=tryCatch(expr = {base::`/`(a,b)},
                   error=function(e) 'thiswrong')
    if (check[1]=='thiswrong') {
        a %not% b
    }else{
        check
    }
}
