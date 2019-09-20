COMMON <- function(...){
    x=list(...)
    for (i in 1:length(x)) {
        if (i==1){
            common = unlist(x[i])
        }else{
            common=unlist(x[i])[unique(unlist(common %==% unlist(x[i])))]
        }
    }
    common
}
