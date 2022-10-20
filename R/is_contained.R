

#' is.contained
#'
#' a function to check if a list is included in a predefined list
#'
#' @param vec1 A list
#' @param vec2 A list
#' @export
is.contained=function(vec1,vec2){
    x=vector(length = length(vec1))
    for (i in 1:length(vec1)) {
        x[i] = vec1[i] %in% vec2
        if(length(which(vec1[i] %in% vec2)) == 0) vec2 else
            vec2=vec2[-match(vec1[i], vec2)]
    }
    y=all(x==T)
    return(y)
}

