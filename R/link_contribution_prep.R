#' @param module
#' @export

link_contribution_prep <- function(module){

    step_1 <- gsub(".*\\(|\\).*", "", module@functions[1])
    step_2 <- unlist(strsplit(step_1, ","))
    step_3 <- strsplit(step_2, " ")
    res <- list()
    for(i in i:length(step_3)){
        res[i]<- step_3[[i]][length(step_3[[i]])]
    }
    tmp <- paste(unlist(res), collapse = ",")
    paste0( "(", tmp, ")")
}
