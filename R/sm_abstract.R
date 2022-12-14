

.SurvivalModel <- setClass(
    Class = "SurvivalModel",
    slots = list(
        "stan" = "StanModule"
    )
)


#' @export
SurvivalModel <- function(stan = StanModule(), ...) {
    base_stan <- paste0(read_stan("base/survival.stan"), collapse = "\n")
    stan_full <- jinjar::render(
        .x = base_stan,
        stan = add_missing_stan_blocks(as.list(stan))
    )
    .SurvivalModel(stan = StanModule(stan_full), ...)
}


#' @export
setMethod(
    f = "as.list",
    signature = c("SurvivalModel"),
    definition = function(x) {
        as.list(x@stan)
    }
)

