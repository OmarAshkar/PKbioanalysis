setClass(
    "ChromResBase",
    slots = c(
        metadata = "data.frame",
        peaks = "data.frame",
        transitions = "data.frame",
        compounds = "data.frame",
        vendor = "character",
        pk_metadata = "list"
    )
)

#' Class ChromRes inherits from ChromResBase
#' @noRd
setClass(
    "ChromRes",
    contains = "ChromResBase",
    slots = c(
        runs = "list"
    )
)
