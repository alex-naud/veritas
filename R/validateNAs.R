#' Validate NAs in numerical variables
#'
#' Check for negative vlaues or 99 in numerical variables
#'
#' This step was added as non standard NAs were found in Veritas data
#'  of the InterAct project. It is run by [transformData()], but only
#'  generate a `warning` if non standard NAs are found.
#'
#' @param locations Locations Veritas table
#' @param people People Veritas table
#' @param groups Groups Veritas table
#' @param relations Relations Veritas table
#'
#' @return An object of class `validation.NAs`, which is a
#' list comprising `valide`, a boolean indicating wether there potentialy
#' is non standard NAs, and `details`, the number of non standard NAs
#' by variables in each table.
#'
#' @export
validateNAs <- function(locations, people, groups, relations){

    # Create list
    veritas_data <- list(locations = locations,
                         people = people,
                         groups = groups,
                         relations = relations)

    # Find numerical values < 0 or == 99
    details <- lapply(veritas_data, function(x){

        # Which variables are numeric
        numeric_index <- vapply(x, is.numeric, logical(1))

        # Remove ids
        numeric_index[grepl("(p|_)id$", names(numeric_index))] <- FALSE

        subset(x, select = numeric_index) %>%
            lapply(function(x) any(stats::na.exclude(x) < 0 | 
                                   stats::na.exclude(x) %in% 99))

        })

    # Check if potential non standard NAs
    valide <- !any(unlist(details))

    # Create output
    out <- list(valide = valide,
                details = details)

    # Add S3 class
    class(out) <- "validation.NAs"

    # Return
    return(out)
}
