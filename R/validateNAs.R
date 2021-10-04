#' Check if there is numerical NAs
#' 
#' Check for negative values or 99s in numerical variables
#' 
#' @param locations location table
#' @param people people table
#' @param groups groups table
#' @param relations relations table
#' 
#' @return list of validation information. 
#' Valide is a boolean indicating wether there potentialy is
#' non standard NAs. Details is the number of non standard NAs by
#' variables in each table.
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

        subset(x, select = numeric_index) |>
            lapply(function(x) any(na.exclude(x) < 0 | na.exclude(x) %in% 99))

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
