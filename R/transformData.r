transformData <- function(locations, people, groups, relations){

    # Process started
    cli::cli_alert_info("Process started")

    # Run validation
    validation <- validateData(locations, people, groups, relations)
    validation_NAs <- validateNAs(locations, people, groups, relations)

    # Stop if not valide
    if(!validation$valide) {

        cli::cli_abort(c(
            "Data is not valide",
            "Run {.fn validateData} for more information")
        )
    }

    # Warn if NAs
    if(!validation_NAs$valide) {

        cli::cli_alert_warning("We found potential non standard NAs")
        cli::cli_alert_warning("Run {.fn validateNAs} for more info")
    }

    # Extract data by PIDs
    split_data <- lapply(list(locations, people, groups, relations), 
                         function(x) split(x, factor(x$pid)))
    names(split_data) <- c("locations", "people", "groups", "relations")

    # Extract PIDs
    pids <- names(split_data$locations)

    # function to add empty object
    addEmpty <- function(split_df, pids){

        # Find missing pids
        missings <- pids[!pids %in% names(split_df)]

        # Return if no missings
        if(length(missings) == 0){

            # Reorder and return
            split_df[order(names(split_df))]
            return(split_df)
        }

        # Create list of NULL
        empty_list <- vector("list", length = length(missings))
        names(empty_list) <- missings

        # Append
        new <- c(split_df, empty_list)

        # Reorder
        new <- new[order(names(new))]

        # Return
        return(new)
    }

    # Append empty to all
    split_data <- lapply(split_data, addEmpty, pids)

    # Create list by PIDs
    out <- lapply(pids, function(x){
                            list(locations = split_data[["locations"]][[x]],
                                 people = split_data[["people"]][[x]],
                                 groups = split_data[["groups"]][[x]],
                                 relations = split_data[["relations"]][[x]])
    })

    # Add pid names
    names(out) <- pids

    # Add S3 class
    class(out) <- "veritas.raw"
    for (i in seq_along(out)) class(out[[i]]) <- 'veritas.pid'

    # Add summary information as attributes
    attr(out, "nb_locations") <- nrow(locations)
    attr(out, "nb_people") <- nrow(people)
    attr(out, "nb_groups") <- nrow(groups)
    attr(out, "nb_relations") <- nrow(relations)

    # Process complete
    cli::cli_alert_success("Process completed")

    # Return
    return(out)
}
