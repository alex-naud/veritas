messageColumns <- function(v_col) {

    # Define theming
    cli::cli_div(theme = list(span.success = list(color = "green"),
                              span.fail = list(color = "red")))

    # Create header
    cli::cli_h1("Column names")

    # Function to print Line
    cliPrintLine <- function(name){

        # Print table name
        cli::cli_text(paste0("{.strong ", name, "}"))

        # Extract column names
        col_names <- names(v_col[[name]])

        # Transform validation message
        msg <- lapply(col_names, function(x){
            ifelse(v_col[[name]][x],
                paste("{.success v}", x),
                paste("{.fail x}", x))
        }) |> unlist()

        # Print column validation
        cli::cli_text(paste(msg, collapse = " | "))
    }

    # print output for every table
    cliPrintLine("locations"); cli::cli_text()
    cliPrintLine("people"); cli::cli_text()
    cliPrintLine("groups"); cli::cli_text()
    cliPrintLine("relations")
}

messageCount <- function(list_data, header){

    # Define theming
    cli::cli_div(theme = list(span.success = list(color = "green"),
                              span.fail = list(color = "red")))

    # Create header
    cli::cli_h1(header)

    # Number by table
    for(x in names(list_data)) {

        if(list_data[[x]] == 0) {
            cli::cli_text("{x}: {.success {list_data[[x]]}}")
        } else {
            cli::cli_text("{x}: {.fail {list_data[[x]]}}")
        }
    }
}

messageGroups <- function(group_data){

    # Define theming
    cli::cli_div(theme = list(span.success = list(color = "green"),
                              span.fail = list(color = "red")))

    # Calculate number of group <= 0
    count <- sum(group_data$size_no_ind <= 0)

    # Header
    cli::cli_h1("Group size")

    # Print message
    if(count == 0) {
            cli::cli_text(
                "{.success {count}} groups have a size <= 0")
        } else {
            cli::cli_text(
                "{.fail {count}} groups have a size <= 0"
            )
        }
}

messageRelations <- function(relative_freqs){

    # Define theming
    cli::cli_div(theme = list(span.success = list(color = "green"),
                              span.fail = list(color = "red")))

    # Extract non zero frequencies
    relative_non_zero <- lapply(relative_freqs, function(x) {

        # Extract position
        index <- which(x > 0, arr.ind = TRUE)

        # Extract values
        out <- t(apply(index, 1, function(y) {
            c(id = colnames(x)[y["col"]],
              val = x[y["row"], y["col"]])
            }
        ))

        # Add mpde as first line
        out <- cbind(rownames(out), out)
        colnames(out)[1] <- "node"
        rownames(out) <- NULL

        # Return
        return(out)
    })

    # Header
    cli::cli_h1("Node id frequencies in relation table")

    # Function to print a single line
    cliPrintLine <- function(val, node, id){

        # Round value
        val <- round(val, 2)

        # Print
        if(val == 100){
                cli::cli_text("{.success {val}%} of {node} is in {id}")
            } else {
              cli::cli_text("{.fail {val}%} of {node} is in {id}")
            }
    }


    # Function to print multiple lines
    cliPrint <- function(tbl) {

        for(i in seq(nrow(tbl))){

            # Extract
            val <- as.numeric(tbl[i, "val"]) * 100
            node <- tbl[i, "node"]
            id <- tbl[i, "id"]

            # Print
            cliPrintLine(val, node, id)
        }
    }

    # Print People
    cli::cli_text("{.strong People relations}")
    cliPrint(relative_non_zero[[1]])
    cli::cli_text()

    # Print groups
    cli::cli_text("{.strong Group relations}")
    cliPrint(relative_non_zero[[2]])
    cli::cli_text()

    # Print locations
    cli::cli_text("{.strong Location relations}")

    ## Print location
    location_val <- relative_freqs[[3]][2, 1] * 100
    cliPrintLine(location_val, "node_2", "location_id")

    ## Sum people an group ids
    social_val <- sum(relative_freqs[[3]][1, 2:3]) * 100
    cliPrintLine(social_val, "node_1", "people_id or group_id")
}

Sprint.validation <- function(validation){

    # Column names information
    messageColumns(validation$details$columns)

    # Duplicated IDs
    messageCount(validation$duplicated_ids, "Duplicated ids")

    # PIDs in locations
    messageCount(validation$pids_locations, "PIDs not in locations table")

    # Relation IDs
    messageRelations(validation$freq_relations$relative)

    # Group size
    messageGroups(validation$group_size)
}

print.partial.validation <- function(partial.validation){

        # Columns output
        messageColumns(partial.validation$details$columns)

        # Partial error
        cli::cli_div(theme = list (.alert = list(color = "red")))
        cli::cli_alert("Validation stoped")
        cli::cli_end()
        cat("Either missing variables or wrong column names", "\n")
}
