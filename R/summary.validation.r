summary.validation <- function(validation){

    # Define theming
    cli::cli_div(theme = list(span.success = list(color = "green"),
                              span.fail = list(color = "red")))

    # Header
    cli::cli_h1("Summary")

    # Function to print line
    cliPrintLine <- function(name, status){

        # Create
        if(status == TRUE){
            status_txt <- paste0("{.success pass}")
        } else {
            status_txt <- paste0("{.fail fail}")
        }

        # Print
        cli::cli_text(paste0("{.strong ", name, ": }", status_txt))
    }

    # Extract detail
    details <- validation$details

    # Print each line
    cliPrintLine("Column names", all(unlist(details$columns)))
    cliPrintLine("Duplicated IDs", details$dupli_ids)
    cliPrintLine("PIDs in locations", details$pids_locations)
    cliPrintLine("Relation table", all(unlist(details$relations)))
    cliPrintLine("Group size", details$group_size)
}

summary.partial.validation <- function(partial.validation){

    # Define theming
    cli::cli_div(theme = list(span.unknow = list(color = "yellow"),
                              span.fail = list(color = "red")))

    # Print
    cli::cli_text("{.strong Column names:} {.fail fail}")
    cli::cli_text("{.strong Duplicated IDs:} {.unknow unknown}")
    cli::cli_text("{.strong PIDs in locations:} {.unknow unknown}")
    cli::cli_text("{.strong Relation table:} {.unknow unknown}")
    cli::cli_text("{.strong Group size:} {.unknow unknown}")
}