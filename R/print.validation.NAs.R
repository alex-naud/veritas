#' Print NAs validation
#' 
#' @param x Object returned by [validateNAs()]
#' @param ... Only used to remain consistent with plot generic function
#' @return NULL | Print in CLI information on NA validation
#' @export
print.validation.NAs <- function(x, ...){

    # Create header
    cli::cli_h1("NAs in numerical values")

    # Information
    cli::cli_alert_info("Looking for values < 0 or == 99")
    cli::cli_alert_info("These could be NAs that need to be transformed")

    # Empty space
    cli::cli_text()

    # Ok!
    if(x$valide) {

        # Statement
        cli::cli_alert_success(paste("We did not find any of those",
                                     "in numerical variables"))

        # String variables warning
        cli::cli_alert("{.emph Check manually in string variables!}")

    } else {

        # Statement
        cli::cli_alert_warning(
            "We found potential numerical NAs in the following columns")

        # String variables warning
        cli::cli_alert_warning("{.emph Check manually in string columns!}")

        # Empty space
        cli::cli_text()

        # Extract tables with numerical NAs
        index <- vapply(x$details, 
                        function(x) any(unlist(x)),
                        logical(1))

        # Subset for table with NAs
        for(i in names(index)){

            if(index[i]) {

                # Table name
                cli::cli_h2(i)

                # Extract column names and print
                cnames <- subset(names(x$details[[i]]),
                                unlist(x$details[[i]]))
                cli::cli_text(paste(cnames, collapse = " | "))
            }
        }
    }

    return(NULL)
}