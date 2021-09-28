print.validation.NAs <- function(validation_NAs){

    # Create header
    cli::cli_h1("NAs in numerical values")

    # Information
    cli::cli_alert_info("Looking for values < 0 or == 99")
    cli::cli_alert_info("These could be NAs that need to be transformed")

    # Empty space
    cli::cli_text()

    # Ok!
    if(validation_NAs$valide) {

        # Statement
        cli::cli_alert_success("We did not find any of those in numerical variables")
        
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
        index <- vapply(validation_NAs$details, 
                        function(x) any(unlist(x)),
                        logical(1))

        # Subset for table with NAs
        for(i in names(index)){
            
            if(index[i]) {

                # Table name
                # cli::cli_text(paste0("{.strong ", i, "}"))
                cli::cli_h2(i)

                # Extract column names and print
                cnames <- subset(names(validation_NAs$details[[i]]),
                                unlist(validation_NAs$details[[i]]))
                cli::cli_text(paste(cnames, collapse = " | "))
            }
        }
    }
}
