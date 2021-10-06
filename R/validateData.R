#' Run validation on veritas tables
#'
#' @param locations location table
#' @param people people table
#' @param groups groups table
#' @param relations relations table
#'
#' @seealso Run [comparePIDs()], [compareRelationIds()],
#' [duplicatedIds()], [duplicatedIds()], [modifiedGroupSize()],
#' [validateColumns()], [validateRelations()]
#'
#' @return List of validation information
#' @export
validateData <- function(locations, people, groups, relations) {

    # Create list
    veritas_data <- list(locations = locations,
                         people = people,
                         groups = groups,
                         relations = relations)

    # Create an empty list to store validation TRUE/FALSE
    validation <- list()

    # validate column names
    validation$columns <- validateColumns(veritas_data)

    # Stop if not all columns are validated
    if(!all(unlist(validation$columns))) {

        # Return partial validation
        out <- list(valide = FALSE,
                    details = validation)
        class(out) <- "partial.validation"
        return(out)
    }

    # Duplicated ids
    dupli_ids <- duplicatedIds(veritas_data)
    validation$dupli_ids <- all(dupli_ids == 0)

    # PIDs in locations
    pids_locations <- comparePIDs(veritas_data)
    validation$pids_locations <- all(pids_locations == 0)

    # Check relation table
    freq_relations <- compareRelationIds(veritas_data)
    validation$relations <- validateRelations(freq_relations$relative)

    # Group size == 0, 1 or smaller than number of people in it
    group_data <- modifiedGroupSize(veritas_data)
    validation$group_size <- all(group_data$size_no_ind > 0)

    # Create output
    out <- list(valide = all(unlist(validation)),
                details = validation,
                duplicated_ids = dupli_ids,
                pids_locations = pids_locations,
                freq_relations = freq_relations,
                group_size = group_data[c("pid", "group_id", "group_size",
                                          "nb_ind", "size_no_ind")])

    # Add S3 class
    class(out) <- "validation"

    # Return
    return(out)
}

#' Check if PIDs in people, groups and relations tables are found
#' in locations table
#'
#' @param veritas_data A list of locations, people, groups and 
#' relations table
#'
#' @return The number of PIDs NOT in locations table
comparePIDs <- function(veritas_data){

    # Check if pids are in location table
    lapply(
        veritas_data[c("people", "groups", "relations")],
        function(x){
            sum(!x$pid %in% veritas_data$locations$pid)
        })
}

#' Check that nodes in relation tables are in locations, people
#' and groups table
#'
#' @param veritas_data A list of locations, people, groups and 
#' relations table
#'
#' @return Two tables of absolute and relative frequencies
#' of nodes in locations, people and groups table
compareRelationIds <- function(veritas_data) {

    # Split by relation_type
    split_relations <- with(veritas_data,
        split(relations, f = factor(relations$relation_type)))

    # Check frequency of node types by columns
    absolute_freqs <- lapply(split_relations, function(x) {

        # Calculate frequencies
        freq <- lapply(x[c("node_1", "node_2")], function(y) {
            c(sum(y %in% veritas_data$locations$location_id),
              sum(y %in% veritas_data$people$people_id),
              sum(y %in% veritas_data$groups$group_id))
        })

        # Create table
        freq <- do.call(rbind, freq)

        # Change column names
        colnames(freq) <- c("location_id", "people_id", "group_id")

        # Return table
        return(freq)
    })

    # Calculate sums
    sums <- lapply(split_relations, nrow)

    # Calculate relative frequencies
    relative_freqs <- mapply(function(x, y) x / y,
        absolute_freqs, sums,
        SIMPLIFY = FALSE)

    # Return both object
    return(list(absolute = absolute_freqs, relative = relative_freqs))
}

#' Check if there is duplicated nodes in locations, people and groups
#'
#' @param veritas_data A list of locations, people, groups and 
#' relations table
#'
#' @return Number of duplicated nodes in each table 
duplicatedIds <- function(veritas_data){

    dupli_ids <- mapply(function(x, y) sum(duplicated(x$y)),
        veritas_data[c("locations", "people", "groups")],
        c("location_id", "people_id", "group_id"))

    names(dupli_ids) <- c("location_id", "people_id", "group_id")

    return(dupli_ids)
}

#' Calculate group sizes minus people in groups
#'
#' @param veritas_data A list of locations, people, groups and
#' relations table
#'
#' @return Modified group sizes
modifiedGroupSize <- function(veritas_data) {

    # Calculate number of individuals in groups
    group_data <- veritas_data$relations |>
        subset(veritas_data$relations$relation_type == 2) |>
        {\(x) split(x, factor(x$node_2)) }() |>
        vapply(nrow, numeric(1)) |>
        {\(x) data.frame(group_id = names(x), nb_ind = x)}() |>
        {\(x) merge(veritas_data$groups, x, by = "group_id", all.x = TRUE)}()

    # Change NAs for 0s
    group_data[is.na(group_data$nb_ind), "nb_ind"] <- 0

    # Calculate group size without individuals
    group_data$size_no_ind <- group_data$group_size - group_data$nb_ind

    # Return modified group_data
    return(group_data)
}

#' Check if there is the right column names in each table
#'
#' @param veritas_data A list of locations, people, groups and
#' relations table
#'
#' @return List of validated column names
validateColumns <- function(veritas_data){

    fun <- function(col_names, table) {
        out <- col_names %in% colnames(table)
        names(out) <- col_names
        out
    }

    # list object
    v_col <- list()

    # Locations
    v_col$locations <- fun(c("pid", "location_id"), veritas_data$locations)

    # people
    v_col$people <- fun(c("pid", "people_id"), veritas_data$people)

    # groups
    v_col$groups <- fun(c("pid", "group_id", "group_size"),
                        veritas_data$groups)

    # relations
    v_col$relations <- fun(c("pid", "node_1", "node_2", "relation_type"),
                        veritas_data$relations)

    # return validation
    return(v_col)
}

#' Validate that nodes in relation table are from the
#' right tables
#'
#' @param relative_freqs Relative frequencies of node ids
#' in locations, people, and groups tables.
#' Generated from [compareRelationIds()]
#'
#' @return Boolean value indicating if nodes are from the right table
validateRelations <- function(relative_freqs) {

    # Compare with expected values
    validation <- list()

    ## People relations
    validation$people <- all(relative_freqs$`1`[c(3,4)] %in% 1)

    ## Group relations
    validation$groups <- all(relative_freqs$`2`[c(3,6)] %in% 1)

    ## Location relations
    validation$locations <- all(c(relative_freqs$`3`[2],
          sum(relative_freqs$`3`[c(3, 5)])) %in% 1)

    # Return
    return(validation)
}