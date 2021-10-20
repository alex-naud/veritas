#' Clean problems associated with group data
#'
#' Added relations between people in a same groups, and
#'  between people and locations if these relations exist
#'  with the associated groups. Also removed group of size < 1
#'  after removing people individualy reported, but also part
#'  of the groups
#'
#' @param groups Groups Veritas table
#' @param relations Relations Veritas table
#'
#' @return A list of the modified group and relations data
#' @noRd
cleanGroupData <- function(groups, relations){

    # Extract group <-> people relations
    groups_people_relations <- relations %>%
        subset(relations$node_2 %in% groups$group_id &
               relations$relation_type == 2,
               drop = TRUE) %>%
        split(., factor(.$node_2))

    # find new relations in-between people of same groups
    new_relations_1 <- groups_people_relations %>%
        lapply(addPeopleRelations, relations) %>%
        do.call(rbind, .)

    # Add relations between people and locations related to groups
    new_relations_2 <- groups_people_relations %>%
        lapply(addPeopleLocationsRelations, relations) %>%
        do.call(rbind, .)

    # Find and remove groups with size < 1, and related relations
    reduced_data <- removeGroupsSize0(groups, relations)

    # Merge relations data
    new_relations <- rbind(reduced_data$relations,
                            new_relations_1,
                            new_relations_2)

    # Add attributes
    attr(new_relations, "added_people_people") <- nrow(new_relations_1)
    attr(new_relations, "added_people_locations") <- nrow(new_relations_2)
    attr(new_relations, "removed_group_size_0") <- nrow(relations) -
                                             nrow(reduced_data$relations)

    attr(reduced_data$groups, "removed_group_size_0") <-
        nrow(groups) - nrow(reduced_data$groups)

    # Remove row.names
    row.names(new_relations) <- NULL
    row.names(reduced_data$groups) <- NULL

    # Return
    return(list(groups = reduced_data$groups, relations = new_relations))
}

#' Inside function of [cleanGroupData()]
#'
#' Find people <-> people relations not present in relation table
#'
#' @param x A subset of the relation table providing
#'  who is part of a single group
#' @param relations Relations Veritas table
#'
#' @return New rows for the relation table
#' @noRd
addPeopleRelations <- function(x, relations){

    # Run only for groups with more than 1 separatedely reported person
    if(nrow(x) > 1){

        # Extract only people relations for the pid from relation table
        sub <- subset(relations, relations$pid == unique(x$pid) &
                                 relations$relation_type == 1)

        # Compute all relations for this single group
        rel <- utils::combn(x$node_1, 2)

        # Create output table
        out <- data.frame(matrix(nrow = 0, ncol = ncol(sub)))
        colnames(out) <- colnames(sub)

        # Add if they are not in relation table
        for(i in seq_len(ncol(rel))) {

            bool <- c(sub$node_1 == rel[1, i] & sub$node_2 == rel[2, i],
                      sub$node_1 == rel[2, i] & sub$node_2 == rel[1, i])

            if(sum(bool) == 0) {

                # Create a new row
                new_row <- sub[1, ]

                # Replace node_1 and node_2
                new_row$node_1 <- rel[1,i]
                new_row$node_2 <- rel[2,i]

                # Append
                out <- rbind(out, new_row)
            }
        }

        # Return new relations
        return(out)
    }
}

#' Inside function of [cleanGroupData()]
#'
#' Find people <-> locations relations not present in relation table
#'
#' @param x A subset of the relation table providing
#'  where a single group is seen
#' @param relations Relations Veritas table
#'
#' @return New rows for the relation table
addPeopleLocationsRelations <- function(x, relations) {

    # Extract the locations to which this group is connected
    loc <- relations %>%
        subset(relations$relation_type %in% 3 & 
               relations$node_1 %in% x$node_2,
               "node_2",
               drop = TRUE)

    # Extract people that are part of this group
    pl <- x$node_1

    # Extract only relations for the pid from relation table
    sub <- subset(relations, relations$pid == unique(x$pid) &
                             relations$relation_type == 3)

    # Check if relations between those people and locations exist
    rel <- expand.grid(pl, loc)

    # Create output table
    out <- data.frame(matrix(nrow = 0, ncol = ncol(sub)))
    colnames(out) <- colnames(sub)

    # Run over all relations
    for(i in seq_len(nrow(rel))) {

        # Check if a single relation exist in relation table
        bool <- sub$node_1 == rel[i, 1] & sub$node_2 == rel[i, 2]

        # Add if non existing
        if(sum(bool) == 0) {

            # User first row to keep information
            new_row <- sub[1, ]

            # Replace node_1 and node_2
            new_row$node_1 <- rel[i, 1]
            new_row$node_2 <- rel[i, 2]

            # Append
            out <- rbind(out, new_row)
        }
    }

    # # Return new relations
    return(out)
}

#' Inside function of [cleanGroupData()]
#'
#' Removed groups (and associated relations) which have
#'  a size < 1 (after removing individual who were define alone)
#'
#' @param groups Groups Veritas table
#' @param relations Relations Veritas table
#'
#' @return A list of the reduced groups and relations table
#' @noRd
removeGroupsSize0 <- function(groups, relations){

    # Calculate number of individuals in groups
    group_sizes <- relations %>%
        subset(relations$relation_type == 2) %>%
        split(., factor(.$node_2)) %>%
        vapply(nrow, numeric(1)) %>%
        data.frame(group_id = names(.), nb_ind = .) %>%
        merge(groups[, c("group_id", "group_size")], .,
              by = "group_id", all.x = TRUE)

    # Change NAs for 0s
    group_sizes[is.na(group_sizes$nb_ind), "nb_ind"] <- 0

    # Calculate group size without individuals
    group_sizes$size_no_ind <- group_sizes$group_size - group_sizes$nb_ind

    # Find those with size < 1
    gid <- group_sizes[group_sizes$size_no_ind < 1, "group_id"]

    # Remove groups
    reduced_groups <- subset(groups, !groups$group_id %in% gid)

    # Remove relations
    reduced_relations <- subset(
        relations,
        !(relations$node_1 %in% gid) & !(relations$node_2 %in% gid))

    # Return reduced groups and relations
    return(list(groups = reduced_groups, relations = reduced_relations))
}
