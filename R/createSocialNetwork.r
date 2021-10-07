#' Constructor of social networks
#' 
#' This function return the set of all social networks from a Veritas dataset.
#'
#' Generates a set of igraph networks,
#' each of which includes a participant and the people linked to him
#' (either reported individually or from groups).
#'
#' @param veritas_split A list of veritas data for a all participants.
#'  See [transformData()].
#' @param group_scale Boolean. Declare if the number of vertex generated
#' from group are scaled to the square root.
#'
#' @seealso This function is a wrapper of [createSingleSocialNetwork()].
#'
#' @return A List of igraph networks.
#' @export
createSocialNetworks <- function(veritas_split, group_scale = TRUE) {

    # Check if object if of class veritas.split
    if(!inherits(veritas_split, "veritas.split")) {
        cli::cli_abort("Object is not of class veritas.split")
    }

    # Print process start
    cli::cli_alert_info("Generating Social Networks")

    # Create all social network
    network_list <- lapply(veritas_split,
                           createSingleSocialNetwork,
                           group_scale)

    # Print process start
    cli::cli_alert_success("Process done")

    # Add class
    class(network_list) <- c("veritas.social.network.all")

    # Return network object
    return(network_list)
}

#' Social network constructor
#'
#' This function create a social network for a single participant,
#'
#' Generate an igraph networks which include the participant and
#'  the people linked to him (either reported individually of from groups),
#'
#' @param veritas_pid Veritas data for a single participant.
#'  See [transformData()].
#' @param group_scale Boolean. Declare if the number of vertex generated
#' from group are scaled to the square root.
#'
#' @return A single igraph network
#' @export
createSingleSocialNetwork <- function(veritas_pid, group_scale = TRUE){

    # Check if object if of class veritas.pid
    if(!inherits(veritas_pid, "veritas.pid")) {
        cli::cli_abort("Object is not of class veritas.pid")
    }

    # Extract pid
    pid <- attr(veritas_pid, "pid")

    # Create empty data.frame for nodes and edges
    nodes <- matrix(nrow = 0, ncol = 2,
                    dimnames = list(NULL, c("name", "status"))) |>
                data.frame()

    edges <- matrix(nrow = 0, ncol = 2,
                    dimnames = list(NULL, c("node_1", "node_2"))) |>
                data.frame()

    # Ego
    nodes <- rbind(nodes, data.frame(name = pid, status = "ego"))

    # Check if there is people
    if(!is.null(veritas_pid$people)) {

        # Extract people data
        people_data <- createPeopleData(veritas_pid)

        # Merge people and group data
        nodes <- rbind(nodes, people_data$nodes)
        edges <- rbind(edges, people_data$edges)
    }

    # Check if there is groups
    if(!is.null(veritas_pid$groups)) {

        # Extract group data
        group_data <- createGroupData(veritas_pid, group_scale = group_scale)

        # Merge people and group data
        nodes <- rbind(nodes, group_data$nodes)
        edges <- rbind(edges, group_data$edges)
    }

    # Create network
    g <- igraph::graph_from_data_frame(edges, 
                                       directed = FALSE,
                                       vertices = nodes)

    # Remove multiedges in case of redundancy in relation table
    g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

    # Add class
    class(g) <- c("veritas.social.network", "igraph")

    # Return network
    return(g)
}

#' Extract people data
#' 
#' Extract nodes and edges from people data.
#'
#' This function extract people information and return two tables,
#' one with people, and the other with the relations between ego and people,
#' and in between people.
#
#' @param veritas_pid Veritas data for a single participant.
#'  See [transformData()].
#'
#' @return Two elements list comprising nodes and edges tables.
#'
#' @noRd
createPeopleData <- function(veritas_pid){

    # Extract pid and people_id
    pid <- attr(veritas_pid, "pid")
    people_id <- veritas_pid$people$people_id

    # Node
    alter <- data.frame(name = people_id, status = "alter")

    # Edges
    ego_relations <- data.frame(node_1 = pid, node_2 = people_id)

    ## Check if there is relations!
    if(!is.null(veritas_pid$relations)) {
        alter_relations <- subset(veritas_pid$relations,
                                veritas_pid$relations$relation_type %in% 1,
                                c("node_1", "node_2"))
    } else {
        alter_relations <- NULL
    }

    # Return people data
    return(list(nodes = alter,
                edges = rbind(ego_relations, alter_relations)))
}

#' Extract group data
#'
#' Extract nodes and edges from group data.
#'
#' This function extract people information and return two tables,
#' one with n people extracted from groups, and the other with the relations
#' between the participant and the newly generated people, and in between
#' all the people from same groups.
#
#' @param veritas_pid Veritas data for a single participant.
#'  See [transformData()].
#' @param group_scale Boolean. Declare if the number of vertex generated
#'  from group are scaled to the square root.
#'
#' @return list of nodes and edges tables
#'
#' @noRd
createGroupData <- function(veritas_pid, group_scale = TRUE) {

    # Create empty data frame
    group_nodes <- data.frame(matrix(nrow = 0, ncol = 2))
    group_edges <- data.frame(matrix(nrow = 0, ncol = 2))

    colnames(group_nodes) <- c("name", "status")
    colnames(group_edges) <- c("node_1", "node_2")

    # Extract pid
    pid <- attr(veritas_pid, "pid")

    # Extract group relations
    ## Check if there is relations!
    if(!is.null(veritas_pid$relations)) {
        group_relations <- subset(veritas_pid$relations,
                                  veritas_pid$relation_type %in% 2)
    }

    # Append vertex and edge data frames
    for (gid in veritas_pid$groups$group_id) {

        # Extract people in group
        ## Check if there is relations!
        if(!is.null(veritas_pid$relations)) {
            people_in_group <- subset(group_relations,
                                      group_relations$node_2 %in% gid,
                                      "node_1",
                                      drop = TRUE)
        } else {
            people_in_group <- NULL
        }

        # Calculate group size minus people in group
        group_size <- subset(veritas_pid$groups,
                             veritas_pid$groups$group_id %in% gid,
                            "group_size", drop = TRUE) - length(people_in_group)

        # Rescale if asked
        if (group_scale == TRUE) group_size <- ceiling(sqrt(group_size))

        # Create new edge names
        if (group_size == 0) {
            new_names <- NULL

        } else {
            new_names <- paste(rep(gid, group_size), seq(group_size), sep = "_")
        }

        # Append vertex data
        group_nodes <- rbind(group_nodes,
                            expand.grid(name = new_names, status = "group"))

        # Check if the group is not empty!
        if((length(new_names) + length(people_in_group)) > 0) {

            # calculate all edge combinations
            all_edges <- t(utils::combn(c(pid, new_names, people_in_group), 2))
            colnames(all_edges) <- c("node_1", "node_2")

            # Append edges
            group_edges <- rbind(group_edges, all_edges)
        }
    }

    # Return group data
    return(list(nodes = group_nodes, edges = group_edges))
}