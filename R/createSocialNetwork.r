#' This function create a social network for each participant
#' @param veritas_raw: veritas data for a all participants
#' @param group_scale: is the number of vertex generated from group are scaled
#'                     to the square root?
#'
#' @seealso [createSingleSocialNetwork()] which this function wraps
#'
#' @return list of igraph networks
createSocialNetworks <- function(veritas_raw, group_scale = TRUE) {


    # Print process start
    cli::cli_alert_info("Generating Social Networks")

    # Create all social network
    network_list <- lapply(veritas_raw, 
                           createSingleSocialNetwork, 
                           group_scale)

    # Print process start
    cli::cli_alert_success("Process done")

    # Return network object
    return(network_list)
}

#' This function create a social network for a single participant
#' @param veritas_pid: veritas data for a single participant
#' @param group_scale: is the number of vertex generated from group are scaled
#'                     to the square root?
#'
#' @seealso [createPeopleData()] and [createGroupData()] 
#'          which this function call
#'
#' @return A single igraph network
createSingleSocialNetwork <- function(veritas_pid, group_scale = TRUE){

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
    if(!is.null(veritas_pid$people)){

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
    g <- igraph::graph_from_data_frame(edges, directed = FALSE,
                                       vertices = nodes)

    # Remove multiedges in case of redundancy in relation table
    g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

    # Return network
    return(g)
}

#' @title Extract people data
#' Create a node table with n people
#' Create an edge table with relations between ego and people,
#' and in between people
#
#' @param veritas_pid: veritas data for a single participant
#'
#' @return list of nodes and edges tables
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
                                relation_type %in% 1,
                                c("node_1", "node_2"))
    } else {
        alter_relations <- NULL
    }

    # Return people data
    return(list(nodes = alter,
                edges = rbind(ego_relations, alter_relations)))
}

#' @title Extrac group data
#' Create a node table with n people proportional to group size
#' Create an edge table with people from group,
#' people belonging to group (in people data) and ego,
#' fully connected to each other
#
#' @param veritas_pid: veritas data for a single participant
#' @param group_scale: is the number of vertex generated from group
#'              are scaled to sqrt(group_size)?
#'
#' @return list of nodes and edges tables
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
        group_relations <- subset(veritas_pid$relations, relation_type %in% 2)
    }

    # Append vertex and edge data frames
    for (gid in veritas_pid$groups$group_id) {

        # Extract people in group
        ## Check if there is relations!
        if(!is.null(veritas_pid$relations)) {
            people_in_group <- subset(group_relations,
                                    node_2 %in% gid,
                                    "node_1", 
                                    drop = TRUE)
        } else {
            people_in_group <- NULL
        }

        # Calculate group size minus people in group
        group_size <- subset(veritas_pid$groups, group_id %in% gid,
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
            all_edges <- t(combn(c(pid, new_names, people_in_group), 2))
            colnames(all_edges) <- c("node_1", "node_2")

            # Append edges
            group_edges <- rbind(group_edges, all_edges)
        }
    }

    # Return group data
    return(list(nodes = group_nodes, edges = group_edges))
}
