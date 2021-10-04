socialNetworkMeasures <- function(social_networks){

    # Create a function to calculate measure on a single network
    calculateMeasures <- function(g){

        # Degree
        people_degree <- sum(igraph::V(g)$status %in% "alter")
        group_degree <- sum(igraph::V(g)$status %in% "group")
        all_degree <- people_degree + group_degree

        # Clustering
        cc <- igraph::induced.subgraph(g, igraph::V(g)$status != "ego") |>
                    igraph::graph.density()

        # Simmelian brokerage
        sb <- simmelianBrokerage(g)

        # Return
        c(people_degree = people_degree, group_degree = group_degree,
          degree = all_degree, clustering = cc, simmelian = sb)
    }

    # Run on all social networks
    out <- do.call(rbind, lapply(social_networks, calculateMeasures))

    # transform to data.frame and add pid as column
    out <- data.frame(out)
    out <- cbind(data.frame(pid = row.names(out)), out)
    row.names(out) <- NULL

    # Return
    return(out)
}

simmelianBrokerage <- function(g){
# Adapted from a code by Christopher G. Watson; cgwatson@bu.edu
# Retrieve from https://pastebin.com/XqkEYtJS
# Calculate simmelian brokerage for the ego of a social network
#
# g: igraph network object


  # Return NA if there is only less than two alter
  if(igraph::vcount(g) < 2) {
    return(NA)

    # Calculate local eff
  } else{

    # Remove ego
    g_sub <- igraph::induced.subgraph(g, igraph::V(g)$status != "ego")

    # Ego degree
    degree <- igraph::vcount(g_sub)

    # Calculate shortest path
    paths <- igraph::shortest.paths(g_sub, weights = NA)
    paths <- paths[upper.tri(paths)]

    # Calculate local efficiency
    local_eff <- 2 / degree / (degree - 1) * sum(1 / paths[paths != 0])

    # Calculate simmelian
    return(degree - (degree - 1) * local_eff)

  }
}