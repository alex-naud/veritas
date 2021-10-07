#! Add escape character to add in library
#' Plot social networks
#'
#' Plot successively all social networks generated with [createSocialNetworks()]
#' Navigate using left right arrows
#'
#' Method of the [plot()] generic function that run on object
#' of class `veritas.social.network.all`.
#'
#' @param networks An object of class `veritas.social.network.all`
#' @param groups Boolean. Define if groups are displayed
#' @param labels Boolean. Define if labels are displayed
#'
#' @return NULL
#'
#' @seealso This function is a wrapper of [plot.veritas.social.network()]
#'
#' @noRd
plot.veritas.social.network.all <- function(networks,
                                           groups = TRUE,
                                           labels = FALSE) {


  # Extract PIDs
  pids <- names(networks)

  # Set counter
  n <- length(pids)
  i <- 1

  while (i <= n | i > 0) {

    # Extrac id
    pid <- pids[i]

    # Extract graph
    g <- networks[[pid]]

    # Plot Network
    plot.veritas.social.network(g, groups = groups, labels = labels)

    # Print counter
    print(paste0(i, " on ", n))

    # Keypress action
    #TODO Add escape character
    key <- readkeygraph("[press left of right, or e to escape]")
    if (key == "Right") i <- i + 1
    if (key == "Left") i <- i - 1
  }

  return(NULL)
}

#' Plot social network
#'
#' Plot a social networks generated with [createSocialNetworks()]
#'
#' Method of the [plot()] generic function that run on object
#' of class `veritas.social.network`.
#'
#' @param g An object of class `veritas.social.network`
#' @param groups Boolean. Define if groups are displayed
#' @param labels Boolean. Define if labels are displayed
#'
#' @return NULL
#'
#' @export
plot.veritas.social.network <- function(g, groups = TRUE, labels = FALSE) {


    # Remove group if needed
    if (groups == FALSE) {
        g <- igraph::induced.subgraph(g, igraph::V(g)$status != "group")
    }

    # Extract pid
    pid <- igraph::V(g)$name[igraph::V(g)$status == "ego"]

    # Load vertex attributes to define colors
    colors <- as.character(igraph::V(g)$status)

    # Define vertices colors
    colors[colors %in% "ego"] <- "#fbb4ae"
    colors[colors %in% "alter"] <- "#ba8bd4"
    colors[colors %in% "group"] <- "#b3cde3"

    # Plot
    if (labels == TRUE) igraph::plot.igraph(g, vertex.color = colors)
    if (labels == FALSE) igraph::plot.igraph(g, vertex.color = colors,
                                             vertex.label = NA)

    # Add id
    graphics::title(main = pid)
}

#' Read keypress
#'
#' This function read pressed key in a graphical interfaces
#'
#' @param prompt Displayed on top of graphic device
#'
#' @return The pressed key
#'
#' @noRd
readkeygraph <- function(prompt) {

    # Prompt and return pressed key
    grDevices::getGraphicsEvent(
        prompt = prompt,
        onMouseDown = NULL, onMouseMove = NULL,
        onMouseUp = NULL, onKeybd = onKeybd,
        consolePrompt = "[click on graph then follow top prompt to continue]"
    )

    # Wait 0.01 sec
    Sys.sleep(0.01)

    # Return pressed key
    return(key)
}

#' Return key in parent environment
#'
#' Function use inside readkeygraph.
#' Allow to return the keypressed in environment of the function.
#' 
#' @param key Thre pressed key
#'
#' @return NULL
#'
#' @noRd
onKeybd <- function(key) {
  # Function use inside readkeygraph
  # Allow to return the keypressed in environment of the function
  key <<- key
}