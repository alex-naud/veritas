summary.veritas.raw <- function(veritas_raw){

    # Extract count
    nb_participants <- length(veritas_raw)
    nb_locations <- attr(veritas_raw, "nb_locations")
    nb_people <- attr(veritas_raw, "nb_people")
    nb_groups <- attr(veritas_raw, "nb_groups")
    nb_relations <- attr(veritas_raw, "nb_relations")

    # Print summary
    cli::cli_text("{nb_participants} participants")
    cli::cli_text("{nb_locations} locations")
    cli::cli_text("{nb_people} people")
    cli::cli_text("{nb_groups} groups")
    cli::cli_text("{nb_relations} relations")
}

summary.veritas.pid <- function(veritas_pid){

    # Extract pid
    pid <- veritas_pid$locations$pid

    # Extract count
    nb_locations <- length(unique(veritas_pid$locations$location_id))
    nb_people <- length(unique(veritas_pid$people$people_id))
    nb_groups <- length(unique(veritas_pid$groups$group_id))

    nb_relations_people <- veritas_pid$relations |>
                                subset(relation_type == 1) |>
                                nrow()

    nb_relations_groups <- veritas_pid$relations |>
                                subset(relation_type == 2) |>
                                nrow()
    
    nb_relations_locations <- veritas_pid$relations |>
                                subset(relation_type == 3) |>
                                nrow()
    
    # Print
    cli::cli_h2("nodes")
    cli::cli_text("{nb_locations} locations")
    cli::cli_text("{nb_people} people")
    cli::cli_text("{nb_groups} groups")

    cli::cli_h2("relations")
    cli::cli_text("{nb_relations_people} people <--> people")
    cli::cli_text("{nb_relations_groups} people <--> group")
    cli::cli_text("{nb_relations_locations} people/group <--> locations")
}