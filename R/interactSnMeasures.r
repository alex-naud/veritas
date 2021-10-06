#' Calculate social network measures for interact project
#'
#' @param veritas_data Transformed data generated from [transformData()]
#'
#' @return Table of social network measures
#'
#' @export
interactSnMeasures <- function(veritas_data){

    calculateMeasures <- function(veritas_pid){

        # Extract people table
        people <- veritas_pid$people

        # Return if people is null
        if(is.null(people))
        {
            out <- c(spouse = 0, family = 0, friend = 0, colleague = 0,
                     other = 0, diversity = 0, chat = 0, meet = 0,
                     chat_no_spouse = 0, meet_no_spouse = 0,
                     live_house = 0, live_building = 0, live_neighborhood = 0,
                     live_outside_neighborhood = 0, live_unknown = 0,
                     age_mean = NA, age_cv = NA,
                     socialize_size = 0, socialize_meet = 0, socialize_chat = 0,
                     important_size = 0, important_meet = 0, important_chat = 0,
                     not_close_size = 0, not_close_meet = 0, not_close_chat = 0)

            out <- t(as.matrix(out))

            return(out)
        }

        #== Network size by relation type
        size <- c(spouse = sum(people$people_relation %in% 1),
                  family = sum(people$people_relation %in% c(2, 3, 4)),
                  friend = sum(people$people_relation %in% 5),
                  colleague = sum(people$people_relation %in% 6),
                  other = sum(people$people_relation %in% c(7, 99)))

        #== Calculate Shanon-Weber entropy
        ## Remove spouse as it does not add up like other relations
        ## (not living in a polygamous culture)
        size_sub <- size[names(size) != "spouse"]

        # Calculate probability distribution
        size_prob <- size_sub / sum(size_sub)

        # Calculate p * ln(p)
        entropy <- size_prob * log2(size_prob)

        # Replace NaN by 0 because limit of p*ln(p) when p-> 0 = 0
        entropy[is.na(entropy)] <- 0

        # Calculate diversity
        diversity <- -sum(entropy)

        #== Frequency of interactions
        ## With the spouse
        freq_interactions <- c(chat = sum(people$people_chat),
                               meet = sum(people$people_meet))

        ## Without the spouse
        people_sub <- people[people$people_relation != 1,]
        freq_no_spouse <- c(chat_no_spouse = sum(people_sub$people_chat),
                         meet_no_spouse = sum(people_sub$people_meet))

        #== Where people live
        living <- c(live_house = sum(people$people_where %in% 1),
                    live_building = sum(people$people_where %in% 2),
                    live_neighborhood = sum(people$people_where %in% 3),
                    live_outside_neighborhood = sum(people$people_where %in% 4),
                    live_unknown = sum(people$people_where %in% 5))

        #== Age
        age <- c(age_mean = mean(people$people_age, na.rm = TRUE),
                 age_cv = stats::sd(people$people_age, na.rm = TRUE) /
                          mean(people$people_age, na.rm = TRUE))

        #== Size and frequency by support, companionship and
        #   emotional proximity

        socialize_people <- subset(people, socialize == TRUE)
        important_people <- subset(people, important == TRUE)
        not_close_people <- subset(people, not_close == TRUE)

        socialize <- c(socialize_size = nrow(socialize_people),
                       socialize_meet = sum(socialize_people$people_meet),
                       socialize_chat = sum(socialize_people$people_chat))

        important <- c(important_size = nrow(important_people),
                       important_meet = sum(important_people$people_meet),
                       important_chat = sum(important_people$people_chat))

        not_close <- c(not_close_size = nrow(not_close_people),
                       not_close_meet = sum(not_close_people$people_meet),
                       not_close_chat = sum(not_close_people$people_chat))

        # Return
        return(cbind(t(size), diversity, t(freq_interactions),
        t(freq_no_spouse), t(living), t(age), t(socialize),
        t(important), t(not_close)))
    }

    # Create output table
    out <- do.call(rbind, lapply(veritas_data, calculateMeasures))

    # transform to data.frame and add pid as column
    out <- data.frame(out)
    out <- cbind(data.frame(pid = names(veritas_data)), out)

    # Return
    return(out)

}