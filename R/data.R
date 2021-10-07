#' Locations data
#'
#' A dumy dataset containing the reported locations of 50 participants
#'
#' @format A data frame with 1140 rows and 4 variables:
#' \describe{
#'   \item{pid}{participants unique identifiers}
#'   \item{location_id}{location unique identifiers}
#'   \item{location_category}{location categories}
#'   \item{location_name}{location names}
#' }
"locations"

#' People data
#'
#' A dumy dataset containing the reported people of 50 participants
#'
#' @format A data frame with 190 rows and 4 variables:
#' \describe{
#'   \item{pid}{participants unique identifiers}
#'   \item{people_id}{people unique identifiers}
#'   \item{people_firstname}{First names}
#'   \item{people_relation}{relation categories}
#' }
"people"

#' Groups data
#'
#' A dumy dataset containing the reported groups of 50 participants
#'
#' @format A data frame with 68 rows and 4 variables:
#' \describe{
#'   \item{pid}{participants unique identifiers}
#'   \item{group_id}{group unique identifiers}
#'   \item{group_name}{group names}
#'   \item{group_size}{Number of people in the group}
#' }
"groups"

#' Relations data
#'
#' A dumy dataset containing the reported relations between people,
#' groups and locations for 50 participants
#'
#' @format A data frame with 626 rows and 4 variables:
#' \describe{
#'   \item{pid}{participants unique identifiers}
#'   \item{node_1}{node 1 unique identifiers}
#'   \item{relation_type}{Types of relations}
#'   \item{node_2}{node 2 unique identifiers}
#' }
"relations"