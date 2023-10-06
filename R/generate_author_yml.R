#' generate_author_yml
#'
#' This function helps organize YAML author and affiliation fields such that authorship order can be changed without having to also update the order of affiliations.
#' @param researchers a list of named character vectors. Author information is stored in the name of the vector. Abbreviated affiliations are stored in the vector.
#' @param affiliations a list of named character strings. Abbreviated affiliations are stored in the names, the full affiliation is stored in the string.
#' @param corres_name A character string. The name of the corresponding author, must match the author details in the researcher argument identically.
#' @param corres_address A character string. The address of the corresponding author
#' @param corres_email A character string. The email of the corresponding author.
#' @export
#'
#' @examples
#'
#' library(papaja)
#'
#' generate_author_yml (
#'  researchers = list(
#'   "Emma J. Citizen" = c("example_hospital", "example_college"),
#'   "John H. Smith" = "example_college",
#'   "Kate C. Jones" = "example_hospital"
#'    ),
#'  affiliations = list(
#'    "example_hospital" = "Southern Example Hospital, NSW, Australia",
#'    "example_college" = "New Example College, VIC, Australia"
#'    ),
#'  corres_name = "Emma J. Citizen",
#'  corres_address = "123 Example Street, Epping, NSW 2121",
#'  corres_email = "jane@example.com"
#' )

generate_author_yml <- function(
           researchers
           , affiliations
           , corres_name
           , corres_address
           , corres_email
) {

    validate(affiliations, check_class = "list", check_NA = TRUE)
    validate(researchers, check_class = "list", check_NA = TRUE)
    validate(corres_name, check_class = "character", check_length = 1L)
    validate(corres_email, check_class = "character", check_length = 1L)

    if (any(!unlist(researchers) %in% names(affiliations)))
      stop("Some affiliation codes listed for researchers are not listed under affiliations")

    if(length(unique(names(researchers))) != length(names(researchers)))
      stop("All researcher names must be unique")

    if(length(unique(names(affiliations))) != length(names(affiliations)))
      stop("All affiliation object names must be unique")

    if(length(unique(unlist(affiliations))) != length(unlist(affiliations)))
      stop("All affiliations must be unique")

    if (!corres_name %in% names(researchers))
      stop("The corresponding author is not listed as a researcher")
    if(any(sapply(affiliations, length) != 1))
      stop("The length of each listed affiliation must be equal to 1")

    affil_order <- unique(unlist(researchers))
    affil_numeric <- seq_along(affil_order)
    names(affil_numeric) <- affil_order
    affil_content <- affiliations[affil_order]


    affils <- paste(
      glue::glue(
        '  - id            : "{affil_numeric}"
    institution   : "{affil_content}"',
    .trim = FALSE
      ),
    collapse = "\n"
    )

    authors <- sapply(seq_along(researchers), function(i) {
      i_affil <- affil_numeric[match(unlist(researchers[i]), names(affil_numeric))]
      i_affil <- paste(i_affil, collapse = ",")

      if (names(researchers)[i] == corres_name) {
        extra <- as.character(
          glue::glue(
            "    corresponding : yes
    address       : \"{corres_address}\"
    email         : \"{corres_email}\"\n",
    .trim = FALSE
          )
        )
      } else{
        extra <- ""
      }

      glue::glue("  - name          : \"{names(researchers)[i]}\"
    affiliation   : \"{i_affil}\"\n{extra}",
    .trim = FALSE)

    })

    authors <- paste(authors, collapse = "")

    yml <- glue::glue("author:
            {authors}

            affiliation:
            {affils}")

    yml

  }
