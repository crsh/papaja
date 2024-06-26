
author_yml <- generate_author_yml(
  researchers = list(
    "James H. Conigrave" = "IPPE",
    "Emma L. Bradshaw" = "IPPE",
    "Michael Noetel" = "UQ"
  ),
  affiliations = list("IPPE" = "Institute for Postitive Psychology and Education",
                      "UQ" = "University of Queensland"),
  corres_email = "james@conigrave.com",
  corres_address = "33 Berry Street, North Sydney, NSW, Australia",
  corres_name = "James H. Conigrave"
)

test_that(
  "generate_author_yml output knits"
  , {
    skip_on_cran()

    # Get skeleton file
    rmd <- readLines(
      system.file(
        "rmarkdown", "templates", "apa6", "skeleton",
        "skeleton.Rmd"
       , package = "papaja")
    )

    # Remove author and affiliation information
    author_start <- which(grepl("author:\\s*",rmd))
    affiliation_end <- which(grepl("authornote: \\|", rmd)) - 1

    rmd <- rmd[ - (author_start : affiliation_end)]

    position_to_insert <- which(grepl("authornote: \\|", rmd)) - 1

    rmd <- c(rmd[1:position_to_insert]
             , author_yml
             , "",
             rmd[(position_to_insert + 1):length(rmd)]
             , ""
            )

    rmd <- paste(rmd, collapse = "\n")

    input_file <- tempfile(fileext = ".Rmd")
    write(rmd, file = input_file)

    output_file <- gsub(input_file, pattern = "\\.Rmd$", replacement = ".pdf")

    suppressWarnings(
      try(
        capture.output(
          rmarkdown::render(
            input = input_file,
            quiet = TRUE
          )
        )
        , silent = TRUE
      )
    )

    expect_true(file.exists(output_file))

    # Clean up
    file.remove(input_file)
    file.remove(output_file)

  }
)

test_that("generate_author_yml simplifies affiliations", {
  yaml_content <- yaml::yaml.load(author_yml)

  testthat::expect_length(yaml_content$author, 3)
  testthat::expect_length(yaml_content$affiliation, 2)

})

test_that("generate_author_yml enforces unique authors/affiliations", {

  expect_error(
    generate_author_yml(
      researchers = list(
        "James H. Conigrave" = "IPPE",
        "James H. Conigrave" = "IPPE",
        "Michael Noetel" = "UQ"
      ),
      affiliations = list("IPPE" = "Institute for Postitive Psychology and Education",
                          "UQ" = "University of Queensland"),
      corres_email = "james@conigrave.com",
      corres_address = "33 Berry Street, North Sydney, NSW, Australia",
      corres_name = "James H. Conigrave"
    )
  )

  expect_error(
    generate_author_yml(
      researchers = list(
        "James H. Conigrave" = "IPPE",
        "Emma L. Bradshaw" = "IPPE",
        "Michael Noetel" = "UQ"
      ),
      affiliations = list("IPPE" = "Institute for Postitive Psychology and Education",
                          "IPPE" = "Fail",
                          "UQ" = "University of Queensland"),
      corres_email = "james@conigrave.com",
      corres_address = "33 Berry Street, North Sydney, NSW, Australia",
      corres_name = "James H. Conigrave"
    )
  )

  expect_error(
    generate_author_yml(
      researchers = list(
        "James H. Conigrave" = "IPPE",
        "Emma L. Bradshaw" = "IPPE",
        "Michael Noetel" = "UQ"
      ),
      affiliations = list(
        "IPPE" = "Institute for Postitive Psychology and Education",
        "Fail" = "Institute for Postitive Psychology and Education",
        "UQ" = "University of Queensland"
      ),
      corres_email = "james@conigrave.com",
      corres_address = "33 Berry Street, North Sydney, NSW, Australia",
      corres_name = "James H. Conigrave"
    )
  )

})

test_that("generate_author_yml preserves affiliation order", {

  text <- generate_author_yml(
    researchers = list(
      "James H. Conigrave" = c("IPPE", "UQ"),
      "Michael Noetel" = c("UQ", "IPPE")
    ),
    affiliations = list("IPPE" = "Institute for Postitive Psychology and Education",
                        "UQ" = "University of Queensland"),
    corres_email = "james@conigrave.com",
    corres_address = "33 Berry Street, North Sydney, NSW, Australia",
    corres_name = "James H. Conigrave"
  )

  matches <- gregexpr("(?<=affiliation   : \")\\d+(,\\d+)*", text, perl=TRUE)

  all_affiliations <- regmatches(text, matches)[[1]]

  # Get the second instance
  second_affiliation <- all_affiliations[2]
  expect_equal("2,1", second_affiliation)

})
