
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

    input_path <- tempfile(fileext = ".Rmd")
    write(rmd, file = input_path)

    output_path <- tempfile(fileext = ".pdf")

    suppressWarnings(
      try(
        capture.output(
          rmarkdown::render(
            input = input_path,
            output_file = output_path,
            quiet = TRUE,
          )
        )
        , silent = TRUE
      )
    )

    expect_true(file.exists(output_path))

    # Clean up
    file.remove(input_path)
    file.remove(output_path)

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
