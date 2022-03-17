test_that(
  "theme_apa()"
  , {
    output_theme <- theme_apa()

    expect_is(output_theme, c("theme", "gg"))
  }
)
