context("Snapshot tests for apa_*plot functions")

skip_on_cran()

set.seed(43L)
palette("R4")

# suppress printing
registerS3method(genname = "print", class = "noprint", method = function(x, ...) invisible())


example_data <- expand.grid(
  A = paste0("a", 1:5),
  B = paste0("b", 1:3),
  C = paste0("c", 1:2),
  D = paste0("d", 1:4),
  sid = 1:12
)
example_data$first_dv <- rnorm(12)[example_data$sid] +
  rnorm(5)[example_data$A] +
  rnorm(3)[example_data$B] +
  rnorm(2)[example_data$C] +
  rnorm(4)[example_data$D] +
  rnorm(nrow(example_data))

variable_labels(example_data) <- list(
  A = expression("Factor"~bold(A))
  , B = expression("Factor"~bold(B))
  , C = expression("Factor"~bold(C))
  , D = expression("Factor"~bold(D))#
  , first_dv = expression("Dependent variable"~italic(Y)[1])
)

test_that(
  "Legacy interface, apa_beeplot()", {
    vdiffr::expect_doppelganger(
      "Legacy interface, apa_beeplot()",
      apa_beeplot(
        data = example_data
        , dv = "first_dv"
        , id = "sid"
        , factors = c("A", "B")
        , dispersion = wsci
        , level = .99
      )
    )
  }
)


test_that(
  "Legacy interface, apa_barplot()", {
    vdiffr::expect_doppelganger(
      "Legacy interface, apa_barplot()",
      apa_barplot(
        data = example_data
        , dv = "first_dv"
        , factors = c("A", "B", "D")
        , id = "sid"
        , args_rect = list(
          col = 3:5
          , border = 1:3
        )
        , args_legend = list(
          legend = letters[2:4]
        )
      )
    )
  }
)

test_that(
  "Legacy interface, apa_lineplot()", {
    vdiffr::expect_doppelganger(
      "Legacy interface, apa_lineplot()",
      apa_lineplot(
        data = example_data
        , dv = "first_dv"
        , factors = c("A", "B", "D")
        , id = "sid"
        , args_lines = list(
          lwd = 3:1
        )
        , args_points = list(
          cex = 3
        )
        , args_x_axis = list(
          cex.axis = 2
        )
      )
    )
  }
)

test_that(
  "Legacy interface, apa_beeplot(), swarm inheritance", {
    vdiffr::expect_doppelganger(
      "Legacy interface, swarm inheritance",
      apa_beeplot(
        data = example_data, dv = "first_dv", id = "sid", factors = c("A", "B")
        , args_points = list(
          cex = seq(1, 2, length = 3)
          , bg = 2:4
          , pch = 23:25
          , col = "skyblue4"
          , lwd = 3
        )
      )
    )
  }
)

# More complex designs ----
test_that(
  "Legacy interface, apa_beeplot(), three factors", {
    vdiffr::expect_doppelganger(
      "Legacy interface, apa_beeplot(), three factors",
      apa_beeplot(data = example_data, dv = "first_dv", id = "sid", factors = c("A", "B", "C"))
    )
  }
)

test_that(
  "Legacy interface, apa_barplot(), three factors", {
    vdiffr::expect_doppelganger(
      "Legacy interface, apa_barplot(), three factors",
      apa_barplot(data = example_data, dv = "first_dv", id = "sid", factors = c("A", "B", "C"))

    )
  }
)

test_that(
  "Legacy interface, apa_lineplot(), three factors", {
    vdiffr::expect_doppelganger(
      "Legacy interface, apa_lineplot(), three factors",
      apa_lineplot(data = example_data, dv = "first_dv", id = "sid", factors = c("A", "B", "C"))
    )
  }
)
