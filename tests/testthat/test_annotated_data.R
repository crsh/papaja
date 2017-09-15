context("annotated_data")

test_that(
  "droplevels,annotated_factor-method"
  , {
    anno <- new("vector_annotation", label = "lab", unit = "un")
    object_1 <- new("annotated_factor", .Data = 1:4, levels = letters[1:10], annotation = anno, label = "lab")
    object_2 <- droplevels(object_1, exclude = "b")
    object_3 <- droplevels(object_1)

    object_4 <- droplevels.factor(x = factor(letters[1:4], levels = letters[1:10]), exclude = "b")

    # test case with excluded levels
    expect_identical(
      object = object_2
      , expected = new(
        "annotated_factor"
        , object_4 # base behavior changed recently, so just check consistency with currently installed version
        , annotation = anno
        , label = "lab"
      )
    )

    # test standard case
    expect_identical(
      object = object_3
      , expected = new(
        "annotated_factor"
        , .Data = 1:4
        , levels = letters[1:4]
        , annotation = anno
        , label = "lab"
      )
    )
  }
)

test_that(
  "droplevels,annotated_named_factor-method"
  , {
    anno <- new("vector_annotation", label = "lab", unit = "un")
    object_1 <- new(
      "annotated_named_factor"
      , .Data = 1:4
      , names = LETTERS[1:4]
      , levels = letters[1:10]
      , annotation = anno
    )
    object_2 <- droplevels(object_1, exclude = "b")
    object_3 <- droplevels(object_1)

    # base behavior changed recently, we can only check consistency with
    # installed base version
    object_4 <- droplevels.factor(
      x = factor(
        letters[1:4]
        , levels = letters[1:10]
      )
      , exclude = "b"
    )

    # test case with excluded levels
    expect_identical(
      object = object_2
      , expected = new(
        "annotated_named_factor"
        , object_4
        , names = LETTERS[1:4]
        , annotation = anno
        , label = "lab"
      )
    )

    # test standard case
    expect_identical(
      object = object_3
      , expected = new(
        "annotated_named_factor"
        , .Data = 1:4
        , names = LETTERS[1:4]
        , levels = letters[1:4]
        , annotation = anno
        , label = "lab"
      )
    )
  }
)


test_that(
  "show-methods for annotated_vector and annotated_factor"
  , {
    object_1 <- new("annotated_integer", .Data = 1:4)
    object_2 <- new("annotated_factor", .Data = 1:4, levels = letters[1:5])

    expect_identical(
      object = capture.output(show(object_1))
      , expected = c("Variable label     : ", "Unit of measurement: ", "[1] 1 2 3 4")
    )
    expect_identical(
      object = capture.output(show(object_2))
      , expected = c("Variable label     : ", "Unit of measurement: ", "[1] a b c d", "Levels:  a b c d e")
    )
  }
)


test_that(
  "relevel,annotated_factor,character/integer-method"
  , {
    object_1 <- new("annotated_factor", .Data = 1:3, levels = letters[1:3], label = "label1", annotation = new("vector_annotation", label = "label1", unit = "unit1"))
    object_2 <- relevel(object_1, ref = c("c", "b"))
    object_3 <- new("annotated_factor", .Data = 3:1, levels = letters[3:1], label = "label1", annotation = new("vector_annotation", label = "label1", unit = "unit1"))
    object_4 <- relevel(object_1, ref = 2L)
    object_5 <- new("annotated_factor", .Data = c(2, 1, 3), levels = letters[c(2, 1, 3)], label = "label1", annotation = new("vector_annotation", label = "label1", unit = "unit1"))

    expect_identical(
      object = object_2
      , expected = object_3
    )
    expect_identical(
      object = object_4
      , expected = object_5
    )
  }
)



test_that(
  "rep-methods for annotated_vector and annotated-factor"
  , {
    anno <- new("vector_annotation", label = "label1", unit = "unit1")
    object_1 <- rep(new("annotated_numeric", 1:4+.1, annotation = anno, label = "label1"), each = 2, times = 3)
    object_2 <- rep(new("annotated_factor", .Data = 1:2, levels = letters[1:6], annotation = anno, label = "label1"), each = 2, times = 3)

    expect_identical(
      object = object_1
      , expected = new("annotated_numeric", rep(1:4+.1, each = 2, times = 3), label = "label1", annotation = anno)
    )
    expect_identical(
      object = object_2
      , expected = new("annotated_factor", .Data = rep(1:2, each = 2, times = 3), levels = letters[1:6], annotation = anno, label ="label1")
    )
  }
)

test_that(
  "levels<-,annotated_factor-method"
  , {
    annotation <- new(
      "vector_annotation"
      , label = "label1"
      , unit = "unit1"
    )

    object_1 <- new(
      "annotated_factor"
      , .Data = 1:3
      , levels = letters[1:3]
      , annotation = annotation
      , label = annotation@label
    )

    levels(object_1) <- letters[4:9]

    object_2 <- new(
      "annotated_factor"
      , .Data = 1:3
      , levels = letters[4:9]
      , annotation = annotation
      , label = "label1"
    )

    expect_identical(
      object = object_1
      , expected = object_2
    )
  }
)


# ------------------------------------------------------------------------------

test_that(
  "names<-, annotated_vector-method"
  , {
    # Construct a standard annotated_integer
    annotation <- new("vector_annotation", label = "label1", unit = "kg")
    object_1 <- new("annotated_integer", .Data = 6:4, label = annotation@label, annotation = annotation)
    object_2 <- object_1
    object_3 <- object_1
    object_4 <- object_1
    # If full names are supplied, are they properly assigned
    names(object_2) <- letters[3:1]
    # If partial names are supplied, are NAs generated properly
    names(object_3) <- c(NA_character_, "b")
    # If NULL-names are assigned, vector should keep class annotated_integer
    names(object_4) <- NULL

    expect_identical(
      object = object_2
      , expected = new(
        "annotated_named_integer"
        , .Data = 6:4
        , names = letters[3:1]
        , label = "label1"
        , annotation = annotation
      )
    )
    expect_identical(
      object = object_3
      , expected = new(
        "annotated_named_integer"
        , .Data = 6:4
        , names = c(NA_character_, "b", NA_character_)
        , label = "label1"
        , annotation = annotation
      )
    )
    expect_identical(
      object = object_4
      , expected = object_1
    )
  }
)



test_that(
  "names<-, annotated_named_vector-method"
  , {
    annotation <- new(
      "vector_annotation"
      , label = "labelX"
      , unit = "unitX"
    )
    object <- new(
      "annotated_named_integer"
      , .Data = 5:7
      , names = letters[2:4]
      , annotation = annotation
    )
    names(object) <- letters[26:25]
    expect_identical(
      object = object
      , expected = new(
        "annotated_named_integer"
        , .Data = 5:7
        , names = c("z", "y", NA_character_)
        , annotation = annotation
        , label = "labelX"
      )
    )
  }
)






test_that(
  "as,annotated_vector,annotated_named_vector-methods"
  , {
    # Prepare
    annotation <- new(
      "vector_annotation"
      , label = "label1"
      , unit = "kg"
    )

    # Coercion from unnamed to named
    object_1 <- new(
      "annotated_integer"
      , .Data = 6:8
      , label = "label1"
      , annotation = annotation
    )

    object_2 <- as(object_1, "annotated_named_integer")

    # Coercion from named to unnamed
    object_3 <- new(
      "annotated_named_integer"
      , .Data = 2:5
      , names = letters[1:4]
      , label = "label1"
      , annotation = annotation
    )
    object_4 <- as(object_3, "annotated_integer")

    # Expectations
    expect_identical(
      object = object_2
      , expected = new(
        "annotated_named_integer"
        , .Data = 6:8
        , label = "label1"
        , annotation = annotation
      )
    )

    expect_identical(
      object = object_4
      , expected = new(
        "annotated_integer"
        , .Data = 2:5
        , label = "label1"
        , annotation = annotation
      )
    )
  }
)

# ------------------------------------------------------------------------------
# as-methods for coercion from one annotated_vector-type to another

test_that(
  "as,annotated_vector,annotated_vector-methods"
  , {
    annotation <- new("vector_annotation", label = "labelX", unit = "unitX")

    object_1 <- new(
      "annotated_numeric"
      , .Data = 1:3 - .5
      , annotation = annotation
    )

    object_2 <- new(
      "annotated_integer"
      , .Data = 1:3
      , annotation = annotation
    )
    object_3 <- new(
      "annotated_character"
      , .Data = letters[2:4]
      , annotation = annotation
    )
    object_4 <- new(
      "annotated_logical"
      , .Data = c(T, F)
      , annotation = annotation
    )
    # Coercion to annotated_numeric --------------------------------------------
    as_num_1 <- as(object_1, "annotated_numeric")
    as_num_2 <- as(object_2, "annotated_numeric")
    as_num_3 <- suppressWarnings(as(object_3, "annotated_numeric")) # expect a warning
    as_num_4 <- as(object_4, "annotated_numeric")

    # Coercion to annotated_integer --------------------------------------------
    as_int_1 <- as(object_1, "annotated_integer")
    as_int_2 <- as(object_2, "annotated_integer")
    as_int_3 <- suppressWarnings(as(object_3, "annotated_integer")) # expect a warning
    as_int_4 <- as(object_4, "annotated_integer")

    # Coercion to annotated_character ------------------------------------------
    as_chr_1 <- as(object_1, "annotated_character")
    as_chr_2 <- as(object_2, "annotated_character")
    as_chr_3 <- as(object_3, "annotated_character")
    as_chr_4 <- as(object_4, "annotated_character")

    # Coercion to annotated_logical --------------------------------------------
    as_log_1 <- as(object_1, "annotated_logical")
    as_log_2 <- as(object_2, "annotated_logical")
    as_log_3 <- as(object_3, "annotated_logical")
    as_log_4 <- as(object_4, "annotated_logical")

    # Coercion to annotated_factor ---------------------------------------------
    as_fac_1 <- as(object_1, "annotated_factor")
    as_fac_2 <- as(object_2, "annotated_factor")
    as_fac_3 <- as(object_3, "annotated_factor")
    as_fac_4 <- as(object_4, "annotated_factor")

    # --------------------------------------------------------------------------
    expect_identical(
      object = as_num_1
      , expected = object_1
    )
    expect_identical(
      object = as_num_2
      , expected = new(
        "annotated_numeric"
        , .Data = as.numeric(1:3)
        , annotation = annotation
      )
    )
    expect_identical(
      object = as_num_3
      , expected = new(
        "annotated_numeric"
        , .Data = rep(NA_real_, 3)
        , annotation = annotation
      )
    )
    expect_identical(
      object = as_num_4
      , expected = new(
        "annotated_numeric"
        , .Data = as.numeric(c(1, 0))
        , annotation = annotation
      )
    )
    # --------------------------------------------------------------------------
    expect_identical(
      object = as_int_1
      , expected = new(
        "annotated_integer"
        , .Data = 0:2
        , annotation = annotation
      )
    )
    expect_identical(
      object = as_int_2
      , expected = object_2
    )
    expect_identical(
      object = as_int_3
      , expected = new(
        "annotated_integer"
        , .Data = rep(NA_integer_, 3)
        , annotation = annotation
      )
    )
    expect_identical(
      object = as_int_4
      , expected = new(
        "annotated_integer"
        , .Data = c(1, 0)
        , annotation = annotation
      )
    )
    # --------------------------------------------------------------------------
    expect_identical(
      object = as_chr_1
      , expected = new(
        "annotated_character"
        , .Data = c("0.5", "1.5", "2.5")
        , annotation = annotation
      )
    )
    expect_identical(
      object = as_chr_2
      , expected = new(
        "annotated_character"
        , .Data = c("1", "2", "3")
        , annotation = annotation
      )
    )
    expect_identical(
      object = as_chr_3
      , expected = object_3
    )
    expect_identical(
      object = as_chr_4
      , expected = new(
        "annotated_character"
        , .Data = c("TRUE", "FALSE")
        , annotation = annotation
      )
    )
    # --------------------------------------------------------------------------
    expect_identical(
      object = as_log_1
      , expected = new(
        "annotated_logical"
        , .Data = rep(TRUE, 3)
        , annotation = annotation
      )
    )
    expect_identical(
      object = as_log_2
      , expected = new(
        "annotated_logical"
        , .Data = rep(TRUE, 3)
        , annotation = annotation
      )
    )
    expect_identical(
      object = as_log_3
      , expected = new(
        "annotated_logical"
        , .Data = rep(NA_integer_, 3)
        , annotation = annotation
      )
    )
    expect_identical(
      object = as_log_4
      , expected = object_4
    )
    # --------------------------------------------------------------------------
    expect_identical(
      object = as_fac_1
      , expected = new(
        "annotated_factor"
        , .Data = 1:3
        , annotation = annotation
        , levels = c("0.5", "1.5", "2.5")
      )
    )
    expect_identical(
      object = as_fac_2
      , expected = new(
        "annotated_factor"
        , .Data = 1:3
        , annotation = annotation
        , levels = as.character(1:3)
      )
    )
    expect_identical(
      object = as_fac_3
      , expected = new(
        "annotated_factor"
        , .Data = 1:3
        , annotation = annotation
        , levels = letters[2:4]
      )
    )
    expect_identical(
      object = as_fac_4
      , expected = new(
        "annotated_factor"
        , .Data = 2:1
        , annotation = annotation
        , levels = c("FALSE", "TRUE")
      )
    )
  }
)


test_that(
  "[,annotated_named_vector-methods"
  , {
    annotation <- new("vector_annotation", label = "labelX", unit = "unitX")
    object_1 <- new(
      "annotated_named_integer"
      , .Data = 1:4
      , names = letters[1:4]
      , annotation = annotation
    )
    object_2 <- new(
      "annotated_named_factor"
      , .Data = 1:4
      , names = letters[1:4]
      , annotation = annotation
      , levels = LETTERS[1:4]
    )

    expect_identical(
      object = object_1[2:3]
      , expected = new(
        "annotated_named_integer"
        , .Data = 2:3
        , names = letters[2:3]
        , annotation = annotation
      )
    )
    expect_identical(
      object = object_2[2:3]
      , expected = new(
        "annotated_named_factor"
        , .Data = 2:3
        , names = letters[2:3]
        , annotation = annotation
        , levels = LETTERS[1:4]
      )
    )
  }
)

test_that(
  "annotate,annotated_vector-method"
  , {
    anno <- new("vector_annotation", label = "label", unit = "unit")
    object_1 <- new("annotated_numeric", .Data = 1:2, annotation = anno)
    object_2 <- annotate(object_1)
    expect_identical(
      object = object_2
      , expected = object_1
    )
  }
)

# Coercion to annotated_factor -------------------------------------------------

test_that(
  "coerce to annotated_factor"
  , {
    num <- as.numeric(1:4)
    int <- 1:4
    chr <- letters[1:3]
    log <- c(TRUE, FALSE)
    fac <- factor(letters[3:5])

    object_1 <- as(num, "annotated_factor")
    object_2 <- as(int, "annotated_factor")
    object_3 <- as(chr, "annotated_factor")
    object_4 <- as(log, "annotated_factor")
    object_5 <- as(fac[1:2], "annotated_factor")

    expect_identical(
      object = object_1
      , expected = new(
        "annotated_factor"
        , .Data = 1:4
        , levels = as.character(1:4)
      )
    )
    expect_identical(
      object = object_2
      , expected = new(
        "annotated_factor"
        , .Data = 1:4
        , levels = as.character(1:4)
      )
    )
    expect_identical(
      object = object_3
      , expected = new(
        "annotated_factor"
        , .Data = 1:3
        , levels = letters[1:3]
      )
    )
    expect_identical(
      object = object_4
      , expected = new(
        "annotated_factor"
        , .Data = 2:1
        , levels = c("FALSE", "TRUE")
      )
    )
    expect_identical(
      object = object_5
      , expected = new(
        "annotated_factor"
        , .Data = 1:2
        , levels = letters[3:5]
      )
    )
  }
)

