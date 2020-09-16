
test_that(
  "Check lookup tables"
  , {
    expect(
      !any(duplicated(names(lookup_names)))
      , "Some names are duplicated in 'lookup_names'."
    )
    expect(
      !any(duplicated(names(lookup_labels)))
      , "Some names are duplicated in 'lookup_labels'."
    )
  }
)
