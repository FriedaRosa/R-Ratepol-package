example_community_NA <- data.frame(
  sample_id = c("A", "B", "C"),
  sp1 = c(1, 2, 3),
  sp2 = c(4, NA, 6),
  stringsAsFactors = FALSE
)

example_community <- data.frame(
  sample_id = c("A", "B", "C"),
  sp1 = c(1, 2, 3),
  sp2 = c(4, 4, 6),
  stringsAsFactors = FALSE
)


example_age <- data.frame(
  sample_id = c("A", "B", "C"),
  age = c(1000, 2000, 3000),
  stringsAsFactors = FALSE
)

example_uncertainty <- matrix(
  c(950, 1050, 1950, 2050, 2950, 3050),
  nrow = 2,
  ncol = 3
)

test_that("extract_data returns expected structure", {
  result <- extract_data(
    data_community_extract = example_community_NA,
    data_age_extract = example_age,
    age_uncertainty = NULL,
    verbose = FALSE
  )
  
  expect_type(result, "list")
  expect_named(result, c("community", "age", "age_un"))
  expect_s3_class(result$community, "data.frame")
  expect_s3_class(result$age, "data.frame")
  expect_null(result$age_un)
})

test_that("missing values in community are replaced with 0", {
  result <- extract_data(example_community_NA, example_age, verbose = FALSE)
  expect_true(all(!is.na(result$community)))
  expect_true(all(result$community["B", "sp2"] == 0))
})

test_that("sample.id column is automatically renamed", {
  comm <- example_community
  names(comm)[1] <- "sample_id"
  age <- example_age
  names(age)[1] <- "sample_id"
  
  expect_silent(
    result <- extract_data(comm, age, verbose = FALSE)
  )
  expect_equal(rownames(result$community), c("A", "B", "C"))
})

test_that("throws error if sample_ids do not match", {
  broken_age <- example_age
  broken_age$sample_id[3] <- "D"
  expect_error(
    extract_data(example_community, broken_age),
    "Variable 'sample_id' must have same values"
  )
})

test_that("throws error when age and community do not have the same values", {
  shorter_age <- example_age[1:2, ]
  expect_error(
    extract_data(example_community, shorter_age),
    "Variable 'sample_id' must have same values in
    'data_age' and 'data_community"
  )
})


# There is a bug in extract_data() where column names for age_uncertainty 
# are assigned from a column in the ages table that does not exisit anymore.

test_that("age_uncertainty is handled correctly", {
  result <- extract_data(
    data_community_extract = example_community,
    data_age_extract = example_age,
    age_uncertainty = example_uncertainty,
    verbose = FALSE
  )
  
  expect_s3_class(result$age_un, "data.frame")
  expect_named(result$age_un, c("A", "B", "C"))
})

