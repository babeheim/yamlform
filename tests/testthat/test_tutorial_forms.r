
forms <- list.files("./tutorial_forms", full.names = TRUE)

test_that("standard tutorial forms load", {
  for (i in 1:length(forms)) {
    expect_silent(read_yamlform(forms[i]))
  }
})