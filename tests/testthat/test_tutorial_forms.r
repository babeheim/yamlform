
forms <- list.files("./tutorial_forms", pattern = "\\.yaml$", full.names = TRUE)

test_that("standard tutorial forms load", {
  for (i in 1:length(forms)) {
    expect_silent(read_yamlform(forms[i]))
  }
})

test_that("standard tutorial forms converts to xlsx", {
  for (i in 1:length(forms)) {
    expect_output(convert_yamlform(forms[i]))
  }
})

test_that("standard tutorial forms converts to xml using pyxform", {
  for (i in 1:length(forms)) {
    expect_output(convert_yamlform(forms[i]), output = "xml")
  }
})
