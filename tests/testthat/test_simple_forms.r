
forms <- list.files("./simple", pattern = "\\.yaml$", full.names = TRUE)

test_that("simple forms load", {
  for (i in 1:length(forms)) {
    expect_silent(read_yamlform(forms[i]))
  }
})

test_that("simple forms converts to xlsx", {
  for (i in 1:length(forms)) {
    expect_output(convert_yamlform(forms[i]))
  }
})

test_that("simple forms converts to xml using pyxform", {
  for (i in 1:length(forms)) {
    expect_output(convert_yamlform(forms[i]), output = "xml")
  }
})
