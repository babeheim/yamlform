
form <- list(
  survey = list(
    list(
      name = "patient_name",
      label = list(
        English = "patient name here",
        Spanish = "nombre aqui"
      ),
      type = "text"
    )
  )
)

form2 <- list(
  survey = list(
    list(
      name = "media example",
      type = "note",
      label = "Media Example",
      hint = list(
        English = "a hint",
        Spanish = "een hint"
      ),
      media = list(
        image = list(
          English = "en.jpg",
          Dutch = "nl.jpg"
        ),
        video = list(
          English = "en.mp4",
          Dutch = "nl.mp4"
        )
      )
    )
  )
)

test_that("flatten_keys works", {
  expect_silent(form$survey %>% map(flatten_keys)
    %>% `[[`(1) %>% names -> x)
  expect_equal(x, c("name",  "label::English", "label::Spanish", "type"))
  expect_silent(form2$survey %>% map(flatten_keys))
})





