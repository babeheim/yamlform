
form1 <- list(
  survey = list(
    list(
      name = "basic_group",
      label = "basic group",
      type = "begin group",
      survey = list(
        list(
          name = "second_group",
          label = "second group",
          type = "begin group",
          survey = list(
            list(
              name = "patient_age",
              label = "patient age",
              type = "integer"
            )
          )
        )
      )
    )
  )
)


form2 <- list(
  survey = list(
    list(
      name = "basic_group",
      label = "basic group",
      type = "begin group",
      survey = list(
        list(
          name = "second_group",
          label = "second group",
          type = "begin group",
          survey = list(
            list(
              name = "third_group",
              label = "third group",
              type = "begin group",
              survey = list(
                list(
                  name = "patient_age",
                  label = "patient age",
                  type = "integer"
                ),
                list(
                  name = "patient_sex",
                  label = "patient sex",
                  type = "integer"
                )
              )
            )
          )
        )
      )
    )
  )
)



form3 <- list(
  survey = list(
    list(
      name = "basic_group",
      label = "basic group",
      type = "begin repeat",
      survey = list(
        list(
          name = "second_group",
          label = "second group",
          type = "begin repeat",
          survey = list(
            list(
              name = "third_group",
              label = "third group",
              type = "begin repeat",
              survey = list(
                list(
                  name = "patient_age",
                  label = "patient age",
                  type = "integer"
                ),
                list(
                  name = "patient_sex",
                  label = "patient sex",
                  type = "integer"
                )
              )
            )
          )
        )
      )
    )
  )
)


form4 <- list(
  survey = list(
    list(
      name = "basic_group",
      label = "basic group",
      type = "begin repeat",
      survey = list(
        list(
          name = "second_group",
          label = "second group",
          type = "begin group",
          survey = list(
            list(
              name = "third_group",
              label = "third group",
              type = "begin repeat",
              survey = list(
                list(
                  name = "patient_age",
                  label = "patient age",
                  type = "integer"
                ),
                list(
                  name = "patient_sex",
                  label = "patient sex",
                  type = "integer"
                )
              )
            )
          )
        )
      )
    )
  )
)

form <- list()
form$survey <- c(form1$survey, form2$survey, form3$survey, form4$survey)

test_that("ungroup_survey works", {
  expect_error(form1 %>% ungroup_survey)
  expect_silent(form1$survey %>% ungroup_survey)
  expect_silent(form2$survey %>% ungroup_survey)
  expect_silent(form3$survey %>% ungroup_survey)
  expect_silent(form4$survey %>% ungroup_survey)
  expect_silent(form$survey %>% ungroup_survey)
  expect_silent(form$survey %>% ungroup_survey %>% bind_rows)
})
