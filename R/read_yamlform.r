


ungroup_survey <- function(survey_object) {
  survey_object %>% map(names) %>% map(~ any(. == "survey")) %>%
    unlist() %>% sum() -> n_subsurveys
  if (n_subsurveys > 0) {
    for (j in 1:n_subsurveys) {
      survey_object %>% map(names) %>% map(~ any(. == "survey")) %>%
        unlist() %>% which() %>% min() -> i
      # any survey_object might have more of them nested inside
      survey_object[[i]]$survey %>% ungroup_survey -> survey_object[[i]]$survey
      sub_list <- c(
        survey_object[[i]]$survey,
        list(
          list(
            type = sub("begin", "end", survey_object[[i]]$type)
          )
        )
      )
      survey_object[[i]]$survey <- NULL
      survey_object %>% append(sub_list, after = i) -> survey_object
    }
  }
  return(survey_object)
}

flatten_variants <- function(survey_object) {
  for (i in 1:length(survey_object)) {
    survey_object[[i]] %>% map(length) -> lengths
    if (any(lengths > 1)) {
      list_entries <- which(lengths > 1)
      for (j in list_entries) {
        names(survey_object[[i]][[j]]) <- paste0(names(survey_object[[i]])[j], "::",
          names(survey_object[[i]][[j]]))
      }
      survey_object[[i]] %>% flatten -> survey_object[[i]]
    }
  }
  return(survey_object)
}

# XLSForm standard document

# the `type` variable has to be one of the following:
#   - integer
#   - decimal
#   - range
#   - text
#   - select_one [key] [options]
#   - select_multiple [key] [options]
#   - text
#   - note
#   - geopoint
#   - geotrace
#   - geoshape
#   - date
#   - time
#   - dateTime
#   - image
#   - audio
#   - video
#   - file
#   - barcode
#   - calculate
#   - acknowledge
#   - hidden
#   - xml-external
#   - begin repeat
#   - end repeat

# if i want to transfer mine in yaml, why isn't there are write_yamlform?

# and i need a flag to embed_choices or not

# read_yamlform needs to be smart enough to be able to detect embedded choices,
# pull them out and make them a separate thing

# in YAML, "yes" and "no" cannot be names, they ave to be in quotes or become TRUE/FALSE


read_yamlform <- function(path) {

  d <- yaml::read_yaml(path)

  if ("settings" %in% names(d)) {
    d$settings %>% as.data.frame() %>% as.tbl() -> d$settings
  }

  # if `choices` exists as its own list already...
  choices <- list()
  for (i in 1:length(d$choices)) {
    d$choices[[i]]$choices %>% bind_rows() %>%
      mutate(`list name` = d$choices[[i]]$`list name`) -> choices[[i]]
  }
  choices %>% bind_rows -> d$choices

  d$survey %>% ungroup_survey -> d$survey
  d$survey %>% flatten_variants -> d$survey
  d$survey %>% bind_rows() %>%
    select(type, name, label, everything()) -> d$survey

  return(d)

}




read_jsonform <- function(path) {

  d <- jsonlite::read_json(path)

  if ("settings" %in% names(d)) {
    d$settings %>% as.data.frame() %>% as.tbl() -> d$settings
  }

  # if `choices` exists as its own list already...
  # in YAML, "yes" and "no" cannot be names, they ave to be in quotes or become TRUE/FALSE
  if ("choices" %in% names(d)) {
    choices <- list()
    for (i in 1:length(d$choices)) {
      d$choices[[i]]$choices %>% bind_rows() %>%
        mutate(`list name` = d$choices[[i]]$`list name`) -> choices[[i]]
    }
    choices %>% bind_rows -> d$choices
  } else {
    # if `choices` exist within each question, pull them out?
    # but how does it know the `list name` then?
    # ah, its in the `type` field
    # we have to drop duplicates in this case
  }

  d$survey %>% ungroup_survey -> d$survey
  d$survey %>% flatten_variants -> d$survey
  d$survey %>% bind_rows() %>%
    select(type, name, label, everything()) -> d$survey

  return(d)

}




inspect_xlsxform <- function(path) {}
# reads it in, tells u if there's bugs


# since I'm already at it...
write_jsonform <- function(form, embed_choices = TRUE) {

}


write_yamlform <- function(form, embed_choices = TRUE) {

}


read_xlsxform <- function(path) {
  out <- list()
  out$survey <- read.xlsx(path, sheet = "survey")
  out$choices <- read.xlsx(path, sheet = "choices")
  out$settings <- read.xlsx(path, sheet = "settings")
  return(out)
}

write_xlsxform <- function(form, path) {
  write.xlsx(form, path, colWidths = "auto")
}

