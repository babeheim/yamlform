
check_tags_against_valid <- function(tags, valid){
  tags_valid <- tags %in% valid
  if(!all(tags_valid)){
    out_msg <- paste0("invalid tags: `",
      paste(tags[!tags_valid], collapse = "`, `"), "`\n see XLSform documentation")
    stop(out_msg)
  }
}


flatten_choices <- function(choices) {
  out <- list()
  for (i in 1:length(choices)) {
    for (j in 1:length(choices[[i]]$choices)) {
      choices[[i]]$choices[[j]]$`list name` <- choices[[i]]$`list name`
    }
    out <- c(out, choices[[i]]$choices)
  }
  return(out)
}

add_label <- function(dat) {
  if(!"label" %in% names(dat)) dat$label <- ""
  return(dat)
}



check_form <- function(form) {

  # to add:
  # for a block of questions in a table-list format, need to make sure there is only one choices list
  # every entry in choices must have a 'list name' entry, and must have a 'choices' entry
  # every entry in the choices sublist must have a `name` and a `label`
 
  type_options <- c(
  "start",
  "end",
  "today",
  "deviceid",
  "subscriberid",
  "simserial",
  "phonenumber",
  "integer",
  "decimal",
  "range",
  "text",
  "select_one",
  "select_multiple",
  "note",
  "geopoint",
  "geotrace",
  "geoshape",
  "date",
  "time",
  "dateTime",
  "image",
  "audio",
  "video",
  "file",
  "barcode",
  "calculate",
  "acknowledge",
  "hidden",
  "xml-external",
  "rank",
  "begin repeat",
  "end repeat",
  "begin group",
  "end group")

  type_tags <- form$survey$type

  type_tags <- gsub("rank .*", "rank", type_tags)
  type_tags <- gsub("select_one .*", "select_one", type_tags)
  type_tags <- gsub("select_multiple .*", "select_multiple", type_tags)
  check_tags_against_valid(type_tags, type_options)

  if ("appearance" %in% names(form$survey)) {
    appearance_tags <- form$survey$appearance
    appearance_options <- c(
      "multiline",
      "minimal",
      "quick",
      "no-calendar",
      "month-year",
      "year",
      "horizontal-compact",
      "horizontal",
      "likert",
      "compact",
      "quickcompact",
      "field-list",
      "label",
      "list-nolabel",
      "table-list",
      "signature",
      "draw",
      NA)

    check_tags_against_valid(appearance_tags, appearance_options)
  }
}


convert_yamlform <- function(file, output = "xlsx") {
  file %>% gsub("\\.yaml$", ".xlsx", .) -> xlsx_file
  file %>% read_yamlform() %>%
    write_xlsxform(path = xlsx_file)
  print(paste(xlsx_file, "created!"))
  if (output %in% c("xml", "xform")) {
    system(paste("xls2xform", xlsx_file))
    xlsx_file %>% gsub("\\.xlsx$", ".xml", .) -> xml_file
    print(paste(xml_file, "created!"))
  }
}



ungroup_survey <- function(survey_object) {
  if (!is.null(names(survey_object))) stop("survey_object must be an unnamed list!")
  survey_object %>% map(names) %>% map(~ any(. == "survey")) %>%
    unlist() %>% sum() -> n_subsurveys
  if (n_subsurveys > 0) {
    for (j in 1:n_subsurveys) {
      survey_object %>% map(names) %>% map(~ any(. == "survey")) %>%
        unlist() %>% which() %>% min() -> i
      # any survey_object might have more of them nested inside, so apply this recursively
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



flatten_keys <- function(named_list) {
  if (is.null(names(named_list))) stop("input must be a named list")
  named_list %>% map(class) %>% map(~ . == "list") %>% unlist() %>% which() -> sublists
  for (j in sublists) {
    names(named_list[[j]]) <- paste0(names(named_list)[j], "::", names(named_list[[j]]))
    class(named_list[[j]])
    named_list[[j]] <- flatten_keys(named_list[[j]])
  }
  named_list <- flatten(named_list)
  return(named_list)
}



read_yamlform <- function(path) {

  d <- yaml::read_yaml(path)

  if ("settings" %in% names(d)) {
    d$settings %>% as.data.frame() %>% as.tbl() -> d$settings
  }

  if ("choices" %in% names(d)) {
    d$choices %>% flatten_choices() %>%
      map(flatten_keys) %>% bind_rows() %>% add_label() %>%
      select(`list name`, name, label, everything()) -> d$choices
  }

  d$survey %>% ungroup_survey %>% map(flatten_keys) %>% bind_rows() %>%
    add_label() %>% select(type, name, label, everything()) -> d$survey
  
  d %>% check_form()

  return(d)

}




read_jsonform <- function(path) {

  d <- jsonlite::read_json(path)

  if ("settings" %in% names(d)) {
    d$settings %>% as.data.frame() %>% as.tbl() -> d$settings
  }

  if ("choices" %in% names(d)) {
    choices <- list()
    for (i in 1:length(d$choices)) {
      d$choices[[i]]$choices %>% bind_rows() %>%
        mutate(`list name` = d$choices[[i]]$`list name`) -> choices[[i]]
    }
    choices %>% bind_rows -> d$choices
  }

  d$survey %>% ungroup_survey -> d$survey
  d$survey %>% flatten_variants -> d$survey
  d$survey %>% bind_rows() %>%
    select(type, name, label, everything()) -> d$survey

  d %>% check_form()

  return(d)

}

read_xlsxform <- function(path) {
  out <- list()
  out$survey <- read.xlsx(path, sheet = "survey")
  out$choices <- read.xlsx(path, sheet = "choices")
  out$settings <- read.xlsx(path, sheet = "settings")
  out %>% check_form()
  return(out)
}

write_xlsxform <- function(form, path) {
  write.xlsx(form, path, colWidths = "auto")
}

