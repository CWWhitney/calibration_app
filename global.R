# R packages -------------------------------------------------------------------
library(googlesheets4)
library(pins)
library(purrr)
library(bslib)
library(shiny.i18n)
library(jsonlite)
library(shinyvalidate)

# Load custom functions --------------------------------------------------------
list.files("R", full.names = TRUE) |> 
  purrr::walk(source)

# User-Defined Parameters -------------------------------------------------
## Define the language questions will be asked in
language <- "German"

languages <- c(
  "German",
  "English",
  "Kiswahili",
  "Spanish",
  "Vietnamese",
  "Engkis"
)


# Generate a 32-byte key for encryption
# sodium::random(32)
our_key <- as.raw(c(0x3f, 0x79, 0x6b, 0x5c, 0xe0, 0x40, 0x8e, 0x0b, 0x64, 
                    0x1c, 0xf6, 0xe7, 0x51, 0xaa, 0xd2, 0xe7, 0x7a, 0x4f, 0x38, 0x32, 
                    0x49, 0x3e, 0x3e, 0x29, 0xed, 0x18, 0xa9, 0xa2, 0x6c, 0x3b, 0x09, 
                    0x71))

# Generate a nonce
# sodium::random(24)
our_nonce <- as.raw(c(0x9a, 0xb0, 0x99, 0x9a, 0xbe, 0x10, 0x59, 0x0d, 0x06, 
                      0x6f, 0x29, 0x0a, 0xb8, 0xf7, 0xc3, 0xdb, 0xa8, 0x58, 0x90, 0x4c, 
                      0x0a, 0xc9, 0xae, 0xc2)) 

options(scipen=1000000000)

# Question Selection -----------------------------------------------------------

## Select the questions you want for each group (i.e., each "round")

## Change and use these to set up each workshop
selected_questions <- list(
  Group_1 = list(
    binary = c(138), #c(138:147),
    range = c(153) #c(153:162)
  ),
  Group_2 = list(
    binary = c(148:169), #c(148:151, 153),
    range = c(175, 184, 207) #c(175:182, 184, 207)
  ),
  
  Group_3 = list(
    binary = NULL,
    range = c(164, 165, 167:174)
  ),
  
  Group_4 = list(
    binary = c(154:163),
    range = c(208:227)
  ),
  Group_5 = list(
    binary = c(164:173),
    range = c(242:261)
  )
)


# User interface language ------------------------------------------------------
interface_language_url <- 
  Sys.getenv("interface_language_url")

interface_languages <- 
  # googlesheets4::read_sheet(interface_language_url)
  readRDS("data/interface_languages.RDS")

selected_language <- dplyr::pull(interface_languages, language)


# Import the binary & range questions ------------------------------------------

## Define Google API authentication type
## If the Google Sheet is public, simply call `googlesheets4::gs4_death()` here
## to indicate that no authentication is necessary

# googlesheets4::gs4_deauth()

## Retrieve the URL of the Google Sheet
google_sheets_url <- 
  Sys.getenv("google_sheets_url") 


questions_full <- get_full_data(
  gs_url = google_sheets_url
)

questions <- selected_data(
  selected_questions_list = selected_questions, 
  questions_full = questions_full,
  languages = languages
)

# questions <- get_data(
#   selected_questions_list = selected_questions,
#   language = language,
#   gs_url = Sys.getenv("google_sheets_url")
# )
# 


# Questions index --------------------------------------------------------------
## Create a data frame that indexes all of the workshop questions
question_index <- 
  questions |>
  purrr::map_dfr(
    ~ dplyr::select(.x, Group, QuestionNumber),
    .id = "QuestionType"
  ) |>
  dplyr::mutate(Group = as.integer(stringr::str_sub(Group, -1, -1))) |>
  dplyr::arrange(Group, QuestionType, QuestionNumber) |>
  dplyr::mutate(Index = dplyr::row_number())

# Translators ------------------------------------------------------------------

## Interface -------------------------------------------------------------------
list(
  "languages" = languages,
  "translation" = interface_languages |>
    dplyr::select(dplyr::all_of(languages)) 
) |> 
  toJSON(pretty = TRUE, auto_unbox = TRUE) |> 
  write(file = "interface_translations.json")

interface_translator <- 
  Translator$new(
    translation_json_path = "interface_translations.json"
  )

interface_translator$set_translation_language(language)


## Binary ----------------------------------------------------------------------
list(
  "languages" = languages,
  "translation" = questions_full |> 
    purrr::pluck("binary") |>
    dplyr::select(dplyr::all_of(languages)) 
) |> 
  toJSON(pretty = TRUE, auto_unbox = TRUE) |> 
  write(file = "binary_translations.json")


binary_translator <- 
  Translator$new(translation_json_path = "binary_translations.json")

binary_translator$set_translation_language(language)


## Range -----------------------------------------------------------------------
list(
  "languages" = languages,
  "translation" = 
    questions_full |> 
    purrr::pluck("range") |>
    dplyr::select(dplyr::all_of(languages)) 
) |> 
  toJSON(pretty = TRUE, auto_unbox = TRUE) |> 
  write(file = "range_translations.json")


range_translator <- 
  Translator$new(translation_json_path = "range_translations.json")

range_translator$set_translation_language(language)


# Build UI Theme ---------------------------------------------------------------
## Develop the Bootstrap theme for the app

app_theme <- bslib::bs_theme(
  version = 5, 
  bootswatch = "sketchy", 
  dark = "#153015",
  bg = "#153015", 
  fg = "#FFFFFF", 
  primary = "#004F9E",   # Bonn blue
  secondary = "#FBBA00",   # Bonn yellow
  warning = "#FBBA00", # Bonn yellow
  danger = "#FE6100",
  "body-bg" = "#153015",
  "navbar-bg" = "#153015",
  "navbar-light-color" = "white",
  "navbar-light-hover-color" = "white",
  "nav-tabs-link-active-color" = "white",
  "nav-link-color" = "white",
  "nav-link-hover-color" = "#FBBA00"
) |> bslib::bs_add_variables(
  # "modal-backdrop-opacity" = 1,
  # "modal-backdrop-bg" = "grey"
) 

