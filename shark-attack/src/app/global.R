library(shiny)
library(shiny.fluent)
library(modules)
library(config)
library(sass)
library(shinyjs)
library(glue)
library(googlesheets4)

consts <- config::get(file = "constants/constants.yml")

sass(
  sass::sass_file(consts$sass$input),
  cache = FALSE,
  options = sass_options(output_style = consts$sass$style),
  output = consts$sass$output
)

utils <- use("utils/utils.R")

gameOver <- use("modules/gameOver.R")
gameStart <- use("modules/gameStart.R")

GridManager <- use("logic/GridManager.R")$GridManager
ObjectsManager <- use("logic/ObjectsManager.R")$ObjectsManager
ScoreManager <- use("logic/ScoreManager.R")$ScoreManager
TrashManager <- use("logic/TrashManager.R")$TrashManager
LeaderboardManager <- use("logic/LeaderboardManager.R")$LeaderboardManager
