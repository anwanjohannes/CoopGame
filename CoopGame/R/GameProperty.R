#' @title GameProperty - Virtual class for game property concepts
#' @description Virtual class for game property concepts, inherits methods and slots from class
#' \linkS4class{CoopGameProperty}.
#' @include CoopGameProperty.R
#' @exportClass GameProperty

setClass(
  "GameProperty",
  representation("VIRTUAL"),
  contains = "CoopGameProperty"
)