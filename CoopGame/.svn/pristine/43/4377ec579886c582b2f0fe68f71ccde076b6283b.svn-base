#' @name nucleolus
#' @title nucleolus for n-player TU games
#' @description Computes the nucleolus of a TU game with n players.
#' @aliases nucleolus
#' @export nucleolus
#' @template author/JA
#' @template author/DG
#' @template cites/SCHMEIDLER_1969
#' @templateVar SCHMEIDLER_1969_P pp. 1163--1170
#' @template cites/KOHLBERG_1971
#' @templateVar KOHLBERG_1971_P pp. 62--66
#' @template cites/KOPELOWITZ_1967
#' @template cites/MEGIDDO_1974
#' @templateVar MEGIDDO_1974_P pp. 355--358
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 82 ff.
#' @inheritParams CoopGameBaseParams
#' @template param/enableTermOutLP
#' @return Numeric vector of length n representing the nucleolus.
#' @examples
#' #Example with no glpk output:
#' nucleolus(c(1, 1, 1, 2, 3, 4, 5))
#' 
#' #Example with glpk output:
#' nucleolus(c(0, 0, 0, 0, 5, 5, 8, 9, 10, 8, 13, 15, 16, 17, 21), enableTermOutLP = TRUE)

nucleolus <- function(A, enableTermOutLP = FALSE){
  nuc=Nucleolus(A=A)
  return(calculateNucleolus(nuc,enableTermOutLP=enableTermOutLP))
} 


#' @title Constructor for Nucleolus
#' @noRd
#' @template author/DG
#' @name Nucleolus
# @export
Nucleolus<-function(A){
  retNucleolus=methods::new("Nucleolus",A=A)
  return(retNucleolus)
}

#' @title Nucleolus
#' @noRd
#' @template author/JA
#' @template author/DG
#' @description An S4 class for various solving the Nucleolus concept
#' @include NucleolusBase.R
# @exportClass Nucleolus

setClass(
  "Nucleolus",
  contains="NucleolusBase"
)

#' @name drawNucleolus
#' @title drawNucleolus for n players
#' @description drawNucleolus draws the nucleolus for 3 or 4 players.
#' @aliases drawNucleolus
#' @export drawNucleolus
#' @template author/JA
#' @template cites/SCHMEIDLER_1969
#' @templateVar SCHMEIDLER_1969_P pp. 1163--1170
#' @template cites/KOHLBERG_1971
#' @templateVar KOHLBERG_1971_P pp. 62--66
#' @template cites/KOPELOWITZ_1967
#' @template cites/MEGIDDO_1974
#' @templateVar MEGIDDO_1974_P pp. 355--358
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 82 ff.
#' @inheritParams CoopGameBaseParams
#' @inheritParams visualize
#' @examples
#' A=c(1, 1, 1, 2, 3, 4, 5)
#' drawNucleolus(A)
drawNucleolus<-function(A,holdOn=FALSE, colour = NA , label=TRUE, name = "Nucleolus"){
  A=GameVector(A)
  nuc=nucleolus(A);
  visualize(A, pointsToDraw=nuc, holdOn=holdOn, colour = colour , label=label, name = name)
}

