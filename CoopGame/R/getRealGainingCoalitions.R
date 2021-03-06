#' @name getRealGainingCoalitions
#' @title getRealGainingCoalitions
#' @description The function getRealGaingCoalitions identifies all real gaining coalitions.
#' Coalition \code{S} is a real gaining coalition if for any true subset \code{T} of \code{S} 
#' there holds: v(S) > v(T)
#' @export getRealGainingCoalitions
#' @template author/JA
#' @template author/JS
#' @template cites/HOLLER_ET_LI_1995
#' @templateVar HOLLER_ET_LI_1995_P pp. 257 -- 270
#' @template cites/BERTINI_ET_STACH_2015
#' @templateVar BERTINI_ET_STACH_2015_P pp. 9 -- 25
#' @template param/A
#' @return A data frame containing all real gaining coalitions.
#' @examples
#' # Example from original 1995 paper by Holler and Li
#' library(CoopGame)
#' A <- c(1,2,3,4,0,0,0)
#' getRealGainingCoalitions(A)
#'# Output:
#'#    V1 V2 V3 cVal
#'# 1  1  0  0    1
#'# 2  0  1  0    2
#'# 3  0  0  1    3
#'# 4  1  1  0    4
#'

getRealGainingCoalitions<-function(A){
   n=as.numeric(getNumberOfPlayers(A))
   N=length(A)
   bm=as.data.frame(createBitMatrix(n,A))
   players=1:n
   # all singleton coalitions with a value > 0 are real gaining
   idcs=unlist(sapply(1:n, function(pl){
     if(bm[pl,"cVal"]>0){
       return(pl) 
     }
   }))
   # find the rest of the real gaining coalitions
   for(i in (n+1):N){
     involvedPlayers=getPlayersFromBMRow(bm[i,])
     uninvolvedPlayers=players[-involvedPlayers]
     corrCVals=getCorrespondingCVals(bm[1:(i-1),],uninvolvedPlayers)
     allSmaller=all(corrCVals<bm[i,"cVal"])
     if(allSmaller){
       idcs=c(idcs,i)
     }
   }
   bm=bm[idcs,,drop=FALSE]
   return(bm)
}


