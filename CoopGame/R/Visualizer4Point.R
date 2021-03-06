#' @title Visualizing points
#' @description Draws a point with 4 players
#' @aliases Visualizer4Point
#' @param  A contains a game vector
#' @param  pointsToDraw contains the points to draw
#' @param  colour draws the polyeder with this colour
#' @param  label activates the label for the figur
#' @param  name set a name for the label
#' @template author/FM
 
Visualizer4Point <- function(A, pointsToDraw, colour = NA , label=TRUE, name = NULL){ 
  if(is.na(colour)){
    colour="black"
  }
  #Eckpoints for imputationset
  X=rbind(c(0, 0, 0),  
          c(1, 0, 0),
          c(0.5, 0.5 * sqrt(3), 0),
          c(0.5, 1/6 * sqrt(3), 1/3 *sqrt(6)))
  
  vertices = bary2cart(X,pointsToDraw/A[15])
  
  #Drawing the point
  points3d(vertices[1],vertices[2],vertices[3],col=colour,size=10.0)
  
  #Label
  if(label == TRUE)
  {
    setLabels(A, pointsToDraw, name)
  }
}