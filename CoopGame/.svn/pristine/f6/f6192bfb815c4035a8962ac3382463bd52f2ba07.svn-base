boolSkip=F

test_that("Check 48.1 - unanimity coefficients for 3 players and game vector A=(0,0,0,60,48,30,72) - see also example at Harald Wiese p.124" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  A=c(0,0,0,60,48,30,72)
  result=getUnanimityCoefficients(A)
  expect_equal(result, c(0,0,0,60,48,30,-66))
})


#Example comes from powerpoint presentation "DECISION DANS L'INCERTAIN ET THEORIE DES JEUX", MORETTI, RIOS & TSOUKIAS - URL: http://www.lamsade.dauphine.fr/~tsoukias/cooperative_games_eddimo.ppt
test_that("Check 48.2 - unanimity coefficients for 3 players and game vector A=(3,4,1,8,4,6,10) - example found on the internet" ,{
  if(boolSkip){
    skip("Test was skipped")
  }

  A=c(3,4,1,8,4,6,10);
  result=getUnanimityCoefficients(A);
  expect_equal(result, c(3,4,1,1,0,1,0));
})

#Example comes from TUGlab harsanyidividends.M
# % >> A=[3 3 3 2 5 9 9];
# % >> [HD,unanimidad,Aplus,Aminus]=harsanyidividends(A)
# % HD = 3     3     3    -4    -1     3     2
test_that("Check 48.3 - unanimity coefficients for 3 players and game vector A=(3, 3, 3, 2, 5, 9, 9) - example from TUGlab file harsanyidividends.M" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  
  A=c(3, 3, 3, 2, 5, 9, 9);
  result=getUnanimityCoefficients(A);
  expect_equal(result, c(3,3,3,-4,-1,3,2));
})

#Example out of "Average tree solutions and the distribution of Harsanyi dividends" BEAL, REMILA & SOLAL (2009). URL:https://mpra.ub.uni-muenchen.de/17909/1/MPRA_paper_17909.pdf
test_that("Check 48.4 - unanimity coefficients for 3 players and game vector A=(0,2,1,1,4,1,6) - example found on the internet" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  
  A=c(0,2,1,1,4,1,6);
  result=getUnanimityCoefficients(A);
  expect_equal(result, c(0,  2,  1, -1,  3, -2,  3));
})




