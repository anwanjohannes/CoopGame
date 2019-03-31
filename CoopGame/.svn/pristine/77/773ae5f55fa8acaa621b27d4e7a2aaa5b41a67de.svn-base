boolSkip=F




test_that("Check 70.1 - testing calculation of class Modiclus" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  
  A<-c( 0,  0,  0,  1,  1, 0, 1)
  result=modiclus(A)
  
  expect_equal(result,c(0.5,0.25,0.25),tolerance=1e-3)
  
})

test_that("Check 70.2 - testing calculation of class Modiclus" ,{
  if(boolSkip){
    skip("Test was skipped")
  }
  a=seq(from=0,to=(2.5),by=1e-1)
  
  bm5=createBitMatrix(n=5)
  N=31
  for(i in 1:length(a)){
    A=apply(bm5,1,FUN=function(x){min(sum(x[1:2]),a[i]*sum(x[3:5]))})
    if( !(length(unique(A)) == 1 & A[1]==0) )
    {
      modiclus=new("Modiclus",A)
      mn=calculateNucleolus(modiclus)
      cm=getMatrix(modiclus@LPCoopGameUtils)
      rlb=getRlb(modiclus@LPCoopGameUtils)
      cm_1=sweep(cm[,1:5],MARGIN=2,mn,'*')
      ex_1=sort(rlb-sum(cm_1[,1:5]),decreasing = TRUE)
  
      cm_2=cm_1=sweep(cm[,1:5],MARGIN=2,((1/12)*c(3*A[N],3*A[N],2*A[N],2*A[N],2*A[N])),'*')
      ex_2=sort(rlb-sum(cm_2[,1:5]),decreasing = TRUE)
      sapply(1:length(ex_1),function(ix){expect_equal(ex_2[ix],ex_1[ix],tolerance=1e-3)})
    }
  }
  
})



