## Michalski AI, Grigoriev P, Gorlishchev VP  ##

## MPIDR Technical Report ##

## 22.11.17 ##

## Splitting Fx (aggregated age-specific fertility rates) using quadratic optimization method (HFD data) ##

## Function 'QOSplitPar' ##

### Arguments of function 'QOSplitPar' ### 

## CY - country-year ID 
## Fx - matrix of birth counts (5-year age groups) to be split into single ages ##
## Pop5 - vector of population exposures (aggregated): length(Pop5)=length(Fx) ##
## Pop1 - vector of population exposures (1x1): length(Pop1)=length(AgeOutput)
## Pop1=NULL - no 1x1 data are available. In this case, the function can return only rates
## L - vector of ages  (lower limits. e.g 10-14 is 10, 20-24 is 20, and so on) ##
## AgeInt - vector of age intervals ##
## Rates=FALSE - birth counts should be returned, otherwise rates
## Pop1=NULL => Rates=TRUE

QOSplitPar<-function(CY,Fx,Pop5,Pop1=NULL,L,AgeInt,Rates=TRUE){
   
  ## Range of output ages ##
  AgeOutput<-c(min(L):(max(L)+AgeInt[length(AgeInt)]-1))
  
  # G - Exposure matrix 
  h<-AgeInt
  n<-length(AgeInt) # number of 5-year age groups
  N<-sum(h)			# number of columns  in G
    
  # Getting G matrix
  G<-matrix(0,nrow=n,ncol=N)
  G[1,1:h[1]]<-Pop5[1]/h[1]  
  j1<-h[1]+1					
  for (i in 2:n){ j2<-j1+h[i]-1		
                  G[i,j1:j2]<-Pop5[i]/h[i]
                  j1<-j2+1                            }
  
  ## Manual setting of weights (automatic solution to be implemented) ##
  A0<-AgeOutput[1]+1 
  setAGW<- function(CY,age0=A0,N=length(AgeOutput),delta=1){
    # function to set age weights  for different parities
    # Input: 	CY - CountryYear
    #		age0 - minimal age
    #		N - number of 1-year age groups
    # Output: 	list OUT with elements
    #		OUT$AGW - 6x(N-2) array AGW
    #		OUT$delta - delta value for quadratic optimization
    AGW<-matrix(1,nrow=6,ncol=N-2)	# age-specific weights by parity
            
    # Country specific cases
    cy<-substr(CY,1,3)
    if (cy=='BLR'){			# BLR
      AG1=rep(18,6); AG1[2]=24
      AG2=rep(50,6);
      Wgt=c(1,10,4,12,40,40)
      for (i in 1:6)AGW[i,AG1[i]:AG2[i]-age0+1]=Wgt[i] # Age weights setting
    }
    if (cy=='CZE'){			# CZE
      YY=as.numeric(substr(CY,4,7))
      if (YY>=1952&YY<=1994){	# Year specific case
        AG1=rep(18,6); AG1[2]=24
        AG2=rep(50,6);
        Wgt=c(1,10,4,12,40,40)
        for (i in 1:6)AGW[i,AG1[i]:AG2[i]-age0+1]=Wgt[i] # Age Weghts setting
      }	
    }
    if (cy=='EST'){			# EST
      YY=as.numeric(substr(CY,4,7))
      if (YY>=1965&YY<=1996){	# Year specific case
        AG1=rep(18,6); AG1[2]=24; AG1[6]=20 
        AG2=rep(53,6); AG2[6]=30
        Wgt=c(1,10,4,12,40,8)
        for (i in 1:6)AGW[i,AG1[i]:AG2[i]-age0+1]=Wgt[i] # Age weights setting
        AGW[6,31:45-age0+1]=16
      }
      AGW[6,46:52-age0+1]=16	
    }
    if (cy=='HUN'){			# HUN
      YY=as.numeric(substr(CY,4,7))
      AG1=rep(18,6); AG1[2]=24; AG1[6]=20 
      AG2=rep(53,6); AG2[6]=50
      Wgt=c(1,10,4,12,40,8)
      for (i in 1:6)AGW[i,AG1[i]:AG2[i]-age0+1]=Wgt[i] # Age weights setting
      AGW[6,26:40-age0+1]=16; AGW[5,22:34-age0+1]=80
      if (YY>=2001&YY<=2013) AGW[2,14:23-age0+1]=30
      if (YY==2014) AGW[2,13:25-age0+1]=40
      if (YY==2000) {AGW[2,14:23-age0+1]=60; delta=1}
    }
    if (cy=='JPN'){			# JPN
      YY=as.numeric(substr(CY,4,7))
      if (YY<=1991){		# Year specific case
        AG1=rep(18,6); AG1[2]=30; 
        AG2=rep(50,6);
        Wgt=c(1,10,4,12,40,8)
        for (i in 1:6)AGW[i,AG1[i]:AG2[i]-age0+1]=Wgt[i] # Age weights setting
        AGW[3,36:50-age0+1]=16
        AGW[4,36:50-age0+1]=32
        AGW[6,20:29-age0+1]=40
        AGW[2,14:21-age0+1]=14
      }
    }
    return(list(AGW=AGW,delta=delta))
  }
  
  aux<-setAGW(CY)  		# Set age weights by parity matrix  
  AGW<-aux$AGW
  delta<-aux$delta
  
 ## Matrix for the 2-nd order derivatives
 F<-matrix(0,nrow=N-2,ncol=N)
 for (i in 1:(N-2)){
   F[i,i]<-1; F[i,i+1]<--2; F[i,i+2]<-1;
 }
 F<-F[,-N]; F<-F[,-1];
   
 # 6 blocks F matrix 
 NB<-6*(N-2)			# total length of solution
 FB<-matrix(0,nrow=NB,ncol=NB)
 j<-1
 for (i in seq(1,NB-1,N-2)){
   nn<-1:(N-2)+i-1
   FB[nn,nn]<-diag(AGW[j,])%*%F
   j<-j+1
 }
 Dmat<-t(FB)%*%FB			# Quadratic form matrix 
 
 # equality constrains #########
 Gg<-G[,-c(1,N)];
 # 6 blocks G matrix 		
GB<-matrix(0,nrow=n*6,ncol=NB) 
i1<-1;n1<-1
 for (i in 1:6){			# loop by Parity
     i2<-i1+n-1;n2<-n1+N-3 
     nn<-1:(N-2)+i-1
   GB[i1:i2,n1:n2]<-Gg	# the same exposure for 'unconditional' ASFR
   i1<-i2+1;n1<-n2+1
 }
   
bvec<-Fx[,1]  			# right part of equality constrains
 for (par in 2:dim(Fx)[2]){
  bvec<-c(bvec,Fx[,par])	  }
 meq<-nrow(GB)			# number of balance constrains
 
 # inequality constrains #######
 # 6 blocks G matrix 
 aux<-diag(colSums(G)[-c(1,N)])		# total exposure by 1-year age groups universal case
 nB<-aux				# matrix of inequality constrains
 for (i in 1:5) {
   nB<-cbind(nB,-aux)
    }
 #delta=0.1				# balance margin
 
 # Matrix of constrains #################
 Amat<-t(rbind(GB,nB,-nB,diag(NB)))			# constrains matrix
 bbvec<-c(bvec,rep(-delta,2*nrow(nB)),rep(0,NB))	# constrains values
 dvec<-rep(0,nrow(Dmat))
 aux<-solve.QP(Dmat, dvec, Amat, bbvec, meq=meq, factorized=FALSE)
 
 ASFRP<-matrix(aux$solution,ncol=6)

 Fj<-rep(0,6)
 for (j in 1:6){
   aux<-F%*%ASFRP[,j]
   Fj[j]<-sum(aux*aux)	# values of functionals by parity
 }
 
 ## Assignment '0' to the first and the last ages ##
  ASFRP<-rbind(rep(0,6),ASFRP,rep(0,6))	# ASFR by PARITY
  ASFRP[ASFRP<0]<-0 ## get rid of extremely small negative values 
  
 ## Rounding ##
 ASFRP<-round(as.data.frame(ASFRP),8)
 tmp<-ASFRP
 ASFRP$CountryYear<-CY
 ASFRP<-cbind(AgeOutput,ASFRP)
 names(ASFRP)<-c("Age","ASFR0","ASFR1","ASFR2","ASFR3","ASFR4","ASFR5P","CountryYear")
 ASFRP<-within(ASFRP,Diff<-round(ASFR0-ASFR1-ASFR2-ASFR3-ASFR4-ASFR5P,5)) ## difference by parity 

 ## Birth counts ##
 if (length(Pop1)>1) { 
 Bx<-round(tmp*Pop1,2)
 Bx$CountryYear<-CY
 Bx<-cbind(AgeOutput,Bx)
 names(Bx)<-c("Age","B0","B1","B2","B3","B4","B5P","CountryYear") 
 Bx<-within(Bx,Diff<-round(B0-B1-B2-B3-B4-B5P,2)) ## difference by parity 
 }

  if (Rates==TRUE){ return(ASFRP) }
       else {return(Bx)} 
  }

################################## END #######################################################