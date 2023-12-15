# output gives number of head, chest and abdomen CTs the person had in a given calendar year ("year") at a given age ("age")
exposure_probability_Dec2015 = function(n_CTs, CT_years, year, age, transmergeddata1CTaboveage10, transmergeddata1CTbelowage10,
                                        transmergeddata2CTaboveage10, transmergeddata2CTbelowage10, transmergeddata3CTaboveage10,
                                        transmergeddata3CTbelowage10, transmergeddata4CTaboveage10, transmergeddata4CTbelowage10,
                                        transmergeddata5CTaboveage10,transmergeddata5CTbelowage10){
  CT_yr = which(CT_years == year)
  n_CT_type = rep(0, 3) # stores number of head, chest and abdomen CTs; col 1- no of head CTs, col2 -no of abd CTs, col3- no of chest CTs
  if (n_CTs == 1){
    type_CT = sample(c(1,2,3), size = 1, replace = T, prob = transmergeddata1CTbelowage10[CT_yr,3:dim(transmergeddata1CTbelowage10)[2]])*(age < 10) + 
      sample(c(1,2,3), size = 1, replace = T, prob = transmergeddata1CTaboveage10[CT_yr,3:dim(transmergeddata1CTaboveage10)[2]])*(age >= 10)
    # 1-head, 2-abd, 3-chest
    n_CT_type[type_CT] = 1 
  }
  else if (n_CTs == 2){
    type_CT = sample(c(1:6), size = 1, replace = T, prob = transmergeddata2CTbelowage10[CT_yr,3:dim(transmergeddata2CTbelowage10)[2]])*(age < 10) + 
      sample(c(1:6), size = 1, replace = T, prob = transmergeddata2CTaboveage10[CT_yr,3:dim(transmergeddata2CTaboveage10)[2]])*(age >= 10)
    # 1-aa, 2-ha, 3-hc, 4-hh, 5-ac, 6-cc
    n_CT_type = (type_CT == 1) * c(0, 2, 0) + (type_CT == 2) * c(1, 1, 0) +
      (type_CT == 3) * c(1, 0, 1) + (type_CT == 4) * c(2, 0, 0) +
      (type_CT == 5) * c(0, 1, 1) + (type_CT == 6) * c(0, 0, 2)
  }
  else if (n_CTs == 3){
    type_CT = sample(c(1:10), size = 1, replace = T, prob = transmergeddata3CTbelowage10[CT_yr,3:dim(transmergeddata3CTbelowage10)[2]])*(age < 10) + 
      sample(c(1:10), size = 1, replace = T, prob = transmergeddata3CTaboveage10[CT_yr,3:dim(transmergeddata3CTaboveage10)[2]])*(age >= 10)
    # 1-haa, 2-hha, 3-hhh, 4-hhc, 5-hcc, 6-acc, 7-ccc, 8-aac, 9-hac, 10-aaa
    n_CT_type = (type_CT == 1) * c(1, 2, 0) + (type_CT == 2) * c(2, 1, 0) +
      (type_CT == 3) * c(3, 0, 0) + (type_CT == 4) * c(2, 0, 1) +
      (type_CT == 5) * c(1, 0, 2) + (type_CT == 6) * c(0, 1, 2) +
      (type_CT == 7) * c(0, 0, 3) + (type_CT == 8) * c(0, 2, 1) +
      (type_CT == 9) * c(1, 1, 1) + (type_CT == 10) * c(0, 3, 0)
  }
  else if (n_CTs == 4){
    type_CT = sample(c(1:15), size = 1, replace = T, prob = transmergeddata4CTbelowage10[CT_yr,3:dim(transmergeddata4CTbelowage10)[2]])*(age < 10) + 
      sample(c(1:15), size = 1, replace = T, prob = transmergeddata4CTaboveage10[CT_yr,3:dim(transmergeddata4CTaboveage10)[2]])*(age >= 10)
    # 1-aaaa, 2-hhhh, 3-cccc, 4-aacc, 5-hacc, 6-hhaa, 7-hhha, 8-hhac, 9-accc, 10-aaac, 11-hhcc, 12-hhhc, 13-haaa, 14-hccc, 15-haac 
    n_CT_type = (type_CT == 1) * c(0, 4, 0) + (type_CT == 2) * c(4, 0, 0) +
      (type_CT == 3) * c(0, 0, 4) + (type_CT == 4) * c(0, 2, 2) +
      (type_CT == 5) * c(1, 1, 2) + (type_CT == 6) * c(2, 2, 0) +
      (type_CT == 7) * c(3, 1, 0) + (type_CT == 8) * c(2, 1, 1) +
      (type_CT == 9) * c(0, 1, 3) + (type_CT == 10) * c(0, 3, 1) +
      (type_CT == 11) * c(2, 0, 2) + (type_CT == 12) * c(3, 0, 1) +
      (type_CT == 13) * c(1, 3, 0) + (type_CT == 14) * c(1, 0, 3) + (type_CT == 15) * c(1, 2, 1)
  }
  else if (n_CTs == 5){
    type_CT = sample(c(1:21), size = 1, replace = T, prob = transmergeddata5CTbelowage10[CT_yr,3:dim(transmergeddata5CTbelowage10)[2]])*(age < 10) + 
      sample(c(1:21), size = 1, replace = T, prob = transmergeddata5CTaboveage10[CT_yr,3:dim(transmergeddata5CTaboveage10)[2]])*(age >= 10)
    #  1-hhhhh, 2-hhhhc, 3-acccc, 4-aaccc, 5-aaacc, 6-hhccc, 7-hhhaa, 8-ccccc, 9-hhacc, 10-haacc, 11-hhhha,
    #  12-aaaaa, 13-hhaac, 14-haaac, 15-hhhac, 16-hcccc, 17-haccc, 18-hhhcc, 19-aaaac, 20-hhaaa, 21-haaaa  
    n_CT_type = (type_CT == 1) * c(5, 0, 0) + (type_CT == 2) * c(4, 0, 1) +
      (type_CT == 3) * c(0, 1, 4) + (type_CT == 4) * c(0, 2, 3) +
      (type_CT == 5) * c(0, 3, 2) + (type_CT == 6) * c(2, 0, 3) +
      (type_CT == 7) * c(3, 2, 0) + (type_CT == 8) * c(0, 0, 5) +
      (type_CT == 9) * c(2, 1, 2) + (type_CT == 10) * c(1, 2, 2) +
      (type_CT == 11) * c(4, 1, 0) + (type_CT == 12) * c(0, 5, 0) +
      (type_CT == 13) * c(2, 2, 1) + (type_CT == 14) * c(1, 3, 1) +
      (type_CT == 15) * c(3, 1, 1) + (type_CT == 16) * c(1, 0, 4) +
      (type_CT == 17) * c(1, 1, 3) + (type_CT == 18) * c(3, 0, 2) +
      (type_CT == 19) * c(0, 4, 1) + (type_CT == 20) * c(2, 3, 0) + (type_CT == 21) * c(1, 4, 0)
  }
  return(n_CT_type)
}


dose_calculation_Dec2015 = function(organ, scan_ages, year, age, sex, no_headCTs, no_chestCTs, no_abdCTs, 
                                    head_scan_doses_males_pre2001, chest_scan_doses_males_pre2001,
                                    abdomen_scan_doses_males_pre2001, head_scan_doses_males_post2001,
                                    chest_scan_doses_males_post2001, abdomen_scan_doses_males_post2001,
                                    head_scan_doses_females_pre2001, chest_scan_doses_females_pre2001,
                                    abdomen_scan_doses_females_pre2001, head_scan_doses_females_post2001,
                                    chest_scan_doses_females_post2001, abdomen_scan_doses_females_post2001){
  #organ = 1 for brain tumor analysis and organ = 2 for leukemia analysis
  age_ct = max(which(scan_ages <= age))
  if (sex == 1){ # if male
    if (year < 2001){
      DoseHeadCT = no_headCTs * head_scan_doses_males_pre2001[age_ct, organ]
      DoseChestCT = no_chestCTs * chest_scan_doses_males_pre2001[age_ct, organ]
      DoseAbdomenCT = no_abdCTs * abdomen_scan_doses_males_pre2001[age_ct, organ]
    }
    else {
      DoseHeadCT = no_headCTs * head_scan_doses_males_post2001[age_ct, organ]
      DoseChestCT = no_chestCTs * chest_scan_doses_males_post2001[age_ct, organ]
      DoseAbdomenCT = no_abdCTs * abdomen_scan_doses_males_post2001[age_ct, organ]
    }
  }
  else {
    if (year < 2001){
      DoseHeadCT = no_headCTs * head_scan_doses_females_pre2001[age_ct, organ]
      DoseChestCT = no_chestCTs * chest_scan_doses_females_pre2001[age_ct, organ]
      DoseAbdomenCT = no_abdCTs * abdomen_scan_doses_females_pre2001[age_ct, organ]
    }
    else {
      DoseHeadCT = no_headCTs * head_scan_doses_females_post2001[age_ct, organ]
      DoseChestCT = no_chestCTs * chest_scan_doses_females_post2001[age_ct, organ]
      DoseAbdomenCT = no_abdCTs * abdomen_scan_doses_females_post2001[age_ct, organ]
    }
  }
  DoseCT = DoseHeadCT + DoseChestCT + DoseAbdomenCT
  AgeCT = age # AgeCT equals "age" if subject had at least 1 CT
  result = list("DoseCT" = DoseCT, "AgeCT" = AgeCT)
  return(result)
}


library(readxl)
library(polynom)
deaths = read_excel("X:/Department Medizin/Biometrie/Projekte/CT/subprojects/simulation_study_bias/datasets_for_simulations/deaths_2009.xlsx")
surv_prob_years_men = (deaths[3,-1]-deaths[1,-1])/deaths[3,-1]
surv_prob_years_women = (deaths[4,-1]-deaths[2,-1])/deaths[4,-1]
ages = c(0.5, 3, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 52.5, 57.5, 62.5, 67.5, 72.5, 77.5, 82.5, 87.5, 92.5, 97.5)
lm9_male = lm(as.numeric(surv_prob_years_men) ~ ages + I(ages^2) + I(ages^3) + I(ages^4) + I(ages^5) + I(ages^6) + I(ages^7) + I(ages^8) + I(ages^9))
lm9_female = lm(as.numeric(surv_prob_years_women) ~ ages + I(ages^2) + I(ages^3) + I(ages^4) + I(ages^5) + I(ages^6) + I(ages^7) + I(ages^8) + I(ages^9))
survival_prob = function(age,sex){
  if (sex == 1){ #male
    S = predict(polynomial(lm9_male$coefficients), age)
  }
  else {
    S = predict(polynomial(lm9_female$coefficients), age)
  }
  return(S)
}


#linERR package function with threshold based only on cases
#' @title Fit linear ERR model
#' @description Fits the linear ERR model on a dataset
#' @param data data frame containing matched case-control data, with a number of columns for doses to different locations, a column containing matched set numbers, a column containing the case's tumor location (value between 1 and the number of locations, with location \eqn{x} corresponding to the \eqn{x}-th column index in \code{doses}) and a column serving as a case-control indicator. Other covariates can also be included, in this case a parameter for each covariate column will be estimated. Hence factor variables need to be converted to dummy variables using \code{model.matrix}. If using \code{ccmethod='meandose'}, a column for tumor location is still required but in this case the column can be a vector of ones.
#' @param doses vector containing the indices of columns containing dose information.
#' @param set column index containing matched set numbers.
#' @param status column index containing case status.
#' @param loc column index containing the location of the matched set's case's second tumor.
#' @param corrvars vector containing the indices of columns containing variables to be corrected for.
#' @param repar reparametrize to \eqn{\beta=exp(\xi)}? Defaults to \code{FALSE}
#' @param ccmethod choice of method of analysis: one of meandose, CCML, CCAL or CL. Defaults to CCAL
#' @param initpars initial values for parameters, default is 0 for all parameters. If supplying a different vector, use a vector with an initial value for \eqn{\beta} or \eqn{\xi}, one for all of the other location effects and one for each other covariate (in that order). Note that if \code{repar=TRUE}, the initial value is used for \eqn{\xi}.
#' @param fitopt list with options to pass to \code{control} argument of optimizer
#' @param fitNull boolean: also fit model without dose effect? Defaults to \code{TRUE}. Note: the same optimization algorithm that was used for the MLE will be used for the null model, even if the null model only has one parameter (see details)
#' @param useOld if TRUE, a previous (slower) implementation of the log-likelihood function will be used. Defaults to \code{FALSE}
#' @param uplimBeta upper limit for \eqn{\beta=exp(\xi)}, default value 5. This is used for constraining the MLE estimation in some settings and for the jackknife inclusion criteria, and can be infinite except when Brent optimization is used (see help for \code{linearERR})
#' @return Object with components:
#' \item{fit}{object produced by \code{mle2}}
#' \item{nullfit}{fit without dose effect produced by \code{mle2}}
#' \item{proflik}{profile likelihood: one-dimensional function of \eqn{\beta} or \eqn{\xi}. Note that the optimization used is the same as for the MLE, leading to one-dimensional Nelder-Mead optimization in certain cases (see details of \code{linearERR})}
#' @importFrom numDeriv hessian
#' @importFrom stats constrOptim
#' @details This is a stripped down version of \code{linearERR}, and should only be used when that function does not suffice. For more details refer to the help of \code{linearERR}.
#' @seealso linearERR
#' @examples
#' data(linearERRdata1)
#'
#' fitmeandose <- linearERRfit(data=linearERRdata1, set=1, doses=2:6,
#' status=8, loc=7, corrvars=9, repar=FALSE, ccmethod="meandose")
#'
#' fitCCML <- linearERRfit(data=linearERRdata1, set=1, doses=2:6,
#' status=8, loc=7, corrvars=9, repar=FALSE, ccmethod="CCML")
#'
#' fitCCAL <- linearERRfit(data=linearERRdata1, set=1, doses=2:6,
#' status=8, loc=7, corrvars=9, repar=FALSE, ccmethod="CCAL")
#'
#' fitCL <- linearERRfit(data=linearERRdata1, set=1, doses=2:6,
#' status=8, loc=7, corrvars=NULL, repar=FALSE, ccmethod="CL")
#'
#' fitmeandose$fit$par
#' fitCCML$fit$par
#' fitCCAL$fit$par
#' fitCL$fit$par
#' @export


linearERRfit_nothresh <- function(data, doses, set, status, loc, corrvars=NULL, repar=FALSE, ccmethod="CCAL", initpars=rep(0,length(doses)+length(corrvars)), fitopt=list(maxit=5000), fitNull=TRUE, useOld=FALSE, uplimBeta=5){
  
  if(ccmethod=="CL") corrvars <- NULL
  
  if(ccmethod %in% c("CCML", "meandose") & length(corrvars)==0){
    opt_method <- "Brent"
    if(is.infinite(uplimBeta)) stop("Please provide a finite value for uplimBeta to use in Brent optimization")
  } else{
    opt_method <- "constrOptim"
    if(repar & is.infinite(uplimBeta)) stop("uplimBeta needs to be finite for the current settings")
  }
  
  if(is.null(fitopt$maxit)){
    fitopt <- c(fitopt, list(maxit=5000))
  }
  if(is.null(fitopt$reltol)){
    fitopt <- c(fitopt, list(reltol=1e-10))
  }
  # if (is.null(fitopt$pgtol) & opt_method=="L-BFGS-B"){
  #   fitopt <- c(fitopt, list(pgtol=1e-8))
  # }
  # if (is.null(fitopt$factr) & opt_method=="L-BFGS-B"){
  #   fitopt <- c(fitopt, list(factr=1e4))
  # }
  # if (is.null(fitopt$ndeps) & opt_method=="L-BFGS-B"){
  #   parlen <- 1+ifelse(ccmethod %in% c("CCAL","CL"),length(doses)-1,0)+length(corrvars)
  #   fitopt <- c(fitopt, list(ndeps=rep(1e-5, parlen)))
  # }
  
  likfun <- function(params){
    if(repar) params[1] <- exp(params[1])
    if(useOld){
      linERRloglikold(params, data=data, set=set, doses=doses,status=status,loc=loc,corrvars=corrvars, ccmethod=ccmethod)
    } else {
      linERRloglik(params, data=data, set=set, doses=doses,status=status,loc=loc,corrvars=corrvars, ccmethod=ccmethod)
    }
  }
  scorefun <- function(params){
    linERRscore(params, data=data, set=set, doses=doses,status=status,loc=loc,corrvars=corrvars, ccmethod=ccmethod, repar=repar)
  }
  
  if(opt_method!="Brent"){
    names <- c(ifelse(repar,"xi","beta"), paste0("alpha",2:length(doses)),names(data)[corrvars])
    names(initpars) <- names
    #parnames(likfun) <- names
  } else {
    names <- ifelse(repar, "xi","beta")
    initpars <- list(params=as.numeric(initpars[1]))
    names(initpars) <- names
    #parnames(likfun) <- names
  }
  
  strt <- initpars
  
  if(ccmethod=="CCAL"){
    fxd <- NULL
    
    lw <- sapply(names,FUN=function(x) -Inf)
    if(!repar) lw[1] <- -1/max(data[,doses])+.0001
    if(repar){
      up <- list(xi=log(uplimBeta))
    } else {
      up <- list(beta=uplimBeta)
    }
    
  } else if (ccmethod=="CL"){
    #strt <- strt[names(strt)%in% c("xi","beta", paste0("alpha",2:length(doses)))]
    fxd <- NULL
    
    lw <- sapply(names[!names %in% names(data)[corrvars]],FUN=function(x) -Inf)
    if(!repar) lw[1] <- -1/max(data[data[,status]==1,doses])+.0001
    if(repar){
      up <- list(xi=log(uplimBeta))
    } else {
      up <- list(beta=uplimBeta)
    }
    
  } else if(ccmethod=="CCML"){
    if(opt_method=="constrOptim"){
      fxd <- sapply(paste0("alpha",2:length(doses)), function(i) 0)
      lw <- sapply(c(ifelse(repar,"xi","beta"),tail(names, length(corrvars))),FUN=function(x) -Inf)
      if(!repar) {lw[1] <- -1/max(data[,doses][cbind(1:nrow(data), data[,loc])])+.0001}
      #if(length(corrvars)==0){
      if(repar){
        up <- list(xi=log(uplimBeta))
      } else {
        up <- list(beta=uplimBeta)
      }
    } else if(opt_method=="Brent") {
      fxd <- NULL
      lw <- list(params=ifelse(repar, -10,  -1/max(data[,doses][cbind(1:nrow(data), data[,loc])])+.0001))
      up <- list(params=ifelse(repar,log(uplimBeta),uplimBeta))
    } #else { # Nelder-Mead
    #   fxd <- sapply(paste0("alpha",2:length(doses)), function(i) 0)
    #   lw <-NULL
    #   up <- NULL
    #}
    #} else{
    #  up <- NULL
    #}
  } else if(ccmethod=="meandose"){
    if(opt_method=="constrOptim"){
      fxd <- sapply(paste0("alpha",2:length(doses)), function(i) 0)
      lw <- sapply(c(ifelse(repar,"xi","beta"),tail(names, length(corrvars))),FUN=function(x) -Inf)
      if(!repar) {lw[1] <- -1/max(rowMeans(data[,doses]))+.0001}
      #if(length(corrvars)==0){
      if(repar){
        up <- list(xi=log(uplimBeta))
      } else {
        up <- list(beta=uplimBeta)
      }
    } else if(opt_method=="Brent") {
      fxd <- NULL
      lw <- list(params=ifelse(repar, -100,  -1/max(data[data$case ==1,]$cumdose)))
      up <- list(params=ifelse(repar,log(uplimBeta),uplimBeta))
    }# else { # Nelder-Mead
    #   fxd <- sapply(paste0("alpha",2:length(doses)), function(i) 0)
    #   up <- NULL
    #   lw <- NULL
    # }
    #} else{
    #  up <- NULL
    #}
  }
  
  
  
  if(opt_method=="constrOptim"){
    strt0 <- strt[!names(strt)%in%names(fxd)]
    strt[names(fxd)] <- unlist(fxd)
    if(repar){
      #fit <- mle2(likfun, start=strt,fixed=fxd, method=opt_method,control=fitopt, vecpar=TRUE, parnames=names)
      ui <- matrix(c(-1,rep(0,length(strt0)-1)),nrow=1)
      ci <- -1*log(uplimBeta)
    } else {
      ui <- matrix(c(1,rep(0,length(strt0)-1)),nrow=1)
      ci <- lw[1]
    }
    betathresh <- ci
    
    fit <- constrOptim(strt0,function(x){
      y <- strt
      y[names(x)] <- x
      likfun(y)
    }, ui=ui, ci=ci, grad=function(x){
      y <- strt
      y[names(x)] <- x
      if(!is.null(fxd)){
        res <-  -scorefun(y[-which(names(y) %in% names(fxd))])$U
        if(any(names(res) %in% names(fxd))) res <- res[-which(names(res) %in% names(fxd))]
      } else {
        res <-  -scorefun(y)$U
      }
      res
    }, control=fitopt, outer.iterations=200, hessian=TRUE)
    tmpcoef <- strt
    tmpcoef[names(fit$par)] <- fit$par
    #fit$par <- tmpcoef
    
    fit$fullcoef <- c(tmpcoef[1],tmpcoef[-1])
    names(fit$fullcoef) <- names
    fit$betathresh <- betathresh
  } else {
    betathresh <- lw
    fit <- optim(initpars, function(x){
      likfun(c(x, rep(0, length(doses)+length(corrvars)-1)))
    }, method=opt_method, lower=lw, upper=up, control=fitopt, hessian=TRUE)
    names(fit$par) <- names
    fit$fullcoef <- fit$par
    fit$betathresh <- betathresh
  }
  
  
  #fit <- mle2(likfun, start=initpars, vecpar=(opt_method!="Brent"),fixed=fxd,upper=up, lower=lw, control=fitopt, method=opt_method)
  
  if(fitNull){
    repar0 <- repar
    repar <- FALSE
    names[1] <- "beta"
    if(opt_method!="Brent" ){
      
      
      fxd2 <- c(fxd, list(beta=0))
      
      
      strt <- sapply(names,FUN=function(x) 0)
      strt0 <- strt[!names(strt)%in%names(fxd2)]
      strt[names(fxd2)] <- unlist(fxd2)
      ui <- matrix(c(-1,rep(0,length(strt0)-1)),nrow=1)
      ci <- -20
      
      nullfit <- constrOptim(strt0,function(x){
        y <- strt
        y[names(x)] <- x
        likfun(y)
      }, ui=ui, ci=ci, grad=function(x){
        y <- strt
        y[names(x)] <- x
        if(!is.null(fxd)){
          res <-  -scorefun(y[-which(names(y) %in% names(fxd))])$U
          res <- res[-which(names(res) %in% names(fxd2))]
        } else {
          res <-  -scorefun(y)$U
          res <- res[-which(names(res) %in% c("beta"))]
        }
        
      }, control=fitopt, outer.iterations=200, hessian=TRUE)
      
      
      
      
    } else {
      nullfit <- list(par=0, value=likfun(c(0, rep(0, length(doses)+length(corrvars)-1))), hessian=hessian(function(x) likfun(c(x, rep(0, length(doses)+length(corrvars)-1))), 0))
      
    }
    
    repar <- repar0
    names[1] <- ifelse(repar, "xi","beta")
    
    #
    # lw2 <- lw[-1]
    # up2 <- up[-1]
    # if(length(lw2)==0) lw2 <- NULL
    # if(length(up2)==0) up2 <- NULL
    # nullfit <- mle2(likfun, start=initpars, vecpar=(opt_method!="Brent"),fixed=fxd2,upper=up2, lower=lw2, control=fitoptnull, method=opt_method)
    
  } else {
    nullfit <- NULL
  }
  
  proflik <- function(x){
    if(opt_method!="Brent"){
      
      if(repar){
        fxd3 <- c(fxd, list(xi=x))
      } else {
        fxd3 <- c(fxd, list(beta=x))
      }
      strt <- sapply(names,FUN=function(x) 0)
      strt0 <- strt[!names(strt)%in%names(fxd3)]
      strt[names(fxd3)] <- unlist(fxd3)
      ui <- matrix(c(-1,rep(0,length(strt0)-1)),nrow=1)
      ci <- -20
      
      proffit <- constrOptim(strt0,function(xx){
        y <- strt
        y[names(xx)] <- xx
        likfun(y)
      }, ui=ui, ci=ci, grad=function(xx){
        y <- strt
        y[names(xx)] <- xx
        if(!is.null(fxd)){
          res <-  -scorefun(y[-which(names(y) %in% names(fxd))])$U
          res <- res[-which(names(res) %in% names(fxd3))]
        } else {
          res <-  -scorefun(y)$U
          res <- res[-which(names(res) %in% c("xi","beta"))]
        }
        
      }, control=fitopt, outer.iterations=200, hessian=TRUE)
    } else {
      proffit <- list(par=x, value=likfun(c(x, rep(0, length(doses)+length(corrvars)-1))), hessian=hessian(function(xxx) likfun(c(xxx, rep(0, length(doses)+length(corrvars)-1))), x))
      
      # fitoptprof <- fitopt
      # fxd3 <- list(params=x)
    }
    # lw3 <- lw[-1]
    # up3 <- up[-1]
    # if(length(lw3)==0) lw3 <- NULL
    # if(length(up3)==0) up3 <- NULL
    
    
    proffit$value
    #mle2(likfun, start=initpars, vecpar=(opt_method!="Brent"),fixed=fxd3,upper=up3, lower=lw3, control=fitoptprof, method=opt_method)@min
  }
  
  
  list(fit=fit, nullfit=nullfit, proflik=proflik)
  
}


#constrained linearERR (it calls linearERRfit_constrained)
#' @title Fit linear ERR model and perform jackknife correction
#' @description Fits the linear ERR model on matched case-control data and performs first and second order jackknife correction
#' @param data data frame containing matched case-control data, with a number of columns for doses to different locations, a column containing matched set numbers, a column containing the case's tumor location (value between 1 and the number of locations, with location \eqn{x} corresponding to the \eqn{x}-th column index in \code{doses}) and a column serving as a case-control indicator. Other covariates can also be included, in this case a parameter for each covariate column will be estimated. Hence factor variables need to be converted to dummy variables using \code{model.matrix}. If using \code{ccmethod='meandose'}, a column for tumor location is still required but in this case the column can be a vector of ones.
#' @param doses vector containing the indices of columns containing dose information.
#' @param set column index containing matched set numbers.
#' @param status column index containing case status.
#' @param loc column index containing the location of the matched set's case's second tumor.
#' @param corrvars vector containing the indices of columns containing variables to be corrected for.
#' @param repar reparametrize to \eqn{\beta=exp(\xi)}? Defaults to \code{FALSE}
#' @param ccmethod choice of method of analysis: one of meandose, CCML, CCAL or CL. Defaults to CCAL
#' @param initpars initial values for parameters, default is 0 for all parameters. If supplying a different vector, use a vector with an initial value for \eqn{\beta} or \eqn{\xi}, one for all of the other location effects and one for each other covariate (in that order). Note that if \code{repar=TRUE}, the initial value is used for \eqn{\xi}.
#' @param fitopt list with options to pass to \code{control} argument of optimizer (see details)
#' @param uplimBeta upper limit for \eqn{\beta=exp(\xi)}, default value 5. This is used for constraining the MLE estimation in some settings and for the jackknife inclusion criteria, and can be infinite except when Brent optimization is used (see details)
#' @param profCI boolean: compute 95\% profile likelihood confidence interval for \eqn{\beta}/\eqn{\xi}? Default value TRUE.
#' @param doJK1 perform first order jackknife correction? Automatically set to \code{TRUE} when \code{doJK2=TRUE}. Caution: this can take a long time to run. Default value FALSE
#' @param doJK2 perform second order jackknife correction? Caution: this can take a very long time to run. Default value FALSE
#' @param jkscorethresh square L2 norm threshold for leave-one-out and leave-two-out estimates to be included in the computation of the first and second order jackknife corrected estimate, respectively
#' @param jkvalrange range of leave-one-out and leave-two-out beta/xi estimates to be allowed in the computation of the first and second order jackknife corrected estimate, respectively
#' @return Object with components \code{MLE} and \code{jackknife}. \code{MLE} has components:
#'
#' \item{coef}{estimated model coefficients}
#' \item{sd}{estimated standard deviation for all coefficient estimates}
#' \item{vcov}{variance-covariance matrix for all estimates}
#' \item{score}{score in the MLE}
#' \item{convergence}{convergence code produced by the optimizer (for details refer to \code{optim})}
#' \item{message}{convergence message produced by the optimizer}
#' \item{dosepval}{p-value for the LRT comparing the produced model with a model without dose effect. Note that the null model this is based on uses the same optimization algorithm used for the MLE, meaning one-dimensional Nelder-Mead is used when \code{repar=TRUE} and the full model has 2 free parameters (see details)}
#' \item{profCI}{the 95\% profile likelihood confidence interval. In some cases one or both of the bounds of the CI cannot be obtained automatically. In that case, it is possible to use the \code{proflik} function that is an output of \code{linearERRfit} directly. Note: the same optimization algorithm that was used for the MLE will be used, even if this model only has one parameter (see details)}
#' \item{fitobj}{Fit object produced by linearERRfit}
#'
#' \code{jackknife} has components \code{firstorder} and \code{secondorder}. Both of these have components:
#'
#' \item{coef}{the jackknife-corrected coefficient estimates}
#' \item{details}{data frame with information on leave-one-out or leave-two-out estimates, with columns:
#' \itemize{
#' \item{\code{set} or \code{set1} and \code{set2}, the left-out set(s)}
#' \item{\code{included}, a 0/1 variable indicating whether this row was used to produce the corrected estimate}
#' \item{\code{conv}, convergence code for each model produced by the optimizer}
#' \item{\code{coef}, the leave-one-out or leave-two-out coefficient estimates}
#' \item{\code{score}, the score in the leave-one-out or leave-two-out estimate}
#' }}
#' Note that the \code{details} for the second order jackknife only include leave-two-out estimates. To access leave-one-out estimates, use \code{details} for the first order jackknife.
#' @importFrom stats model.matrix pchisq uniroot
#' @importFrom utils combn setTxtProgressBar txtProgressBar
#' @details This is the main function of the package, used for fitting the linear ERR model in matched case-control data. Use this function to estimate the MLE (including a profile likelihood confidence interval for the dose effect) and to perform first and second order jackknife corrections.
#'
#' The model being fit is HR=\eqn{\sum(1+\beta d_l)exp(\alpha_l+X^T\gamma)}, where the sum is over organ locations. Here \eqn{\beta} is the dose effect, \eqn{\alpha} are the location effects and \eqn{\gamma} are other covariate effects. The model can be reparametrized to HR=\eqn{\sum(1+exp(\xi) d_l)exp(\alpha_l+X^T\gamma)} using \code{repar=TRUE}. In the original parametrization, \eqn{\beta} is constrained such that HR cannot be negative. There are different choices for the design used to estimate the parameters: mean organ dose, CCML, CL, and CCAL. Mean organ dose (\code{ccmethod='meandose'}) uses the mean of the supplied location doses and compares that mean dose between case and matched controls. The other choices (CCML, CL and CCAL) use the tumor location for the case and compare either only between patients (CCML), only within patients (CL) or both between and within patients (CCAL). CCML only compares the same location between patients, and hence cannot be used to estimate location effects. Similarly, CL compares within patients and cannot be used to estimate covariate effects other than dose, meaning \code{corrvars} should not be supplied for CL.
#'
#' For one-dimensional models (i.e., mean dose or CCML without additional covariates), the Brent algorithm is used with a search interval (-10,log(\code{uplimBeta})) when \code{repar=TRUE} and (L,\code{uplimBeta}) otherwise, where L is determined by the positivity constraint for HR. For other optimizations, the L-BFGS-B algorithm (with constraint \code{uplimBeta}) is used when \code{repar=FALSE}, and the unconstrained Nelder-Mead is used when \code{repar=TRUE}. For details refer to the function \code{optim}, also for \code{fitopt} settings. Note that when supplying \code{ndeps} to \code{fitopt}, a value needs to be specified for every free parameter in the model. For more flexibility in optimizion, use \code{linERRloglik} and optimize directly.
#'
#' The jackknife procedure allows for filtering of the leave-one-out and leave-two-out estimates, which is important as the model can be unstable and produce extreme estimates. All estimates reaching the maximum number of iterations are excluded, as well as estimates larger than uplimBeta (if applicable). Further, the user can set a threshold for the square L2 norm of the score for an estimate (default .01), as well as an allowed value range for the \eqn{\beta}/\eqn{\xi} estimate itself. When the jackknife is run, the output object contains an element \code{details}, allowing the user to inspect the produced leave-one-out and leave-two-out estimates.
#' @examples
#' data(linearERRdata1)
#'
#' fitCCML <- linearERR(data=linearERRdata1, set=1, doses=2:6, status=8,
#' loc=7, corrvars=9, repar=FALSE, ccmethod="CCML", doJK1=TRUE)
#'
#' fitCCML$MLE$coef
#' fitCCML$jackknife$firstorder$coef

#' @export

linearERR_constrained <- function(data, doses, set, status, loc, corrvars=NULL, ccmethod="CCAL", repar=FALSE, initpars=rep(0,length(doses)+length(corrvars)), fitopt=NULL, uplimBeta=5,profCI=TRUE,doJK1=FALSE,doJK2=FALSE,jkscorethresh=.01,jkvalrange=c(-Inf, Inf), constrained_value){
  
  if(doJK2) doJK1 <- TRUE
  
  mainfit <- linearERRfit_constrained(data=data, doses=doses, set=set, status=status, loc=loc, corrvars=corrvars, repar=repar, initpars=initpars, fitopt=fitopt, ccmethod=ccmethod, uplimBeta=uplimBeta, constrained_value = constrained_value)
  
  MLEscore <- linERRscore(params=mainfit$fit$par, data=data, doses=doses, set=set, status=status, loc=loc, corrvars=corrvars, repar=repar,ccmethod=ccmethod)$U
  
  pval <- pchisq(2*(mainfit$nullfit$value-mainfit$fit$value), df=1, lower.tail=FALSE)
  
  
  if(profCI){
    
    
    g <- function(para){
      1-pchisq(2*(mainfit$proflik(para)-mainfit$fit$value),df=1)-.05
    }
    lowLim <- tryCatch(uniroot(g, lower=ifelse(repar,-20,mainfit$fit$betathresh), upper=mainfit$fit$par[1], extendInt="no")$root, error=function(e) NA)
    upLim <- tryCatch(uniroot(g, lower=mainfit$fit$par[1],upper=ifelse(repar,log(100),100), extendInt="no", maxiter=150)$root, error=function(e) NA)
    
  } else{
    lowLim <- NULL
    upLim <- NULL
  }
  
  MLE <- list(coef=mainfit$fit$par,sd=sqrt(diag(solve(mainfit$fit$hessian))), vcov=solve(mainfit$fit$hessian), score=MLEscore, convergence=mainfit$fit$convergence, message=mainfit$fit$message, dosepval=pval, profCI=c(lo=lowLim, up=upLim), fitobj=mainfit)
  
  # Jackknife
  
  setnrs <- unique(data[,set])
  
  
  if(doJK1){
    message("First order jackknife:")
    pb <- txtProgressBar(min = 1, max = length(setnrs), style = 3)
    
    outfunjk1 <- function(exclset) {
      jk1fit <- linearERRfit(data=data[!(data[,set]== exclset),], doses=doses, set=set, status=status, loc=loc, corrvars=corrvars, repar=repar, initpars=initpars, fitopt=fitopt, ccmethod=ccmethod, fitNull=FALSE, uplimBeta=uplimBeta)
      jk1score <- tryCatch(linERRscore(params=jk1fit$fit$par, data=data[!(data[,set]== exclset),], doses=doses, set=set, status=status, loc=loc, corrvars=corrvars, repar=repar,ccmethod=ccmethod)$U, error=function(e) rep(NA, length(jk1fit$fit$fullcoef)))
      list(fit=jk1fit,score=jk1score)
    }
    jk1out <- lapply(setnrs, function(k) {
      setTxtProgressBar(pb, which(setnrs==k))
      return(outfunjk1(k))
    })
    
    jk1coefs <- sapply(jk1out, function(x) x$fit$fit$par)
    jk1coefs <- as.data.frame(matrix(jk1coefs, nrow=length(setnrs),byrow=TRUE, dimnames=list(NULL,paste0("coef.",names(jk1out[[1]]$fit$fit$par)))))
    jk1scores <- sapply(jk1out, function(x) x$score)
    jk1scores <- as.data.frame(matrix(jk1scores, nrow=length(setnrs),byrow=TRUE, dimnames=list(NULL,paste0("score.",names(jk1out[[1]]$fit$fit$par)))))
    jk1conv <- sapply(jk1out, function(x) x$fit$fit$convergence)
    
    jk1included <- (1-1*(jk1coefs[,1]==ifelse(repar,log(uplimBeta),uplimBeta)))*(rowSums(jk1scores^2)<jkscorethresh)*(1-(jk1conv==1))*(jk1coefs[,1]>=jkvalrange[1])*(jk1coefs[,1]<=jkvalrange[2])
    
    jk1coef <- length(setnrs)*mainfit$fit$par-(length(setnrs)-1)*colMeans(jk1coefs[jk1included==1,, drop=FALSE])
    
    jk1details <- data.frame(set=setnrs, included=jk1included,conv=jk1conv,coef=jk1coefs,score=jk1scores)
    
    jackknife1 <- list(coef=jk1coef, details=jk1details)
    close(pb)
    
  } else {
    jackknife1 <- NULL
  }
  
  
  
  
  if(doJK2){
    allpairs <- t(combn(setnrs,2))
    
    message("Second order jackknife:")
    pb2 <- txtProgressBar(min = 1, max = nrow(allpairs), style = 3)
    
    outfunjk2 <- function(pair) {
      jk2fit <- linearERRfit(data=data[!(data[,set]%in% pair),], doses=doses, set=set, status=status, loc=loc, corrvars=corrvars, repar=repar, initpars=initpars, fitopt=fitopt, ccmethod=ccmethod, fitNull=FALSE, uplimBeta=uplimBeta)
      jk2score <- tryCatch(linERRscore(params=jk2fit$fit$par, data=data[!(data[,set]%in% pair),], doses=doses, set=set, status=status, loc=loc, corrvars=corrvars, repar=repar,ccmethod=ccmethod)$U, error=function(e) rep(NA, length(jk2fit$fit$fullcoef)))
      list(fit=jk2fit, score=jk2score)
    }
    jk2out <- lapply(1:nrow(allpairs), function(k){
      setTxtProgressBar(pb2, k)
      outfunjk2(allpairs[k,])
    })
    
    jk2coefs <- sapply(jk2out, function(x) x$fit$fit$par)
    jk2coefs <- as.data.frame(matrix(jk2coefs, nrow=nrow(allpairs),byrow=TRUE, dimnames=list(NULL,paste0("coef.",names(jk2out[[1]]$fit$fit$par)))))
    jk2scores <- sapply(jk2out, function(x) x$score)
    jk2scores <- as.data.frame(matrix(jk2scores, nrow=nrow(allpairs),byrow=TRUE, dimnames=list(NULL,paste0("score.",names(jk2out[[1]]$fit$fit$par)))))
    jk2conv <- sapply(jk2out, function(x) x$fit$fit$convergence)
    
    jk2included <- (1-1*(jk2coefs[,1]==ifelse(repar,log(uplimBeta),uplimBeta)))*(rowSums(jk2scores^2)<jkscorethresh)*(1-(jk2conv==1))*(jk2coefs[,1]>=jkvalrange[1])*(jk2coefs[,1]<=jkvalrange[2])
    
    jk2coef <- (length(setnrs)^3*mainfit$fit$par-(2*length(setnrs)^2-2*length(setnrs)+1)*(length(setnrs)-1)*colMeans(jk1coefs[jk1included==1,, drop=FALSE])+(length(setnrs)-1)^2*(length(setnrs)-2)*colMeans(jk2coefs[jk2included == 1,, drop=FALSE]))/(2*length(setnrs)-1)
    
    allpairs <- as.data.frame(allpairs)
    names(allpairs) <- c("set1","set2")
    jk2details <- cbind(allpairs, data.frame(included=jk2included,conv=jk2conv,coef=jk2coefs,score=jk2scores))
    
    jackknife2 <- list(coef=jk2coef, details=jk2details)
    close(pb2)
    
  } else {
    jackknife2 <- NULL
  }
  
  jackknife <- list(firstorder=jackknife1, secondorder=jackknife2)
  
  list(MLE=MLE, jackknife=jackknife)
  
}


#constrained linearERRfit
#' @title Fit linear ERR model
#' @description Fits the linear ERR model on a dataset
#' @param data data frame containing matched case-control data, with a number of columns for doses to different locations, a column containing matched set numbers, a column containing the case's tumor location (value between 1 and the number of locations, with location \eqn{x} corresponding to the \eqn{x}-th column index in \code{doses}) and a column serving as a case-control indicator. Other covariates can also be included, in this case a parameter for each covariate column will be estimated. Hence factor variables need to be converted to dummy variables using \code{model.matrix}. If using \code{ccmethod='meandose'}, a column for tumor location is still required but in this case the column can be a vector of ones.
#' @param doses vector containing the indices of columns containing dose information.
#' @param set column index containing matched set numbers.
#' @param status column index containing case status.
#' @param loc column index containing the location of the matched set's case's second tumor.
#' @param corrvars vector containing the indices of columns containing variables to be corrected for.
#' @param repar reparametrize to \eqn{\beta=exp(\xi)}? Defaults to \code{FALSE}
#' @param ccmethod choice of method of analysis: one of meandose, CCML, CCAL or CL. Defaults to CCAL
#' @param initpars initial values for parameters, default is 0 for all parameters. If supplying a different vector, use a vector with an initial value for \eqn{\beta} or \eqn{\xi}, one for all of the other location effects and one for each other covariate (in that order). Note that if \code{repar=TRUE}, the initial value is used for \eqn{\xi}.
#' @param fitopt list with options to pass to \code{control} argument of optimizer
#' @param fitNull boolean: also fit model without dose effect? Defaults to \code{TRUE}. Note: the same optimization algorithm that was used for the MLE will be used for the null model, even if the null model only has one parameter (see details)
#' @param useOld if TRUE, a previous (slower) implementation of the log-likelihood function will be used. Defaults to \code{FALSE}
#' @param uplimBeta upper limit for \eqn{\beta=exp(\xi)}, default value 5. This is used for constraining the MLE estimation in some settings and for the jackknife inclusion criteria, and can be infinite except when Brent optimization is used (see help for \code{linearERR})
#' @return Object with components:
#' \item{fit}{object produced by \code{mle2}}
#' \item{nullfit}{fit without dose effect produced by \code{mle2}}
#' \item{proflik}{profile likelihood: one-dimensional function of \eqn{\beta} or \eqn{\xi}. Note that the optimization used is the same as for the MLE, leading to one-dimensional Nelder-Mead optimization in certain cases (see details of \code{linearERR})}
#' @importFrom numDeriv hessian
#' @importFrom stats constrOptim
#' @details This is a stripped down version of \code{linearERR}, and should only be used when that function does not suffice. For more details refer to the help of \code{linearERR}.
#' @seealso linearERR
#' @examples
#' data(linearERRdata1)
#'
#' fitmeandose <- linearERRfit(data=linearERRdata1, set=1, doses=2:6,
#' status=8, loc=7, corrvars=9, repar=FALSE, ccmethod="meandose")
#'
#' fitCCML <- linearERRfit(data=linearERRdata1, set=1, doses=2:6,
#' status=8, loc=7, corrvars=9, repar=FALSE, ccmethod="CCML")
#'
#' fitCCAL <- linearERRfit(data=linearERRdata1, set=1, doses=2:6,
#' status=8, loc=7, corrvars=9, repar=FALSE, ccmethod="CCAL")
#'
#' fitCL <- linearERRfit(data=linearERRdata1, set=1, doses=2:6,
#' status=8, loc=7, corrvars=NULL, repar=FALSE, ccmethod="CL")
#'
#' fitmeandose$fit$par
#' fitCCML$fit$par
#' fitCCAL$fit$par
#' fitCL$fit$par
#' @export


linearERRfit_constrained <- function(data, doses, set, status, loc, corrvars=NULL, repar=FALSE, ccmethod="CCAL", initpars=rep(0,length(doses)+length(corrvars)), fitopt=list(maxit=5000), fitNull=TRUE, useOld=FALSE, uplimBeta=5, constrained_value){
  
  if(ccmethod=="CL") corrvars <- NULL
  
  if(ccmethod %in% c("CCML", "meandose") & length(corrvars)==0){
    opt_method <- "Brent"
    if(is.infinite(uplimBeta)) stop("Please provide a finite value for uplimBeta to use in Brent optimization")
  } else{
    opt_method <- "constrOptim"
    if(repar & is.infinite(uplimBeta)) stop("uplimBeta needs to be finite for the current settings")
  }
  
  if(is.null(fitopt$maxit)){
    fitopt <- c(fitopt, list(maxit=5000))
  }
  if(is.null(fitopt$reltol)){
    fitopt <- c(fitopt, list(reltol=1e-10))
  }
  # if (is.null(fitopt$pgtol) & opt_method=="L-BFGS-B"){
  #   fitopt <- c(fitopt, list(pgtol=1e-8))
  # }
  # if (is.null(fitopt$factr) & opt_method=="L-BFGS-B"){
  #   fitopt <- c(fitopt, list(factr=1e4))
  # }
  # if (is.null(fitopt$ndeps) & opt_method=="L-BFGS-B"){
  #   parlen <- 1+ifelse(ccmethod %in% c("CCAL","CL"),length(doses)-1,0)+length(corrvars)
  #   fitopt <- c(fitopt, list(ndeps=rep(1e-5, parlen)))
  # }
  
  likfun <- function(params){
    if(repar) params[1] <- exp(params[1])
    if(useOld){
      linERRloglikold(params, data=data, set=set, doses=doses,status=status,loc=loc,corrvars=corrvars, ccmethod=ccmethod)
    } else {
      linERRloglik(params, data=data, set=set, doses=doses,status=status,loc=loc,corrvars=corrvars, ccmethod=ccmethod)
    }
  }
  scorefun <- function(params){
    linERRscore(params, data=data, set=set, doses=doses,status=status,loc=loc,corrvars=corrvars, ccmethod=ccmethod, repar=repar)
  }
  
  if(opt_method!="Brent"){
    names <- c(ifelse(repar,"xi","beta"), paste0("alpha",2:length(doses)),names(data)[corrvars])
    names(initpars) <- names
    #parnames(likfun) <- names
  } else {
    names <- ifelse(repar, "xi","beta")
    initpars <- list(params=as.numeric(initpars[1]))
    names(initpars) <- names
    #parnames(likfun) <- names
  }
  
  strt <- initpars
  
  if(ccmethod=="CCAL"){
    fxd <- NULL
    
    lw <- sapply(names,FUN=function(x) -Inf)
    if(!repar) lw[1] <- -1/max(data[,doses])+.0001
    if(repar){
      up <- list(xi=log(uplimBeta))
    } else {
      up <- list(beta=uplimBeta)
    }
    
  } else if (ccmethod=="CL"){
    #strt <- strt[names(strt)%in% c("xi","beta", paste0("alpha",2:length(doses)))]
    fxd <- NULL
    
    lw <- sapply(names[!names %in% names(data)[corrvars]],FUN=function(x) -Inf)
    if(!repar) lw[1] <- -1/max(data[data[,status]==1,doses])+.0001
    if(repar){
      up <- list(xi=log(uplimBeta))
    } else {
      up <- list(beta=uplimBeta)
    }
    
  } else if(ccmethod=="CCML"){
    if(opt_method=="constrOptim"){
      fxd <- sapply(paste0("alpha",2:length(doses)), function(i) 0)
      lw <- sapply(c(ifelse(repar,"xi","beta"),tail(names, length(corrvars))),FUN=function(x) -Inf)
      if(!repar) {lw[1] <- -1/max(data[,doses][cbind(1:nrow(data), data[,loc])])+.0001}
      #if(length(corrvars)==0){
      if(repar){
        up <- list(xi=log(uplimBeta))
      } else {
        up <- list(beta=uplimBeta)
      }
    } else if(opt_method=="Brent") {
      fxd <- NULL
      lw <- list(params=ifelse(repar, -10,  -1/max(data[,doses][cbind(1:nrow(data), data[,loc])])+.0001))
      up <- list(params=ifelse(repar,log(uplimBeta),uplimBeta))
    } #else { # Nelder-Mead
    #   fxd <- sapply(paste0("alpha",2:length(doses)), function(i) 0)
    #   lw <-NULL
    #   up <- NULL
    #}
    #} else{
    #  up <- NULL
    #}
  } else if(ccmethod=="meandose"){
    if(opt_method=="constrOptim"){
      fxd <- sapply(paste0("alpha",2:length(doses)), function(i) 0)
      lw <- sapply(c(ifelse(repar,"xi","beta"),tail(names, length(corrvars))),FUN=function(x) -Inf)
      if(!repar) {lw[1] <- -1/max(rowMeans(data[,doses]))+.0001}
      #if(length(corrvars)==0){
      if(repar){
        up <- list(xi=log(uplimBeta))
      } else {
        up <- list(beta=uplimBeta)
      }
    } else if(opt_method=="Brent") {
      fxd <- NULL
      lw <- list(params=ifelse(repar, -10,  constrained_value - 0.0001))
      up <- list(params=ifelse(repar,log(uplimBeta), constrained_value))
    }# else { # Nelder-Mead
    #   fxd <- sapply(paste0("alpha",2:length(doses)), function(i) 0)
    #   up <- NULL
    #   lw <- NULL
    # }
    #} else{
    #  up <- NULL
    #}
  }
  
  
  
  if(opt_method=="constrOptim"){
    strt0 <- strt[!names(strt)%in%names(fxd)]
    strt[names(fxd)] <- unlist(fxd)
    if(repar){
      #fit <- mle2(likfun, start=strt,fixed=fxd, method=opt_method,control=fitopt, vecpar=TRUE, parnames=names)
      ui <- matrix(c(-1,rep(0,length(strt0)-1)),nrow=1)
      ci <- -1*log(uplimBeta)
    } else {
      ui <- matrix(rbind(c(1,rep(0,length(strt0)-1)), c(-1,rep(0,length(strt0)-1))), nrow = 2)
      #ci <- c(-0.00039, -0.04001)
      #ci <- c(-0.00001, -0.00001)
      ci <- c((constrained_value - 0.0001), -(constrained_value + 0.0001))
    }
    betathresh <- ci
    
    fit <- constrOptim(strt0,function(x){
      y <- strt
      y[names(x)] <- x
      likfun(y)
    }, ui=ui, ci=ci, grad=function(x){
      y <- strt
      y[names(x)] <- x
      if(!is.null(fxd)){
        res <-  -scorefun(y[-which(names(y) %in% names(fxd))])$U
        if(any(names(res) %in% names(fxd))) res <- res[-which(names(res) %in% names(fxd))]
      } else {
        res <-  -scorefun(y)$U
      }
      res
    }, control=fitopt, outer.iterations=200, hessian=TRUE)
    tmpcoef <- strt
    tmpcoef[names(fit$par)] <- fit$par
    #fit$par <- tmpcoef
    
    fit$fullcoef <- c(tmpcoef[1],tmpcoef[-1])
    names(fit$fullcoef) <- names
    fit$betathresh <- betathresh
  } else {
    betathresh <- lw
    fit <- optim(initpars, function(x){
      likfun(c(x, rep(0, length(doses)+length(corrvars)-1)))
    }, method=opt_method, lower=lw, upper=up, control=fitopt, hessian=TRUE)
    names(fit$par) <- names
    fit$fullcoef <- fit$par
    fit$betathresh <- betathresh
  }
  
  
  #fit <- mle2(likfun, start=initpars, vecpar=(opt_method!="Brent"),fixed=fxd,upper=up, lower=lw, control=fitopt, method=opt_method)
  
  if(fitNull){
    repar0 <- repar
    repar <- FALSE
    names[1] <- "beta"
    if(opt_method!="Brent" ){
      
      
      fxd2 <- c(fxd, list(beta=0))
      
      
      strt <- sapply(names,FUN=function(x) 0)
      strt0 <- strt[!names(strt)%in%names(fxd2)]
      strt[names(fxd2)] <- unlist(fxd2)
      ui <- matrix(c(-1,rep(0,length(strt0)-1)),nrow=1)
      ci <- -20
      
      nullfit <- constrOptim(strt0,function(x){
        y <- strt
        y[names(x)] <- x
        likfun(y)
      }, ui=ui, ci=ci, grad=function(x){
        y <- strt
        y[names(x)] <- x
        if(!is.null(fxd)){
          res <-  -scorefun(y[-which(names(y) %in% names(fxd))])$U
          res <- res[-which(names(res) %in% names(fxd2))]
        } else {
          res <-  -scorefun(y)$U
          res <- res[-which(names(res) %in% c("beta"))]
        }
        
      }, control=fitopt, outer.iterations=200, hessian=TRUE)
      
      
      
      
    } else {
      nullfit <- list(par=0, value=likfun(c(0, rep(0, length(doses)+length(corrvars)-1))), hessian=hessian(function(x) likfun(c(x, rep(0, length(doses)+length(corrvars)-1))), 0))
      
    }
    
    repar <- repar0
    names[1] <- ifelse(repar, "xi","beta")
    
    #
    # lw2 <- lw[-1]
    # up2 <- up[-1]
    # if(length(lw2)==0) lw2 <- NULL
    # if(length(up2)==0) up2 <- NULL
    # nullfit <- mle2(likfun, start=initpars, vecpar=(opt_method!="Brent"),fixed=fxd2,upper=up2, lower=lw2, control=fitoptnull, method=opt_method)
    
  } else {
    nullfit <- NULL
  }
  
  proflik <- function(x){
    if(opt_method!="Brent"){
      
      if(repar){
        fxd3 <- c(fxd, list(xi=x))
      } else {
        fxd3 <- c(fxd, list(beta=x))
      }
      strt <- sapply(names,FUN=function(x) 0)
      strt0 <- strt[!names(strt)%in%names(fxd3)]
      strt[names(fxd3)] <- unlist(fxd3)
      ui <- matrix(c(-1,rep(0,length(strt0)-1)),nrow=1)
      ci <- -20
      
      proffit <- constrOptim(strt0,function(xx){
        y <- strt
        y[names(xx)] <- xx
        likfun(y)
      }, ui=ui, ci=ci, grad=function(xx){
        y <- strt
        y[names(xx)] <- xx
        if(!is.null(fxd)){
          res <-  -scorefun(y[-which(names(y) %in% names(fxd))])$U
          res <- res[-which(names(res) %in% names(fxd3))]
        } else {
          res <-  -scorefun(y)$U
          res <- res[-which(names(res) %in% c("xi","beta"))]
        }
        
      }, control=fitopt, outer.iterations=200, hessian=TRUE)
    } else {
      proffit <- list(par=x, value=likfun(c(x, rep(0, length(doses)+length(corrvars)-1))), hessian=hessian(function(xxx) likfun(c(xxx, rep(0, length(doses)+length(corrvars)-1))), x))
      
      # fitoptprof <- fitopt
      # fxd3 <- list(params=x)
    }
    # lw3 <- lw[-1]
    # up3 <- up[-1]
    # if(length(lw3)==0) lw3 <- NULL
    # if(length(up3)==0) up3 <- NULL
    
    
    proffit$value
    #mle2(likfun, start=initpars, vecpar=(opt_method!="Brent"),fixed=fxd3,upper=up3, lower=lw3, control=fitoptprof, method=opt_method)@min
  }
  
  
  list(fit=fit, nullfit=nullfit, proflik=proflik)
  
}

#' @title Derive the Firth-corrected estimate for the linear ERR model
#' @description Finds roots to the Firth-corrected score equations for the linear ERR model using a matched case-control study.
#' @param data data frame containing matched case-control data, with a number of columns for doses to different locations, a column containing matched set numbers, a column containing the case's tumor location (value between 1 and the number of locations, with location \eqn{x} corresponding to the \eqn{x}-th column index in \code{doses}) and a column serving as a case-control indicator. Other covariates can also be included, in this case a parameter for each covariate column will be estimated. Hence factor variables need to be converted to dummy variables using \code{model.matrix}. If using \code{ccmethod='meandose'}, a column for tumor location is still required but in this case the column can be a vector of ones.
#' @param doses vector containing the indices of columns containing dose information.
#' @param set column index containing matched set numbers.
#' @param status column index containing case status.
#' @param loc column index containing the location of the matched set's case's second tumor.
#' @param corrvars vector containing the indices of columns containing variables to be corrected for. Not used with \code{ccmethod='CL'}
#' @param repar reparametrize to \eqn{\beta=exp(\xi)}? It is recommended to reparametrize when using CL or CCAL or when using additional covariates. Defaults to \code{FALSE}
#' @param ccmethod choice of method of analysis: one of meandose, CCML, CCAL or CL. Defaults to CCAL
#' @param initpars initial values for parameters, default is 0 for all parameters. If supplying a different vector, use a vector with an initial value for all free parameters (\eqn{\beta} or \eqn{\xi}, one for each location effect (except the reference) when using CL or CCAL, and for each other covariate if applicable, in that order). Note that if \code{repar=TRUE}, the first initial value is used for \eqn{\xi}.
#' @param lowerlim lower bound for model parameters, in the same order as \code{initpars}. At least one upper or lower limit needs to be finite. Note that when \code{repar=TRUE}, the first entry is the lower limit for \eqn{\xi}. When \code{repar=FALSE}, the lower limit for \eqn{\beta} cannot be smaller than \eqn{-1/max(d)} where the maximum is taken among all relevant doses for the chosen \code{ccmethod}. If this is the case, the limit will automatically be changed to that value
#' @param upperlim upper bound for model parameters, in the same order as \code{initpars}. At least one upper or lower limit needs to be finite. Note that when \code{repar=TRUE}, the first entry is the upper limit for \eqn{\xi}. When \code{repar=TRUE}, if no other lower or upper limit is given as input, an upper limit of \eqn{\log(5)} will be used for \eqn{\xi}
#' @param fitopt list with options to pass to \code{control} argument of optimizer (see details)
#' @return \code{optim} object with fit results.
#' @references David Firth, Bias reduction of maximum likelihood estimates, Biometrika, Volume 80, Issue 1, March 1993, Pages 27–38, \href{https://doi.org/10.1093/biomet/80.1.27}{https://doi.org/10.1093/biomet/80.1.27}
#' @importFrom stats optim constrOptim
#' @details This function looks for roots of the Firth-corrected score functions.
#'
#' The underlying model is HR=\eqn{\sum(1+\beta d_l)exp(\alpha_l+X^T\gamma)}, where the sum is over organ locations. Here \eqn{\beta} is the dose effect, \eqn{\alpha} are the location effects and \eqn{\gamma} are other covariate effects. The model can be reparametrized to HR=\eqn{\sum(1+exp(\xi) d_l)exp(\alpha_l+X^T\gamma)} using \code{repar=TRUE}. In the original parametrization, \eqn{\beta} is constrained such that HR cannot be negative. There are different choices for the design used to estimate the parameters: mean organ dose, CCML, CL, and CCAL. Mean organ dose (\code{ccmethod='meandose'}) uses the mean of the supplied location doses and compares that mean dose between case and matched controls. The other choices (CCML, CL and CCAL) use the tumor location for the case and compare either only between patients (CCML), only within patients (CL) or both between and within patients (CCAL). CCML only compares the same location between patients, and hence cannot be used to estimate location effects. Similarly, CL compares within patients and cannot be used to estimate covariate effects other than dose, meaning \code{corrvars} should not be supplied for CL. For this model, the Firth correction (Firth 1993) is used as a method for bias correction, or for obtaining an estimate when there is separation in the data.
#'
#' To avoid using unstable multidimensional root finders, this function minimizes the square L2 norm of the modified score instead. This is done using the \code{optim} function. If desired, it is possible to use \code{linERRscore} and optimize or search for roots directly. For one-dimensional models (i.e., mean dose or CCML without additional covariates), the Brent algorithm is used with the user-supplied search interval (\code{lowerlim},\code{upperlim}). Note that the choice for search interval is crucial as this determines convergence. For this reason, there is no default setting in this case. For other optimizations, the L-BFGS-B algorithm (with constraints \code{lowerlim} and \code{upperlim}) is used. For details refer to the function optim, also for \code{fitopt} settings. When \code{repar=FALSE}, if the lower bound for \eqn{\beta} is set too small, it is automatically changed according to the positivity constraint for HR.
#'
#' It is advisable to interpret the results with caution. It was found that the modified score function sometimes has multiple roots, which makes setting initial values and search intervals crucial. It is recommended to try different settings for these inputs. Further, it seemed that reparametrizing improved the performance for multidimensional models.
#' @examples
#' data(linearERRdata1)
#'
#' fitMLE <- linearERR(data=linearERRdata1,doses=2:6,set=1,status=8,loc=7,
#' corrvars=9,repar=TRUE,ccmethod="CCAL",profCI=FALSE)
#'
#' fitfirth <- linearERRfirth(data=linearERRdata1,doses=2:6,set=1,status=8,loc=7,
#' corrvars=9,repar=TRUE,ccmethod="CCAL",initpars=fitMLE$MLE$coef)
#'
#' data.frame(MLE=fitMLE$MLE$coef, Firth=fitfirth$par)
#'
#' @export


linearERRfirth_info <- function(data, doses, set, status, loc, corrvars=NULL, repar=FALSE, ccmethod="CCAL", initpars=NULL,lowerlim=NULL, upperlim=NULL, fitopt=list(maxit=5000)){
  
  if(ccmethod=="CL" & !is.null(corrvars)) stop("corrvars needs to be set to NULL when using CL")
  #if(ccmethod=="CL") corrvars <- NULL
  
  
  if(is.null(initpars)){
    if(ccmethod %in%c("CCML","meandose")) initpars <- rep(0,1+length(corrvars))
    if(ccmethod %in%c("CCAL","CL")) initpars <- rep(0,length(doses)+length(corrvars))
  }
  
  if(is.null(lowerlim)) lowerlim <- rep(-Inf, length(initpars))
  if(is.null(upperlim)) {
    upperlim <- rep(Inf, length(initpars))
    if(repar) upperlim[1] <- log(5)
  }
  
  
  if(ccmethod%in%c("CCML","meandose") & length(initpars)!=(length(corrvars)+1)) stop("Length of initpars incorrect. Please provide an initial value for the dose effect and one for each of the other covariates.")
  if(ccmethod%in%c("CCML","meandose") & length(lowerlim)!=(length(corrvars)+1)) stop("Length of lowerlim incorrect. Please provide a lower bound for the dose effect and one for each of the other covariates.")
  if(ccmethod%in%c("CCML","meandose") & length(upperlim)!=(length(corrvars)+1)) stop("Length of upperlim incorrect. Please provide an upper bound for the dose effect and one for each of the other covariates.")
  
  if(ccmethod%in%c("CL","CCAL") & length(initpars)!=(length(corrvars)+length(doses))) stop("Length of initpars incorrect. Please provide an initial value for the dose effect, for all but one of the alphas, and one for each of the other covariates if used.")
  if(ccmethod%in%c("CL","CCAL") & length(lowerlim)!=(length(corrvars)+length(doses))) stop("Length of lowerlim incorrect. Please provide a lower bound for the dose effect, for all but one of the alphas, and one for each of the other covariates if used.")
  if(ccmethod%in%c("CL","CCAL") & length(upperlim)!=(length(corrvars)+length(doses))) stop("Length of upperlim incorrect. Please provide an upper bound for the dose effect, for all but one of the alphas, and one for each of the other covariates if used.")
  
  
  
  
  scorefun <- function(params){
    linERRscore(params=params,data=data, doses=doses, set=set, status=status, loc=loc, ccmethod=ccmethod, corrvars=corrvars, repar=repar)
  }
  
  
  if(ccmethod %in% c("CCML", "meandose") & length(corrvars)==0){
    opt_method <- "Brent"
  } else{
    opt_method <- "constrOptim"
  }
  
  if(opt_method=="Brent" & length(initpars)>1) initpars=initpars[1]
  
  lw <- lowerlim
  up <- upperlim
  if(opt_method=="Brent" & any(is.infinite(c(lw,up)))) stop("Please provide finite search bounds for Brent")
  
  if(is.null(fitopt$maxit)){
    fitopt <- c(fitopt, list(maxit=5000))
  }
  if(is.null(fitopt$reltol)){
    fitopt <- c(fitopt, list(reltol=1e-10))
  }
  # if (is.null(fitopt$pgtol) & opt_method=="L-BFGS-B"){
  #   fitopt <- c(fitopt, list(pgtol=1e-8))
  # }
  # if (is.null(fitopt$factr) & opt_method=="L-BFGS-B"){
  #   fitopt <- c(fitopt, list(factr=1e4))
  # }
  # if (is.null(fitopt$ndeps) & opt_method=="L-BFGS-B"){
  #   fitopt <- c(fitopt, list(ndeps=rep(1e-5, length(initpars))))
  # }
  
  
  
  if(ccmethod=="CCAL"){
    if(!repar) lw[1] <- max(lw[1],-1/max(data[,doses])+.0001)
  } else if (ccmethod=="CL"){
    if(!repar) lw[1] <- max(lw[1],-1/max(data[data[,status]==1,doses])+.0001)
  } else if(ccmethod=="CCML"){
    if(!repar) {lw[1] <- max(lw[1],-1/max(data[,doses][cbind(1:nrow(data), data[,loc])])+.0001)}
    
  } else if(ccmethod=="meandose"){
    if(!repar) {lw[1] <- max(lw[1],-1/max(rowMeans(data[,doses]))+.0001)}
  }
  
  if(opt_method=="Brent"){
    
    fit <- optim(initpars,
                 function(x){
                   tmp <- scorefun(x)
                   sum((tmp$U+tmp$A)^2)
                 },method=opt_method, control=fitopt, lower=lw, upper=up)
  } else {
    
    ui <- rbind(-1*diag(length(initpars)), diag(length(initpars)))
    ci <- c(-1*up, lw)
    
    ui <- ui[which(!is.infinite(ci)),, drop=FALSE]
    ci <- ci[which(!is.infinite(ci))]
    
    fit <- constrOptim(initpars,
                       function(x){
                         tmp <- scorefun(x)
                         sum((tmp$U+tmp$A)^2)
                       }, grad=NULL, control=fitopt, ci=ci,ui=ui, outer.iterations = 200)
  }
  
  infomat_f = -jacobian(function(x){ #first derivative in case of a scalar
    tmp <- scorefun(x)
    scores = tmp$U + tmp$A
  }, fit$par)
  
  
  list(fit = fit, infomat_f = infomat_f)
  
}