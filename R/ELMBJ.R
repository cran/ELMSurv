##' A Kernel Extreme Learning Machine Using the Buckley-James estimator
##' @title  ELMBJ
##' @param x  The covariates(predictor variables) of training data.
##' @param y  Survival time and censored status of training data. Must be a Surv  \code{survival} object
##' @param Regularization_coefficient  Ridge or Tikhonov regularization parameter. Default value for \code{\link{ELMBJEN}} is 10000. It need be set by the user here when using a single base ELM survival model. Also known as \eqn{C} in the ELM paper.
##' @param kerneltype Type of kernel matrix. kerneltype=1,a RBF kernel;kerneltype=2 , a linear kernel;kerneltype=3 ,a polynomial kernel;kerneltype=4, a sigmoid kernel.
##' @param Kernel_para Parameters for different types of kernels. A single value for kerneltype=1 or 2. A vector for kerneltype=3 or 4.
##' @return List of returned values
##'   \tabular{ll}{
##'       \code{trainMSE}    \tab  Mean Square Error(MSE) on training data. \cr
##'       \code{newy} \tab Esitmated survival times of training data by the Buckley-James estimator. \cr
##'       \code{outputWeight} \tab Weights of the output layer in ELM. \cr
##'   }
##' @seealso \code{\link{ELMBJEN}}
##' @author Hong Wang
##' @references
##' \itemize{
##'   \item Hong Wang et al (2018). A Survival Ensemble of Extreme Learning Machine. Applied Intelligence, DOI:10.1007/s10489-017-1063-4.
##'  }
##' @examples
##' set.seed(123)
##' require(ELMSurv)
##' require(survival)
##' #Lung DATA
##' data(lung)
##' lung=na.omit(lung)
##' lung[,3]=lung[,3]-1
##' n=dim(lung)[1]
##' L=sample(1:n,ceiling(n*0.5))
##' trset<-lung[L,]
##' teset<-lung[-L,]
##' rii=c(2,3)
##' #A kernel ELM base model
##' kerelmsurv=ELMBJ(trset[,-rii],Surv(trset[,rii[1]],trset[,rii[2]]))
##' #The traing MSE
##' tr_mse=kerelmsurv$trainMSE
##' #New survival times imputed for training data
##' y_impute=kerelmsurv$newy
##' @export
ELMBJ <- function(x,y, Regularization_coefficient, kerneltype=2,Kernel_para=c(2,1)) 
{ 

  if(missing(Regularization_coefficient)) Regularization_coefficient=10000

  ny <- ncol(y)

  status <- y[, ny]
  survtime = y[, 1L]
  
  
  #imputey
  
  newy = bjimpute(y = survtime, cen = status, x = x,inibeta = NULL)
  
  c = Regularization_coefficient
 
  omega_train = kernmat(x,kerneltype, Kernel_para,NULL)
  #Calculate the mode coefficient beta
  outputWeight = solve(omega_train + diag(rep(1, nrow(x)))/c) %*% newy
  #Calculate the training output
  ypre = omega_train %*% outputWeight 
  
  trainMSE = sqrt(sum((ypre - survtime)^2))
   

    fit <- list()
    fit$trainMSE=trainMSE
	fit$newy=newy
	fit$outputWeight=outputWeight
	fit$trainx=x
	fit$kerneltype=kerneltype
	fit$Kernel_para=Kernel_para  
	
    class(fit) <- "ELMBJ"
    fit  
  
}
