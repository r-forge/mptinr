\name{bmpt-class}
\Rdversion{1.1}
\docType{class}
\alias{bmpt-class}
\alias{mpt-class}
\alias{default.ci,mpt-method}
\alias{default.ci<-,mpt-method}
\alias{estimates,mpt-method}
\alias{goodness.of.fit,mpt-method}
\alias{information.criteria,mpt-method}
\alias{log.likelihood,mpt-method}
\alias{observed.data,mpt-method}
\alias{predicted.data,mpt-method}
\alias{show,mpt-method}

\title{Class \code{"bmpt"}}
\description{
Objects holding the results of fitting multinomial procesing tree (MPT) models. \code{bmpt} objects are fits to models which are member of L-BMPT (\code{\link{bmpt.model-class}}; see also Purdy & Batchelder, 2009), \code{mpt} objects to those binary MPTs that are not represented in a way that is compatible to L-BMPT.
}
\section{Objects from the Class}{
Objects can be created by a call to \code{\link{fit.mpt}}. They hold all information: the model as a model object, the data, and the results from the fitting (e.g., parameters, likelihood values,...).
}
\section{Slots}{
  \describe{
    \item{\code{model}:}{The model object, see \code{\link{bmpt.model-class}}}
    \item{\code{observed.data}:}{Array. The data entered.}
    \item{\code{predicted.data}:}{Aray. The predicted data.}
    \item{\code{C.matrix}:}{A list holding the "C" matrices which contain the relative frequency of data per branch and tree.}
    \item{\code{g2}:}{Numeric. The G^2 value of the fitted model.}
    \item{\code{log.likelihood}:}{Numeric. The (-2*) Log-Likelihood of the fitted model.}
    \item{\code{estimates}:}{Matrix. The parameter estimates of the fitted model.}
    \item{\code{multifit}:}{Logical. Does the data contain more than one individual dataset?}
    \item{\code{hessian}:}{List of Hessian matrices}
    \item{\code{default.ci}:}{Numeric. Standard size of confidence intervals.}

    \item{\code{typeHessian}:}{List containing the type(s) of Hessian matrix. Can be either, \code{"expected"} or \code{"observed"}. Only in class \code{bmpt}.}
    \item{\code{hfail}:}{Numeric. Did calculation of expected hessian fail (=1)? Only in class \code{bmpt}.}
    \item{\code{fia}:}{List of Fisher Information Approximation results. Currently not implemented. Only in class \code{bmpt}.}
    \item{\code{parametric.ci}:}{List containing the parametric bootstrapped confidence intervall. Not yet implemented. Only in class \code{bmpt}.}
    \item{\code{nonparametric.ci}:}{List containing the parametric bootstrapped confidence intervall. Not yet implemented. Only in class \code{bmpt}.}

  }
}
\section{Extends}{
\code{bmpt} extends \code{mpt}.
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "mpt")}: Method for displaying overall results.}
    \item{parameters}{see \code{\link{parameters-methods}}}
    \item{goodness.of.fit}{\code{signature(object = "mpt")}: Function to compute and display the (-2 *) Log-Likelihood, the G^2, the degrees of freedom and the p for the model fit.}
    \item{information.criteria}{\code{signature(object = "mpt")}: Function to compute and display AIC, BIC, and (when computed) FIA.}
    \item{default.ci}{\code{signature(object = "mpt")}: Accessor function for the default.ci slot. A replacement function exists as well!}
    \item{observed.data}{\code{signature(object = "mpt")}: Accessor function for the observed data.}
    \item{predicted.data}{\code{signature(object = "mpt")}: Accessor function for the predicted data.}
    \item{log.likelihood}{\code{signature(object = "mpt")}: Accessor function for the (-2 *) Log-Likelihood of the fitted model.}
    \item{estimates}{\code{signature(object = "mpt")}: Accesor function for the parameter estimates.}
	 }
}
\author{
Henrik Singmann
}
\seealso{
\code{\link{fit.mpt}}
}
\examples{
showClass("bmpt")
}
\keyword{classes}
