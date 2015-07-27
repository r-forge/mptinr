\name{ROCs}
\alias{roc6}
\alias{roc8}
\docType{data}
\title{
Recognition memory ROCs used by Klauer & Kellen (2015)
}
\description{
Data of the meta-analysis on recognition memory ROC (receiver operating characteristic) curves reported in Klauer and Kellen (2015). In total there are 850 individual ROCs, 459 6-point ROCs and 391 8-point ROCs. Both data sets first report responses to old items and then to new item. For both item types the response categories are ordered from sure-new to sure-old. Please always cite the original authors when using this data. 
}
\details{
The source of each data set is given in the \code{exp} column of each data set. The \code{id} column gives a unique id for each data set. For the 6-point ROCs the first 12 columns contain the data, for the 8-point ROCs, the first 16 columns.
}

\source{
The 6-point ROCs contains data from the following sources:

\itemize{
\item \code{Dube_2012-P} and \code{Dube_2012-W}:\cr
Dube, C., & Rotello, C. M. (2012). Binary ROCs in perception and recognition memory are curved. \emph{Journal of Experimental Psychology: Learning, Memory, and Cognition}, 38(1), 130-151. \url{http://doi.org/10.1037/a0024957}
\item \code{heathcote_2006_e1} and \code{heathcote_2006_e2}:\cr
Heathcote, A., Ditton, E., & Mitchell, K. (2006). Word frequency and word likeness mirror effects in episodic recognition memory. \code{Memory & Cognition}, 34(4), 826-838. \url{http://doi.org/10.3758/BF03193430}
\item \code{Jaeger_2013}:\cr
Jaeger, A., Cox, J. C., & Dobbins, I. G. (2012). Recognition confidence under violated and confirmed memory expectations. Journal of Experimental Psychology: General, 141(2), 282-301. \url{http://doi.org/10.1037/a0025687}
\item \code{Koen_2010_pure}:\cr
Koen, J. D., & Yonelinas, A. P. (2010). Memory variability is due to the contribution of recollection and familiarity, not to encoding variability. \emph{Journal of Experimental Psychology: Learning, Memory, and Cognition}, 36(6), 1536-1542. \url{http://doi.org/10.1037/a0020448}
\item \code{Koen_2011}:\cr
Koen, J. D., & Yonelinas, A. P. (2011). From humans to rats and back again: Bridging the divide between human and animal studies of recognition memory with receiver operating characteristics. \emph{Learning & Memory}, 18(8), 519-522. \url{http://doi.org/10.1101/lm.2214511}
\item \code{Koen-2013_full} and \code{Koen-2013_immediate}:\cr
Koen, J. D., Aly, M., Wang, W.-C., & Yonelinas, A. P. (2013). Examining the causes of memory strength variability: Recollection, attention failure, or encoding variability? \emph{Journal of Experimental Psychology: Learning, Memory, and Cognition}, 39(6), 1726-1741. \url{http://doi.org/10.1037/a0033671}
\item \code{Pratte_2010}:\cr
Pratte, M. S., Rouder, J. N., & Morey, R. D. (2010). Separating mnemonic process from participant and item effects in the assessment of ROC asymmetries. \emph{Journal of Experimental Psychology: Learning, Memory, and Cognition}, 36(1), 224-232. \url{http://doi.org/10.1037/a0017682}
\item \code{Smith_2004}:\cr
Smith, D. G., & Duncan, M. J. J. (2004). Testing Theories of Recognition Memory by Predicting Performance Across Paradigms. \emph{Journal of Experimental Psychology. Learning, Memory & Cognition}, 30(3), 615-625.
}

The 8-point ROCs contains data from the following sources:
\itemize{
\item \code{Benjamin_2013}:\cr
Benjamin, A. S., Tullis, J. G., & Lee, J. H. (2013). Criterion Noise in Ratings-Based Recognition: Evidence From the Effects of Response Scale Length on Recognition Accuracy. \emph{Journal of Experimental Psychology: Learning, Memory, and Cognition}, 39, 1601-1608. \url{http://doi.org/10.1037/a0031849}
\item \code{Onyper_2010-Pics} and \code{Onyper_2010-Words}:\cr
Onyper, S. V., Zhang, Y. X., & Howard, M. W. (2010). Some-or-none recollection: Evidence from item and source memory. \emph{Journal of Experimental Psychology: General}, 139(2), 341-364. \url{http://doi.org/10.1037/a0018926}
}

}
\references{
Klauer, K. C., & Kellen, D. (2015). The flexibility of models of recognition memory: The case of confidence ratings. \emph{Journal of Mathematical Psychology}, 67, 8-25. http://doi.org/10.1016/j.jmp.2015.05.002

}
\usage{
data("roc6")
data("roc8")
}
\note{
\bold{Whenever using any of the data available here, please make sure to cite the original sources given in the following.}
}
\examples{
\dontrun{
# This example shows only how to fit the 6-point ROCs
data("roc6")

# 2HTM (2-high threshold model)
htm <- "
(1-Do)*(1-g)*(1-gn1)*(1-gn2)
(1-Do)*(1-g)*(1-gn1)*gn2
(1-Do)*(1-g)*gn1
Do*(1-do1)*(1-do2)  + (1-Do)*g*go1
Do*do1 + (1-Do)*g*(1-go1)*go2
Do*(1-do1)*do2 + (1-Do)*g*(1-go1)*(1-go2)

Dn*(1-dn1)*dn2 + (1-Dn)*(1-g)*(1-gn1)*(1-gn2)
Dn*dn1 + (1-Dn)*(1-g)*(1-gn1)*gn2
Dn*(1-dn1)*(1-dn2) + (1-Dn)*(1-g)*gn1
(1-Dn)*g*go1
(1-Dn)*g*(1-go1)*go2
(1-Dn)*g*(1-go1)*(1-go2)
"

# full 2HTM is over-parametereized:
check.mpt(textConnection(htm))
# apply some symmetric response mapping restrictions for D and g:
check.mpt(textConnection(htm), list("dn2 = do2", "gn2 = go2"))


# UVSD (unequal variance signal detection model)
uvsd <- "
pnorm(c1, mu, sigma)
pnorm(c1+c2, mu, sigma) - pnorm(c1, mu, sigma)
pnorm(c3+c2+c1, mu, sigma) - pnorm(c2+c1, mu, sigma)
pnorm(c4+c3+c2+c1, mu, sigma) - pnorm(c3+c2+c1, mu, sigma)
pnorm(c5+c4+c3+c2+c1, mu, sigma) - pnorm(c4+c3+c2+c1, mu, sigma)
1 - pnorm(c5+c4+c3+c2+c1, mu, sigma)
  
pnorm(c1)
pnorm(c2+c1) - pnorm(c1)
pnorm(c3+c2+c1) - pnorm(c2+c1)
pnorm(c4+c3+c2+c1) - pnorm(c3+c2+c1)
pnorm(c5+c4+c3+c2+c1) - pnorm(c4+c3+c2+c1)
1 - pnorm(c5+c4+c3+c2+c1)
"

# confidence criteria are parameterized as increments:
check.mpt(textConnection(uvsd))
# c1 = [-Inf, Inf]
# c2, c3, c4, c5 = [0, Inf]
# mu = [-Inf, Inf]
# sigma = [0, Inf]


# MSD (mixture signal detection model):
msd <- "
l*(pnorm(c1-m))  +  (1 - l) * (pnorm(c1-m2))
l*(pnorm(c1+c2-m) - pnorm(c1-m)) + (1 - l) * (pnorm(c1+c2-m2) - pnorm(c1-m2))
l*(pnorm(c1+c2+c3-m)-pnorm(c1+c2-m)) + (1-l)*(pnorm(c1+c2+c3-m2)-pnorm(c1+c2-m2))
l*(pnorm(c1+c2+c3+c4-m)-pnorm(c1+c2+c3-m)) + (1-l)*(pnorm(c1+c2+c3+c4-m2)-pnorm(c1+c2+c3-m2))
l*(pnorm(c1+c2+c3+c4+c5-m)-pnorm(c1+c2+c3+c4-m))+(1-l)*(pnorm(c1+c2+c3+c4+c5-m2)-pnorm(c1+c2+c3+c4-m2))
l*(1-pnorm(c1+c2+c3+c4+c5-m))  +  (1 - l) * (1-pnorm(c1+c2+c3+c4+c5-m2))

pnorm(c1)
pnorm(c1+c2) - pnorm(c1)
pnorm(c1+c2+c3) - pnorm(c1+c2)
pnorm(c1+c2+c3+c4) - pnorm(c1+c2+c3)
pnorm(c1+c2+c3+c4+c5) - pnorm(c1+c2+c3+c4)
1-pnorm(c1+c2+c3+c4+c5)
"
# confidence criteria are again parameterized as increments:
check.mpt(textConnection(msd))
# c1 = [-Inf, Inf]
# c2, c3, c4, c5 = [0, Inf]
# lambda = [0, 1]
# m, m2 = [-Inf, Inf]


# DPSD (dual-process signal detection model)
dpsd <- "
(1-R)*pnorm(c1- mu)
(1-R)*(pnorm(c1 + c2 - mu) - pnorm(c1 - mu))
(1-R)*(pnorm(c1 + c2 + c3 - mu) - pnorm(c1 + c2 - mu))
(1-R)*(pnorm(c1 + c2 + c3 + c4 - mu) - pnorm(c1 + c2 + c3 - mu))
(1-R)*(pnorm(c1 + c2 + c3 + c4 + c5 - mu) - pnorm(c1 + c2 + c3 + c4 - mu))
R + (1-R)*(1 - pnorm(c1 + c2 + c3 + c4 + c5 - mu))

pnorm(c1)
pnorm(c1 + c2) - pnorm(c1)
pnorm(c1 + c2 + c3) - pnorm(c1 + c2)
pnorm(c1 + c2 + c3 + c4) - pnorm(c1 + c2 + c3)
pnorm(c1 + c2 + c3 + c4 + c5) - pnorm(c1 + c2 + c3 + c4)
1 - pnorm(c1 + c2 + c3 + c4 + c5)
"

check.mpt(textConnection(dpsd))
# c1 = [-Inf, Inf]
# c2, c3, c4, c5 = [0, Inf]
# mu, mu2 = [-Inf, Inf]
# R = [0, 1]

uvsd_fit <- fit.model(roc6[,1:12], textConnection(uvsd),
            lower.bound=c(-Inf, rep(0, 5), 0.001), upper.bound=Inf)

msd_fit <- fit.model(roc6[,1:12], textConnection(msd),
            lower.bound=c(-Inf, rep(0, 7)), upper.bound=c(rep(Inf, 5), 1, Inf, Inf))

dpsd_fit <- fit.model(roc6[,1:12], textConnection(dpsd),
            lower.bound=c(-Inf, rep(0, 6)), upper.bound=c(rep(Inf, 6), 1))

htm_fit <- fit.mpt(roc6[,1:12], textConnection(htm), 
                   list("dn2 = do2", "gn2 = go2"))

select.mpt(list(uvsd_fit, dpsd_fit, msd_fit, htm_fit))
# Note that the AIC and BIC results do not adequately take model flexibility into account.
##      model n.parameters G.Squared.sum df.sum p.sum p.smaller.05
## 1 uvsd_fit            7      1820.568   1377     0           50
## 2 dpsd_fit            7      2074.188   1377     0           64
## 3  msd_fit            8      1345.595    918     0           51
## 4  htm_fit            9      1994.217    459     0          138
##   delta.AIC.sum wAIC.sum AIC.best delta.BIC.sum wBIC.sum BIC.best
## 1        0.0000        1      230        0.0000        1      273
## 2      253.6197        0      161      253.6197        0      183
## 3      443.0270        0       16     4996.8517        0        3
## 4     2009.6489        0       56    11117.2982        0        4
}
}
\keyword{datasets}
