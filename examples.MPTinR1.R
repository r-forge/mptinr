##############################################################
# Example: fit.mpt

\examples{
# The first example fits the MPT model presented in Riefer and Batchelder (1988, Figure 1)
# to the data presented in Riefer and Batchelder (1988, Table 1)
# Note that Riefer and Batchelder (1988, pp. 328) did some hypotheses tests, that are not done here.
# Rather, we use each condition (i.e., row in Table 1) as a different individual.

# load the data
data(rb.fig1.data, package = "MPTinR")

#get the character string with the position of the model:
model1 <- system.file("extdata", "rb.fig1.model", package = "MPTinR")
model1.eqn <- system.file("extdata", "rb.fig1.model.eqn", package = "MPTinR")

# just fit the first "individual":
fit.mpt(rb.fig1.data[1,], model1, n.optim = 1)
fit.model(rb.fig1.data[1,], model1, n.optim = 1)

#fit all "individuals":
fit.mpt(rb.fig1.data, model1, n.optim = 1)
fit.model(rb.fig1.data, model1, n.optim = 1)

#fit all "individuals" using the .EQN model file:
fit.mpt(rb.fig1.data, model1.eqn, n.optim = 1)

#fit using a textConnection (i.e., you can specify the model in your script/code):
model1.txt <- "p * q * r
p * q * (1-r)
p * (1-q) * r
p * (1-q) * (1-r) + (1-p)"
fit.mpt(rb.fig1.data, textConnection(model1.txt), n.optim = 1)



# The second example fits the MPT model presented in Riefer and Batchelder (1988, Figure 2)
# to the data presented in Riefer and Batchelder (1988, Table 3)
# First, the model without restrictions is fitted: ref.model
# Next, the model with all r set equal is fitted: r.equal
# Then, the model with all c set equal is fitted: c.equal
# Finally, the inferential tests reported by Riefer & Batchelder, (1988, p. 332) are executed.

# get the data
data(rb.fig2.data, package = "MPTinR")

# positions of model and restriction files:
model2 <- system.file("extdata", "rb.fig2.model", package = "MPTinR")
model2r.r.eq <- system.file("extdata", "rb.fig2.r.equal", package = "MPTinR")
model2r.c.eq <- system.file("extdata", "rb.fig2.c.equal", package = "MPTinR")

# The full (i.e., unconstrained) model
(ref.model <- fit.mpt(rb.fig2.data, model2))

# All r equal
(r.equal <- fit.mpt(rb.fig2.data, model2, model2r.r.eq))

# All c equal
(c.equal <- fit.mpt(rb.fig2.data, model2, model2r.c.eq))

# is setting all r equal a good idea?
(g.sq.r.equal <- r.equal[["goodness.of.fit"]][["G.Squared"]] - ref.model[["goodness.of.fit"]][["G.Squared"]])
(df.r.equal <- r.equal[["goodness.of.fit"]][["df"]] - ref.model[["goodness.of.fit"]][["df"]])
(p.value.r.equal <- pchisq(g.sq.r.equal, df.r.equal , lower.tail = FALSE))

# is setting all c equal a good idea?
(g.sq.c.equal <- c.equal[["goodness.of.fit"]][["G.Squared"]] - ref.model[["goodness.of.fit"]][["G.Squared"]])
(df.c.equal <- c.equal[["goodness.of.fit"]][["df"]] - ref.model[["goodness.of.fit"]][["df"]])
(p.value.c.equal <- pchisq(g.sq.c.equal, df.c.equal , lower.tail = FALSE))

# You can specify restrictions also via a list instead of an external file:
# All r equal
r.equal.2 <- fit.mpt(rb.fig2.data, model2, list("r0 = r1 = r2= r3 = r4"), n.optim = 5)
all.equal(r.equal, r.equal.2)

# All c equal
c.equal.2 <- fit.mpt(rb.fig2.data, model2, list("c0 = c1 = c2 = c3= c4"))
all.equal(c.equal, c.equal.2)


\dontrun{

# Example from Broder & Schutz (2009)
# We fit the data from the 40 individuals from their Experiment 3
# We fit three different models:
# 1. Their 2HTM model: br.2htm
# 2. A restricted 2HTM model with Dn = Do: br.2htm.res
# 3. A 1HTM model (i.e., Dn = 0): br.1htm
# We fit the models with, as well as without, applied inequality restrictions (see Details)
# that is, for some models (.ineq) we impose: G1 < G2 < G3 < G4 < G5 
# As will be apparent, the inequality restrictions do not hold for all individuals.
# Finally, we compute the FIA for all models, taking inequalities into account when they are imposed.
# Note: The following examples will take some time (> 1 hour).

data(d.broeder, package = "MPTinR")
m.2htm <- system.file("extdata", "5points.2htm.model", package = "MPTinR")
r.2htm <- system.file("extdata", "broeder.2htm.restr", package = "MPTinR")
r.1htm <- system.file("extdata", "broeder.1htm.restr", package = "MPTinR")
i.2htm <- system.file("extdata", "broeder.2htm.ineq", package = "MPTinR")
ir.2htm <- system.file("extdata", "broeder.2htm.restr.ineq", package = "MPTinR")
ir.1htm <- system.file("extdata", "broeder.1htm.restr.ineq", package = "MPTinR")

# fit the original 2HTM
br.2htm <- fit.mpt(d.broeder, m.2htm)
br.2htm.ineq <- fit.mpt(d.broeder, m.2htm, i.2htm)

# do the inequalities hold for all participants?
br.2htm.ineq[["parameters"]][["individual"]][,"estimates",]
br.2htm[["parameters"]][["individual"]][,"estimates",]
# See the difference between forced and non-forced inequality restrictions:
round(br.2htm[["parameters"]][["individual"]][,"estimates",] - br.2htm.ineq[["parameters"]][["individual"]][,"estimates",],2)

# The same for the other two models
# The restricted 2HTM
br.2htm.res <- fit.mpt(d.broeder, m.2htm, r.2htm)
br.2htm.res.ineq <- fit.mpt(d.broeder, m.2htm, ir.2htm)
round(br.2htm.res[["parameters"]][["individual"]][,"estimates",] - br.2htm.res.ineq[["parameters"]][["individual"]][,"estimates",],2)
# The 1HTM
br.1htm <- fit.mpt(d.broeder, m.2htm, r.1htm)
br.1htm.ineq <- fit.mpt(d.broeder, m.2htm, ir.1htm)
round(br.2htm.res[["parameters"]][["individual"]][,"estimates",] - br.2htm.res.ineq[["parameters"]][["individual"]][,"estimates",],2)

# identical to the last fit of the 1HTM (using a list as restriction):
br.1htm.ineq.list <- fit.mpt(d.broeder, m.2htm, list("G1 < G2 < G3 < G4 < G5", "Dn = 0"))
all.equal(br.1htm.ineq, br.1htm.ineq.list)  # TRUE

# These results show that we cannot compute inequality constraints for the non inequality imposed models.
# (It would look differently if we excluded critical cases, e.g., 2, 6, 7, 10, 18, 21, 25, 29, 32, 34, 35, 37, 38)
# Therefore, we get the FIA for the models as computed above 
# WARNING: The following part will take a long time!

br.2htm.fia <- fit.mpt(d.broeder, m.2htm, fia = 200000)
br.2htm.ineq.fia <- fit.mpt(d.broeder, m.2htm, i.2htm, fia = 200000)
br.2htm.res.fia <- fit.mpt(d.broeder, m.2htm, r.2htm, fia = 200000 )
br.2htm.res.ineq.fia <- fit.mpt(d.broeder, m.2htm, ir.2htm, fia = 200000)
br.1htm.fia <- fit.mpt(d.broeder, m.2htm, r.1htm, fia = 200000)
br.1htm.ineq.fia <- fit.mpt(d.broeder, m.2htm, ir.1htm, fia = 200000)

# Model selection using the FIA
(br.select <- select.mpt(list(orig.2htm = br.2htm.fia, orig.2htm.ineq = br.2htm.ineq.fia, res.2htm = br.2htm.res.fia, res.2htm.ineq = br.2htm.res.ineq.fia, orig.1htm = br.1htm.fia, orig.1htm.ineq = br.1htm.ineq.fia)))
# The same results, ordered by FIA
br.select[order(br.select[,"delta.FIA.sum"]),]

# Compare this with the model selection not using FIA:
select.mpt(list(orig.2htm = br.2htm, orig.2htm.ineq = br.2htm.ineq, res.2htm = br.2htm.res, res.2htm.ineq = br.2htm.res.ineq, orig.1htm = br.1htm, orig.1htm.ineq = br.1htm.ineq))


# Only use the aggregated data:
d.broeder.agg <- colSums(d.broeder)
br.2htm.agg <- fit.mpt(d.broeder.agg, m.2htm)
br.2htm.res.agg <- fit.mpt(d.broeder.agg, m.2htm, r.2htm)
br.1htm.agg <- fit.mpt(d.broeder.agg, m.2htm, r.1htm)

select.mpt(list(orig.2htm = br.2htm.agg, res.2htm = br.2htm.res.agg, orig.1htm = br.1htm.agg), output = "full")


# compare speed of no multicore versus multicore for multiple datasets:

require(snowfall)
# change number of CPUs if more are available
nCPU = 2
sfInit( parallel=TRUE, cpus=nCPU, type = "SOCK" )

# NO multicore
system.time(fit.mpt(d.broeder, m.2htm))

# multicore:
system.time(fit.mpt(d.broeder, m.2htm, multicore = "individual"))

sfStop()
}

  }

##############################################################
# Example: fit.model
  
  
\examples{

\dontrun{

# Example from Broder & Schutz (2009)
# We fit the data from the 40 individuals from their Experiment 3
# We fit three different models:
# 1. Their SDT Model: br.sdt
# 2. Their 2HTM model: br.2htm
# 3. A restricted 2HTM model with Dn = Do: br.2htm.res
# 4. A 1HTM model (i.e., Dn = 0): br.1htm

data(d.broeder, package = "MPTinR")
m.2htm <- system.file("extdata", "5points.2htm.model", package = "MPTinR")


# We specify the SDT model in the code using a textConnection.
# Note that a textCconnection can only be used once. Then you need to call it again (e.g., after calling check.mpt)!

m.sdt <- "
1-pnorm((cr1-mu)/ss)
pnorm((cr1-mu)/ss)

1-pnorm(cr1)
pnorm(cr1)

1-pnorm((cr2-mu)/ss)
pnorm((cr2-mu)/ss)

1-pnorm(cr2)
pnorm(cr2)

1-pnorm((cr3-mu)/ss)
pnorm((cr3-mu)/ss)

1-pnorm(cr3)
pnorm(cr3)

1-pnorm((cr4-mu)/ss)
pnorm((cr4-mu)/ss)

1-pnorm(cr4)
pnorm(cr4)

1-pnorm((cr5-mu)/ss)
pnorm((cr5-mu)/ss)

1-pnorm(cr5)
pnorm(cr5)
"

# How does the model look like?
check.mpt(textConnection(m.sdt))

# fit the SDT (unequal variance version)
br.uvsdt <- fit.model(d.broeder, textConnection(m.sdt), lower.bound = c(rep(-Inf, 5), 0, 1), upper.bound = Inf)

# Is there any effect of studying the items?
br.uvsdt.2 <- fit.model(d.broeder, textConnection(m.sdt), restrictions.filename = list("mu = 0", "ss = 1"), lower.bound = -Inf, upper.bound = Inf)

(diff.g2 <- br.uvsdt.2[["goodness.of.fit"]][["sum"]][["G.Squared"]] - br.uvsdt[["goodness.of.fit"]][["sum"]][["G.Squared"]])
(diff.df <- br.uvsdt.2[["goodness.of.fit"]][["sum"]][["df"]] - br.uvsdt[["goodness.of.fit"]][["sum"]][["df"]])
1 - pchisq(diff.g2, diff.df)

# fit the equal variance SDT model:
br.evsdt <- fit.model(d.broeder, textConnection(m.sdt), lower.bound = c(rep(-Inf, 5), 0), upper.bound = Inf, restrictions.filename = list("ss = 1"))

# fit the MPTs (see also ?fit.mpt).
# In contrast to ?fit.mpt we specify the restrictions using textConnections!
br.2htm <- fit.mpt(d.broeder, m.2htm)
br.2htm.res <- fit.mpt(d.broeder, m.2htm, textConnection("Do = Dn"))
br.1htm <- fit.mpt(d.broeder, m.2htm, textConnection("Dn = 0"))

select.mpt(list(uvsdt = br.uvsdt, evsdt = br.evsdt, two.htm = br.2htm, two.htm.res = br.2htm.res, one.htm = br.1htm), output = "full")

# the restricted 2HTM "wins" for individual data (although evsdt does not perform too bad), but the 2htm and restricted 2htm restricted "win" for aggregated data.

}

}



##############################################################
# Example: fit.mptinr

\examples{
\dontrun{
# the example may occasionally fail due to a starting values - integration mismatch.

# Fit an SDT for a 4 alternative ranking task (Kellen, Klauer, & Singmann, 2012).

ranking.data <- structure(c(39, 80, 75, 35, 61, 54, 73, 52, 44, 63, 40, 48, 80,
49, 43, 80, 68, 53, 81, 60, 60, 65, 49, 58, 69, 75, 71, 47, 44,
85, 23, 9, 11, 21, 12, 21, 14, 20, 19, 15, 29, 13, 14, 15, 22,
11, 12, 16, 13, 20, 20, 9, 26, 19, 13, 9, 14, 15, 24, 9, 19,
7, 9, 26, 16, 14, 6, 17, 21, 14, 20, 18, 5, 19, 17, 5, 11, 21,
4, 9, 15, 17, 7, 17, 11, 11, 9, 19, 20, 3, 19, 4, 5, 18, 11,
11, 7, 11, 16, 8, 11, 21, 1, 17, 18, 4, 9, 10, 2, 11, 5, 9, 18,
6, 7, 5, 6, 19, 12, 3), .Dim = c(30L, 4L))

expSDTrank <- function(Q, param.names, n.params, tmp.env){
   
    e <- vector("numeric",4)

    mu <- Q[1]
    ss <- Q[2]
       
    G1<-function(x){
        ((pnorm(x)^3)*dnorm(x,mean=mu,sd=ss))
    }

    G2<-function(x){
        ((pnorm(x)^2)*dnorm(x,mean=mu,sd=ss)*(1-pnorm(x)))*3
    }

     G3<-function(x){
        (pnorm(x)*dnorm(x,mean=mu,sd=ss)*(1-pnorm(x))^2)*3
    }
 

    e[1] <- integrate(G1,-Inf,Inf,rel.tol = .Machine$double.eps^0.5)$value    
    e[2] <- integrate(G2,-Inf,Inf,rel.tol = .Machine$double.eps^0.5)$value
    e[3] <- integrate(G3,-Inf,Inf,rel.tol = .Machine$double.eps^0.5)$value
    e[4] <- 1-e[1]-e[2]-e[3]  
   
    return(e)
}



SDTrank <- function(Q, data, param.names, n.params, tmp.env, lower.bound, upper.bound){
   
    e<-vector("numeric",4)

    mu <- Q[1]
    ss <- Q[2]
       
    G1<-function(x){
        ((pnorm(x)^3)*dnorm(x,mean=mu,sd=ss))
    }

    G2<-function(x){
        ((pnorm(x)^2)*dnorm(x,mean=mu,sd=ss)*(1-pnorm(x)))*3
    }

     G3<-function(x){
        (pnorm(x)*dnorm(x,mean=mu,sd=ss)*(1-pnorm(x))^2)*3
    }
 

    e[1] <- integrate(G1,-Inf,Inf,rel.tol = .Machine$double.eps^0.5)$value    
    e[2] <- integrate(G2,-Inf,Inf,rel.tol = .Machine$double.eps^0.5)$value
    e[3] <- integrate(G3,-Inf,Inf,rel.tol = .Machine$double.eps^0.5)$value
    e[4] <- 1-e[1]-e[2]-e[3]  
   
    LL <- -sum(data[data!=0]*log(e[data!=0]))
    return(LL)
}

fit.mptinr(ranking.data, SDTrank, c("mu", "sigma"), 4, prediction = expSDTrank, lower.bound = c(0,0.1), upper.bound = Inf)
 }
}