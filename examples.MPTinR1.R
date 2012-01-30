

# The first example fits the MPT model presented in Riefer and Batchelder (1988, Figure 1)
# to the data presented in Riefer and Batchelder (1988, Table 1)
# Note that Riefer and Batchelder (1988, pp. 328) did some hypotheses tests, that are not done here.
# Rather, we use each condition (i.e., row in Table 1) as a different individual.
# We try to use n.optim = 1 here, but this can lead to local minima
# In general we recommend to set n.optim >= 5

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

#fit using a textConnection (i.e., you can specify the model in the code):
model1.connection <- textConnection("p * q * r
p * q * (1-r)
p * (1-q) * r
p * (1-q) * (1-r) + (1-p)")
fit.mpt(rb.fig1.data, model1.connection, n.optim = 1)



# The second example fits the MPT model presented in Riefer and Batchelder (1988, Figure 2)
# to the data presented in Riefer and Batchelder (1988, Table 3)
# First, the model without restrictions is fitted: ref.model
# Next, the model with all r set equal is fitted: r.equal
# Then, the model with all c set equal is fitted: c.equal
# Finally, the inferential tests reported by Riefer & Batchelder, (1988, p. 332) are executed.
# Note, that n.optim = 10, because of frequent local minima.

# get the data
data(rb.fig2.data, package = "MPTinR")

# positions of model and restriction files:
model2 <- system.file("extdata", "rb.fig2.model", package = "MPTinR")
model2r.r.eq <- system.file("extdata", "rb.fig2.r.equal", package = "MPTinR")
model2r.c.eq <- system.file("extdata", "rb.fig2.c.equal", package = "MPTinR")

# The full (i.e., unconstrained) model
(ref.model <- fit.mpt(rb.fig2.data, model2, n.optim = 10))
(ref.model <- fit.model(rb.fig2.data, model2, n.optim = 10))

# All r equal
(r.equal <- fit.mpt(rb.fig2.data, model2, model2r.r.eq, n.optim = 10))
(r.equal <- fit.model(rb.fig2.data, model2, model2r.r.eq, n.optim = 10))

# All c equal
(c.equal <- fit.mpt(rb.fig2.data, model2, model2r.c.eq, n.optim = 10))
(c.equal <- fit.model(rb.fig2.data, model2, model2r.c.eq, n.optim = 10))

# is setting all r equal a good idea?
(g.sq.r.equal <- r.equal[["goodness.of.fit"]][["G.Squared"]] - ref.model[["goodness.of.fit"]][["G.Squared"]])
(df.r.equal <- r.equal[["goodness.of.fit"]][["df"]] - ref.model[["goodness.of.fit"]][["df"]])
(p.value.r.equal <- pchisq(g.sq.r.equal, df.r.equal , lower.tail = FALSE))

# is setting all c equal a good idea?
(g.sq.c.equal <- c.equal[["goodness.of.fit"]][["G.Squared"]] - ref.model[["goodness.of.fit"]][["G.Squared"]])
(df.c.equal <- c.equal[["goodness.of.fit"]][["df"]] - ref.model[["goodness.of.fit"]][["df"]])
(p.value.c.equal <- pchisq(g.sq.c.equal, df.c.equal , lower.tail = FALSE))


## Not run: 

# Example from Broeder & Schuetz (2009)
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
br.2htm.2 <- fit.model(d.broeder, m.2htm, n.optim = 5, control = list(), use.hessian = TRUE, output = "full")
round(br.2htm[["parameters"]][["individual"]] - br.2htm.2[["parameters"]][["individual"]], 2)
round(br.2htm[["goodness.of.fit"]][["individual"]] - br.2htm.2[["goodness.of.fit"]][["individual"]], 2)

br.2htm.ineq <- fit.mpt(d.broeder, m.2htm, i.2htm)
br.2htm.ineq <- fit.model(d.broeder, m.2htm, i.2htm)

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




# compare speed of no multicore versus multicore for multiple optimization runs:

require(snowfall)
# change number of CPUs if more are available
nCPU = 2
sfInit( parallel=TRUE, cpus=nCPU, type = "SOCK" )

# NO multicore
system.time(fit.mpt(rb.fig2.data, model2, model2r.r.eq, n.optim = 2))

# multicore:
system.time(fit.mpt(rb.fig2.data, model2, model2r.r.eq, n.optim = 2, multicore = "n.optim"))

sfStop()

## End(Not run)

  