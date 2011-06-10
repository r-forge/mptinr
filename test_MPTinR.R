
numerical.accuracy <- 1e-8

# Check Example 1 (?fit.mpt) against data in Paper:
example1 <- function(max.diff) {
    load("pkg/data/rb.fig1.data.rda")
    model1 <- "pkg/inst/extdata/rb.fig1.model"
    model1.eqn <- "pkg/inst/extdata/rb.fig1.model.eqn"

    # just fit the first "individual":
    rb.table1.res <- fit.mpt(rb.fig1.data, model1, n.optim = 1)

    load("testfiles/rb.table1.RData")

    e1.estimate <- round(t(rb.table1.res$parameters$individual[,1,]),2)

    (max(abs(rb.table1 - e1.estimate)) - max.diff) < numerical.accuracy
}

# Check parameter values for proactive inhibition model against reference (Riefer & Batchelder,1988, Table 1)
# Maximal accepted difference in parameter values is <= .01
example1(.01)

# Example 2: Stirage-Retrieval Model from Reifer & batchelder (1988)

example2 <- function() {
load("pkg/data/rb.fig2.data.rda")

model2 <- "pkg/inst/extdata/rb.fig2.model"
model2r.r.eq <- "pkg/inst/extdata/rb.fig2.r.equal"
model2r.c.eq <- "pkg/inst/extdata/rb.fig2.c.equal"

# The full (i.e., unconstrained) model
ref.model <- fit.mpt(rb.fig2.data, model2, n.optim = 10)

# All r equal
r.equal <- fit.mpt(rb.fig2.data, model2, model2r.r.eq, n.optim = 10)

c.equal <- fit.mpt(rb.fig2.data, model2, model2r.c.eq, n.optim = 10)

test.r <- round((r.equal[["goodness.of.fit"]][["G.Squared"]] - ref.model[["goodness.of.fit"]][["G.Squared"]]), 2) - 35.81 < numerical.accuracy

test.c <- round((g.sq.c.equal <- c.equal[["goodness.of.fit"]][["G.Squared"]] - ref.model[["goodness.of.fit"]][["G.Squared"]]),2) - 1.43  < numerical.accuracy
list(test.for.r = test.r, test.for.c = test.c)
}

# This tests compares the likelihood ratios tests for two restricted models (setting either r or c equal)
# with the reference in Batchelder & Riefer (1988, p. 332)

suppressWarnings(suppressMessages(example2()))

example.broeder.ex3 <- function(max.diff, numerical.accuracy) {

    load("pkg/data/d.broeder.rda")
    m.2htm <- "pkg/inst/extdata/5points.2htm.model"
    br.2htm <- fit.mpt(apply(d.broeder,2,sum), m.2htm, n.optim = 5)
    g2 <- (br.2htm$goodness.of.fit[["G.Squared"]] - 2.83 - max.diff) < numerical.accuracy
    ps <- (max(abs(br.2htm$parameters[,1] - c(.45, .56, .14, .22, .40, .60, .69))) - max.diff) < numerical.accuracy
    list(goodness.of.fit = g2, parameters = ps)
}

# This examples compares the data from Broeder & Schütz (2009, Experiment 3) with the reference (Table 4)
example.broeder.ex3(0.01, numerical.accuracy)


