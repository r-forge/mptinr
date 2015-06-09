require(devtools)
load_all()

data(rb.fig1.data, package = "MPTinR")
model1 <- system.file("extdata", "rb.fig1.model", package = "MPTinR")
# just fit the first dataset:
fit.mpt(rb.fig1.data[1,], model1, n.optim = 1, fia = 100)
fit.model(rb.fig1.data[1,], model1, n.optim = 1, fia = 100)


m1 <- fit.mpt(rb.fig1.data, model1, fia = 100000)
m1$information.criteria$sum
colSums(m1$information.criteria$individual)

select.mpt(list(m1, m1), dataset = 2)

help(package = "MPTinR")
require("MPTinR")
data(d.broeder)
require(snowfall)
m.2htm <- system.file("extdata", "5points.2htm.model", package = "MPTinR")
get.mpt.fia(d.broeder, m.2htm)


require(Rcpp)
compileAttributes() # DO NOT RUN THIS. It changes the names to determinant (without leading .) which needs to be changed by hand!

data(d.broeder, package = "MPTinR")
m.2htm <- system.file("extdata", "5points.2htm.model", package = "MPTinR")
r.2htm <- system.file("extdata", "broeder.2htm.restr", package = "MPTinR")
r.1htm <- system.file("extdata", "broeder.1htm.restr", package = "MPTinR")

br.2htm.fia <- fit.mpt(d.broeder, m.2htm, fia = 50000, fit.aggregated = FALSE)
br.2htm.res.fia <- fit.mpt(d.broeder, m.2htm, r.2htm, fia = 50000, fit.aggregated = FALSE)
br.1htm.fia <- fit.mpt(d.broeder, m.2htm, r.1htm, fia = 50000, fit.aggregated = FALSE)

select.mpt(list("2htm" = br.2htm.fia, "2htm.r" = br.2htm.res.fia, "1htm" = br.1htm.fia), output = "full")
select.mpt(list("2htm" = br.2htm.fia, "2htm.r" = br.2htm.res.fia, "1htm" = br.1htm.fia), output = "standard")


br.2htm.fia2 <- fit.mpt(d.broeder, m.2htm, fia = 50000, fit.aggregated = TRUE)
br.2htm.res.fia2 <- fit.mpt(d.broeder, m.2htm, r.2htm, fia = 50000, fit.aggregated = TRUE)
br.1htm.fia2 <- fit.mpt(d.broeder, m.2htm, r.1htm, fia = 50000, fit.aggregated = TRUE)

select.mpt(list("2htm" = br.2htm.fia2, "2htm.r" = br.2htm.res.fia2, "1htm" = br.1htm.fia2), output = "full")
