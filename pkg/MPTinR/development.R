load_all()

data(rb.fig1.data, package = "MPTinR")
model1 <- system.file("extdata", "rb.fig1.model", package = "MPTinR")
# just fit the first dataset:
fit.mpt(rb.fig1.data[1,], model1, n.optim = 1, fia = 100)
fit.model(rb.fig1.data[1,], model1, n.optim = 1, fia = 100)


require("MPTinR")
data(d.broeder)
m.2htm <- system.file("extdata", "5points.2htm.model", package = "MPTinR")
get.mpt.fia(d.broeder, m.2htm)


require(Rcpp)
compileAttributes()
