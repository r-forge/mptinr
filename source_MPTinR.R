
for (r.file in list.files("pkg/R", full.names = TRUE)) {
	source(r.file)
}


# Source testfunctions
source("test_MPTinR.R")
numerical.accuracy <- 1e-8

################################################################
## Test 1: Test fitting against references in the literature: ##
################################################################

# Example 1 (?fit.mpt): Proactive Inhibiton Model from Riefer & Batchelder (1988):
# The parameter values for proactive inhibition model are tested against reference (Riefer & Batchelder,1988, Table 1)
# Maximal accepted difference in parameter values is <= .01
suppressWarnings(example1(.01, numerical.accuracy))

# Example 2 (?fit.mpt): Storage-Retrieval Model from Riefer & Batchelder (1988):
# This tests compares the likelihood ratios tests for two restricted models (setting either r or c equal)
# with the reference in Batchelder & Riefer (1988, p. 332)
suppressWarnings(example2(numerical.accuracy))

# Example 3 (?fit.mpt): 2HTM Recognition Model with Data from Bröder & Schütz (2009, Experiment 3)
# This tests compares the G² values and the parameter values from Broeder & Schütz (2009, Experiment 3)
# with the reference in their paper (Table 4)
example.broeder.ex3(0.01, numerical.accuracy)


#######################################################################################
## Test 2: Test fitting of multi-individual fit against reference result from MPTinR ##
#######################################################################################

# current reference was calculated with MPTinR 0.6.3

load("testfiles/bs_ref_063.RData")

example.broeder.mptinr(5)

all.equal(br.2htm.ref[-6], br.2htm.test[-6], tolerance = 0.01)
all.equal(br.2htm.ineq.ref[-6], br.2htm.ineq.test[-6], tolerance = 0.01)

all.equal(br.2htm.res.ref[-6], br.2htm.res.test[-6], tolerance = 0.001)
all.equal(br.2htm.res.ineq.ref[-6], br.2htm.res.ineq.test[-6], tolerance = 0.001)

all.equal(br.1htm.ref[-6], br.1htm.test[-6], tolerance = 0.001)
all.equal(br.1htm.ineq.ref[-6], br.1htm.ineq.test[-6], tolerance = 0.001)




