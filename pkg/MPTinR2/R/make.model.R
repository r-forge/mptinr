
setGeneric("make.mpt", function(model, restrictions = NULL, ...) standardGeneric("make.mpt"))

setMethod("make.mpt", "character", function(model, restrictions = NULL, model.type = c("easy", "eqn", "eqn2"), ...) {
	
	model.filename <- model
	restrictions.filename <- restrictions
	
	if (grepl("\\.eqn$", model.filename) || grepl("\\.EQN$", model.filename)) model.type <- "eqn"
	if (model.type[1] == "eqn") {
		raw.model <- .read.EQN.model(model.filename)
	} else raw.model <- .read.MPT.model(model.filename)
	
	model <- new("mpt.model", initial.model = raw.model, check = list(), restrictions = NULL)
	
	if(!is.null(restrictions.filename)) restrictions(model) <- .read.MPT.restrictions(restrictions.filename)
	
	initial.model.data.frame(model) <- .make.model.df(initial.model(model))
	
	if (is.null(restrictions(model))) model.data.frame(model) <- initial.model.data.frame(model)
	else model.data.frame(model) <- .apply.restrictions(model)
	
	model.list(model) <- .make.model.list(model.data.frame(model))
	
	first.checks <- .check.model(model)
	check(model) <- first.checks[-length(first.checks)]
	check(model)[["df"]] <- c(available = check(model)[["n.categories"]] - check(model)[["n.trees"]], used = check(model)[["n.free.parameters"]], model = (check(model)[["n.categories"]] - check(model)[["n.trees"]]) - check(model)[["n.free.parameters"]])
	
	if (!is.null(first.checks[["lbmpt"]])) bmpt <- is.bmpt(first.checks[["lbmpt"]])
	else bmpt <- FALSE
	if (bmpt) {
		#browser()
		model <- as(model, "bmpt.model")
		matrices <- .make.matrices(model)
		A(model) <- matrices[["A"]]
		storage.mode(A(model)) <- "integer"
		B(model) <- matrices[["B"]]
		storage.mode(B(model)) <- "integer"
		lbmpt(model) <- first.checks[["lbmpt"]]
		check(model)[["is.bmpt"]] <- TRUE
	}
	else check(model)[["is.bmpt"]] <- FALSE
	
	
	model
})



.check.model <- function(model) {

	prob.tree.check <- .check.MPT.probabilities(model.list(model))
	if(all(prob.tree.check==1)) {
		prob.corr <- TRUE
	} else {
		prob.corr <- paste("Model not constructed well: Branch probabilities of tree(s) ", paste(which(prob.tree.check!=1), collapse= ", "), " do not sum to 1!", sep = "")
	}
	orig.params <- .find.MPT.params(model.list(model))	
	if (!is.null(restrictions(model))) {
		for (restriction in fixed.restrictions(model)) orig.params <- orig.params[-which(parameter(restriction) == orig.params)]
		fixed.parameters <- vapply(fixed.restrictions(model), parameter, "")
	} else {
		fixed.parameters <- NULL
	}
	l.orig.params <- length(orig.params)
	n.trees.orig <- length(model.list(model))
	n.categories <- length(unlist(model.list(model)))
	
	max.branches.per.category <- max(table(model.data.frame(model)[,2]))
	branches.per.category <- table(model.data.frame(model)[,2])
	
	original.parameters <- .find.MPT.params(initial.model(model))
	
	suppressWarnings(lbmpt <- tryCatch(.make.mpt.cf(model), error = function(e) NULL))
	
	list(probabilities.eq.1 = prob.corr, n.trees = n.trees.orig, n.categories = n.categories, n.free.parameters = l.orig.params, free.parameters = orig.params, n.fixed.parameters = length(fixed.parameters), fixed.parameters = fixed.parameters, original.parameters = original.parameters, max.branches.per.category = max.branches.per.category, branches.per.category = branches.per.category, lbmpt = lbmpt)
}

is.bmpt <- function(lbmpt) {
	
	is.category <- grepl("^[[:digit:]]+$", lbmpt)	
	type <- ifelse(is.category == 0, 1, 0)
	
	############################################################################################################
	## This code is adapted from Wu, Myung & Batchelder (2010a, 2010b) and based on Purdy & Batchelder (2009) ##
	############################################################################################################
	
	L <- length(lbmpt)
	code <- matrix(0, L, L)
	if (type[1] == 0 & L == 1) return(FALSE)	#This should return TRUE, but a model that small is uninteresting here (Henrik Singmann, 29-7-2011)
	if (type[1] == 0 & L != 1) return(FALSE)
	p <- 1
	u <- 1
	for (i in 2:L) {
		code[i,] <- code[p,]
		code[i,p] <- u
		if (type[i] == 1) {
			u <- 1
			p <- i
		} else {
			u <- -1
			ind <- i-1
			while (ind > 0) {
				if (ind <= 0 & i < L) return(FALSE)
				if (type[ind] == 1) {
					p <- ind
					break
				} else {
					if (type[ind] == 0) {
						if (type[ind-1] !=1) return(FALSE)
						type[c(ind-1,ind, ind+1)] <- -1
						ind <- ind-2
					} else {
						if (type[ind] == -1) {
							type[ind+1] <- -1
							while (type[ind] == -1) ind <- ind-1
							if (type[ind] != 1) return(FALSE)
							type[ind] <- -1
							ind <- ind-1
						}
					}
				}
			}
		}
	}
	if (ind > 0) return(FALSE)
	else return (TRUE)
}

.make.matrices <- function(model) {
	#browser()
	
	n.parameters <- check(model)[["n.free.parameters"]] + check(model)[["n.fixed.parameters"]]
	parameters <- c(check(model)[["free.parameters"]], check(model)[["fixed.parameters"]])
	
	A <- array(0, dim = c(check(model)[["n.categories"]], check(model)[["max.branches.per.category"]], n.parameters))
	dimnames(A)[[3]] <- parameters
	B <- A
	
	for (parameter in parameters) {
		for (branch in 1:dim(model.data.frame(model))[1]) {
			tmp.branch <- strsplit(model.data.frame(model)[branch,"branches"], split="\\*")[[1]]
			A[model.data.frame(model)[branch,"category"], model.data.frame(model)[branch,"n.branch"], parameter] <- sum(grepl(paste("^", parameter, "$", sep = ""), tmp.branch))
			B[model.data.frame(model)[branch,"category"], model.data.frame(model)[branch,"n.branch"], parameter] <- sum(grepl(paste("^\\(1-", parameter, "\\)$", sep = ""), tmp.branch))
		}
	}
	list(A = A, B = B)
}


