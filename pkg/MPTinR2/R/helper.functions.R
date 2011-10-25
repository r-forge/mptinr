

#############
## General ##
#############

.find.MPT.params <- function(model) {
	inobjects <- function(ex) {
		split.exp1 <- strsplit(as.character(ex),"[[:space:]]")
		split.exp2 <- sapply(split.exp1, strsplit, split = "[()+*-]")
		return(sort(unique(grep("[[:alpha:]]",unlist(split.exp2), value = TRUE))))
		}
	tmp <- sapply(model,inobjects)
	return(unique(sort(unique(unlist(tmp)))))
}


.count.branches <- function(model.df) {
	counters <- rep(1, max(model.df[,"category"]))
	for (branch in 1:dim(model.df)[1]) {
		tmp.cat <- model.df[branch,"category"]
		model.df[branch,"n.branch"] <- counters[tmp.cat]
		counters[tmp.cat] <- counters[tmp.cat] + 1
	}
	model.df
}



##############
# make.model #
##############

.check.MPT.probabilities <- function(tree){
	tmp.env <- new.env()
	temp.param.names <- .find.MPT.params(tree)
	temp.param.val <- runif(length(temp.param.names))
	temp.branch <- sapply(tree,length)
	prob <- rep(NA,length(temp.branch))
   
	for (i in 1:length(temp.param.val)) {
		assign(temp.param.names[i],temp.param.val[i], envir = tmp.env)
	}
	temp.check <- sapply(unlist(tree),eval, envir = tmp.env)
	for (i in 1:length(temp.branch)){
		if (i==1) prob[1] <- sum(temp.check[1:temp.branch[1]])
		else prob[i] <- sum(temp.check[(sum(temp.branch[1:(i-1)])+1):sum(temp.branch[1:i])])
	}
	prob <- round(prob,digits=6)
	return(prob)
}


.make.model.df <- function(model) {
	#require(stringr)
	oneLineDeparse <- function(expr){
			paste(deparse(expr), collapse="")
		}
	
	n.trees <- length(model)
	l.trees <- sapply(model, length)
	l.trees <- c(0, l.trees)
	
	fin.model <- vector("list", n.trees)
	
	for (tree in 1:n.trees) {
		utree <- unlist(model[[tree]])
		tree.df.unordered <- do.call("rbind",lapply(1:length(utree), function(t) data.frame(category = t, branches = oneLineDeparse(utree[[t]]), stringsAsFactors = FALSE)))
		
		tree.list <- vector("list", dim(tree.df.unordered)[1])
		for (c1 in 1:length(tree.list)) {
			category <- tree.df.unordered[c1,"category"]
			branch <- strsplit(tree.df.unordered[c1,"branches"], "\\+")
			branch <- gsub(" ", "", branch[[1]])
			tree.list[[c1]] <- data.frame(tree = tree, category = category, branches = branch, stringsAsFactors = FALSE)
		}
		tree.df <- do.call("rbind", tree.list)
		fin.model[[tree]] <- tree.df[rev(order(tree.df[["branches"]])),]
	}
	n.categories <- c(0,sapply(fin.model, function(x) max(x[["category"]])))
	n.cat.cumsum <- cumsum(n.categories)
	
	model.df <- do.call("rbind", fin.model)
	
	model.df[["category"]] <- model.df[,"category"] + n.cat.cumsum[model.df[,"tree"]]
	
	rownames(model.df) <- NULL
	
	.count.branches(model.df)
	
}

.make.model.list <- function(model.df) {
	parse.eqn <- function(x){
		branches <- unique(x[,2])
		l.tree <- length(branches)
		tree <- vector('expression', l.tree)
		for (branch in 1:l.tree) {
			tree[branch] <- parse(text = paste(x[x[,2] == branches[branch],"branches"], collapse = " + "))
		}
		tree
	}
	tmp.ordered <- model.df[order(model.df[,1]),]
	tmp.spl <- split(tmp.ordered, factor(tmp.ordered[,1]))
	tmp.spl <- lapply(tmp.spl, function(d.f) d.f[order(d.f[,2]),])
	model <- lapply(tmp.spl, parse.eqn)
	names(model) <- NULL
	model
}




.apply.restrictions <- function(model) {
	
	tmp.model <- initial.model.data.frame(model)
	
	if (length(inequality.restrictions(model)) != 0) {
		for (inequality in inequality.restrictions(model)) {
			inverse <- paste("\\(1-", parameter(inequality), "\\)", sep = "")
			while (any(grepl(inverse, tmp.model[,"branches"]))) {
				if (length(exchange.inverse(inequality)) == 1) {
					tmp.model[,"branches"] <- gsub(inverse, exchange.inverse(inequality)[[1]], tmp.model[,"branches"])
				} else {
					for (branch in 1:dim(tmp.model)[1]) {
						if (grepl(inverse, tmp.model[branch,"branches"])) {
							for (exchange in exchange.inverse(inequality)) {
								tmp1 <- tmp.model[branch,]
								tmp1[,"branches"] <- sub(inverse, exchange, tmp1[,"branches"])
								if (!exists("new.model")) new.model <- tmp1
								else new.model <- rbind(new.model, tmp1)
							}
						} else {
							if (!exists("new.model")) new.model <- tmp.model[branch,]
							else new.model <- rbind(new.model, tmp.model[branch,])
						}
					}
					tmp.model <- new.model
					rm(new.model)
				}
			}
			parameter <- parameter(inequality)
			while (any(grepl(parameter, tmp.model[,"branches"]))) {
				if (length(exchange.parameter(inequality)) == 1) {
					tmp.model[,"branches"] <- gsub(parameter, exchange.parameter(inequality)[[1]], tmp.model[,"branches"])
				} else {
					for (branch in 1:dim(tmp.model)[1]) {
						if (grepl(parameter, tmp.model[branch,"branches"])) {
							for (exchange in exchange.parameter(inequality)) {
								tmp1 <- tmp.model[branch,]
								tmp1[,"branches"] <- sub(parameter, exchange, tmp1[,"branches"])
								if (!exists("new.model")) new.model <- tmp1
								else new.model <- rbind(new.model, tmp1)
							}
						} else {
							if (!exists("new.model")) new.model <- tmp.model[branch,]
							else new.model <- rbind(new.model, tmp.model[branch,])
						}
					}
					tmp.model <- new.model
					rm(new.model)
				}
			}
		}
		row.names(tmp.model) <- NULL
	}
	if (length(equality.restrictions) != 0) {
		for (equality in equality.restrictions(model)) {
			tmp.model[,"branches"] <- gsub(parameter(equality), value(equality), tmp.model[,"branches"])
		}
	}
	
	.count.branches(tmp.model)
}

.make.mpt.cf <- function(model){
	
	bin.objects <- function(branch) {
		objects <- strsplit(branch, "\\*")[[1]]
		!(grepl("[()]", objects))
	}
	
	model.df.tmp <- model.data.frame(model)
	c.join <- 1
	
	while (length(unique(model.df.tmp[,"tree"])) > 1) {
		model.df.tmp[model.df.tmp$tree == 1, "branches"] <- paste("hank.join.", c.join, "*", model.df.tmp[model.df.tmp$tree == 1, "branches"], sep = "")
		model.df.tmp[model.df.tmp$tree == 2, "branches"] <- paste("(1-hank.join.", c.join, ")*", model.df.tmp[model.df.tmp$tree == 2, "branches"], sep = "")
		model.df.tmp[model.df.tmp$tree == 2, "tree"] <- rep(1, length(model.df.tmp[model.df.tmp$tree == 2, "tree"]))
		model.df.tmp[model.df.tmp$tree > 2, "tree"] <- model.df.tmp[model.df.tmp$tree > 2, "tree"] -1
		c.join <- c.join + 1
	}
	tree.ordered <- model.df.tmp
	tree.list <- lapply(1:dim(tree.ordered)[1], function(x) list(category = tree.ordered[x,"category"], branch = tree.ordered[x,"branches"], objects = strsplit(tree.ordered[x,"branches"], "\\*")[[1]], params = .find.MPT.params(tree.ordered[x,"branches"]), binary = bin.objects(tree.ordered[x,"branches"])))
	tmp.tree <- tree.list
	mpt.string <- c(tmp.tree[[1]][["objects"]], tmp.tree[[1]][["category"]])
	for (counter1 in 2:length(tmp.tree)) {
		if (length(tmp.tree[[counter1]][["binary"]]) == length(tmp.tree[[counter1-1]][["binary"]]) & tmp.tree[[counter1-1]][["binary"]][length(tmp.tree[[counter1-1]][["binary"]])] == TRUE & tmp.tree[[counter1]][["binary"]][length(tmp.tree[[counter1]][["binary"]])] == FALSE) {
			mpt.string <- c(mpt.string, tmp.tree[[counter1]][["category"]])
		} else {
		if (length(tmp.tree[[counter1]][["binary"]]) == length(tmp.tree[[counter1-1]][["binary"]]) & tmp.tree[[counter1-1]][["binary"]][length(tmp.tree[[counter1-1]][["binary"]])] == FALSE & tmp.tree[[counter1]][["binary"]][length(tmp.tree[[counter1]][["binary"]])] == TRUE) {
			change <- min(which((tmp.tree[[counter1]][["binary"]] == tmp.tree[[counter1-1]][["binary"]]) == FALSE))+1
			tmp.objects <- tmp.tree[[counter1]][["objects"]][change:(length(tmp.tree[[counter1]][["binary"]]))]
			mpt.string <- c(mpt.string, tmp.objects[tmp.tree[[counter1]][["binary"]][change:length(tmp.tree[[counter1]][["binary"]])]], tmp.tree[[counter1]][["category"]])
		} else {
		if (length(tmp.tree[[counter1]][["binary"]]) > length(tmp.tree[[counter1-1]][["binary"]])) {
			change <- min(which((tmp.tree[[counter1]][["binary"]] == tmp.tree[[counter1-1]][["binary"]][1:length(tmp.tree[[counter1]][["binary"]])]) == FALSE))+1
			if (change < (length(tmp.tree[[counter1-1]][["binary"]]))) {
				tmp.param <- tmp.tree[[counter1]][["objects"]][change:(length(tmp.tree[[counter1]][["binary"]]))]
			} else {
				tmp.new <- tmp.tree[[counter1]][["objects"]][(length(tmp.tree[[counter1-1]][["binary"]])):length(tmp.tree[[counter1]][["binary"]])]
				tmp.param <- tmp.new[tmp.tree[[counter1]][["binary"]][(length(tmp.tree[[counter1-1]][["binary"]])):length(tmp.tree[[counter1]][["binary"]])]]
			}
			mpt.string <- c(mpt.string, tmp.param, tmp.tree[[counter1]][["category"]])
		} else {
		if (length(tmp.tree[[counter1]][["binary"]]) < length(tmp.tree[[counter1-1]][["binary"]])) {
			change <- min(which((tmp.tree[[counter1]][["binary"]] == tmp.tree[[counter1-1]][["binary"]][1:length(tmp.tree[[counter1]][["binary"]])]) == FALSE))+1
			if (change <= length(tmp.tree[[counter1]][["binary"]])) {
			tmp.objects <- tmp.tree[[counter1]][["objects"]][change:(length(tmp.tree[[counter1]][["binary"]]))]
			} else tmp.objects <- NULL
			mpt.string <- c(mpt.string, tmp.objects[tmp.tree[[counter1]][["binary"]][change:length(tmp.tree[[counter1]][["binary"]])]], tmp.tree[[counter1]][["category"]])
		}
		}
		}
		}
	
	}
	mpt.string
}
