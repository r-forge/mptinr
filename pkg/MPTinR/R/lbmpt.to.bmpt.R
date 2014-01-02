### lbmpt.to.mpt ###

# .isNode
.isNode <- function(op)ifelse(is.na(op), "argument to function does not exist",grepl("[a-z]",op))

# .isLeaf
.isLeaf <- function(op){
  v <- ifelse(is.na(op), "argument to function does not exist",grepl("^[[:digit:]]+$",op))
  if(sum(!v)>0){
    out <- FALSE
  }else{
    out <- TRUE
  }
  if(is.list(op)){
    out <- FALSE
  }
  return(out)
}

# .parse.rec
.parse.rec <-  function(cfl){
  if(.isNode(cfl[1]) & .isLeaf(cfl[2]) & .isLeaf(cfl[3])){
    l <- list(list(c(cfl[1], cfl[2]),(c(paste("(1-",cfl[1],")", sep=""), cfl[3]))),cfl[4:length(cfl)])
    return(l)
  }
  if(.isNode(cfl[1]) & .isLeaf(cfl[2])){
    secondcall <- .parse.rec(cfl[3:length(cfl)])
    l <- list(list(c(cfl[1], cfl[2]), c(paste("(1-",cfl[1],")", sep=""), secondcall[1])),"None")
    return(l)
  }
  if(.isNode(cfl[1])){
    firstcall  <-  .parse.rec(cfl[2:length(cfl)])
    if(.isLeaf(firstcall[[2]])){
      l <- list(list(list(cfl[1], firstcall[[1]]), c(paste("(1-",cfl[1],")", sep=""), firstcall[[2]])),cfl[2:length(cfl)]) 
      return(l)
    } else{
      secondcall <- .parse.rec(firstcall[[2]])
      l <- list(list(list(cfl[1], firstcall[[1]]), c(paste("(1-",cfl[1],")", sep=""), secondcall[1])),secondcall[2])
      return(l)
    }
    }
  }




# .union.helper
.union.helper <- function(op1, op2){
  for(key in names(op2)){
    if(any(names(op1)== key)){
      op1[[key]] <- list(op1[[key]], op2[[key]])
  } else{
    op1[[key]] <- op2[[key]]
    }
}
  return(op1)
}




# .append.helper
.append.helper <- function(ap, dct){
  for(key in names(dct)){
    if(!is.list(dct[[key]])){
      dct[[key]] <- c(dct[[key]], ap)
    } else{
    for(lst in seq_along(dct[[key]])){
      
      dct[[key]][[lst]] <- c(dct[[key]][[lst]], ap)
    }
  }
  }
  return(dct)
}



# .rendEq.rec
.rendEq.rec <- function(parsed){
  left <- parsed[[1]]
  right <- parsed[[2]]
  lp <- list()
  rp <- list()
  if(.isLeaf(left[2])){
    lp[[left[2]]] <- left[[1]]
  } else{
    lp <- .append.helper(left[[1]], .rendEq.rec(left[[2]]))
  }
  if(.isLeaf(right[2])){
    rp[[right[2]]] <- right[[1]]
  } else{
    rp <- .append.helper(right[[1]], .rendEq.rec(right[[2]]))
  }

  return(.union.helper(lp,rp))
}
  


# .parse
.parse <- function(cfl) .parse.rec(cfl)[[1]]



# .renderEquation
.renderEquation <- function(parsed){
  trees <- .rendEq.rec(parsed)
  f.category <- list()
  for(key in names(trees)){
    if(is.list(trees[[key]])){
      category <- vector("list", length(trees[[key]]))
    } else{
    category <- vector("list", 1)
    }
    if(!is.list(trees[[key]])){
      trees[[key]] <- trees[[key]][length(trees[[key]]):1]
      text <- vector("character", length(trees[[key]]))
      if(length(trees[[key]])==1){
        text <- trees[[key]]
        category <- text
        f.category[key] <- category
      }else{
       for(y in seq_along(trees[[key]])[-length(trees[[key]])]){
       text[y] <- paste(trees[[key]][y],"*", sep="")
       text[length(trees[[key]])] <- trees[[key]][length(trees[[key]])]
     }
      category <-  paste(text, collapse="")
      f.category[key] <- category
    } }
    else{
      for(x in seq_along(trees[[key]])){
        if(length(trees[[key]][[x]])==1){
          text <- trees[[key]][[x]]
          category[[x]] <- text
          } else{
     trees[[key]][[x]] <- trees[[key]][[x]][length(trees[[key]][[x]]):1]
       text <- vector("character", length(trees[[key]][[x]]))
       for(i in seq_along(trees[[key]][[x]])[-length(trees[[key]][[x]])]){
       text[i] <- paste(trees[[key]][[x]][i],"*", sep="")
       text[length(trees[[key]][[x]])] <- trees[[key]][[x]][length(trees[[key]][[x]])]}
       category[[x]] <- paste(text, collapse="")
}}
    
    b.category <- vector("character", length(category))
 for(z in seq_along(category)[-length(category)]){
   b.category[z] <- paste(category[[z]], "+")
   b.category[length(category)] <- category[[length(category)]]
 }
    f.category[key] <- paste(b.category, collapse="")
    }
  }
  return(f.category)
}



# .make.tree
.make.tree <- function(tree){
  parsed <- .parse(tree)
  raw.model <- .renderEquation(parsed)
  tree <- vector("character", length(raw.model))
  i <- 1
  for(key in names(raw.model)){
    tree[i] <- paste(raw.model[[key]], "# category", key)
    i <- i+1
  }
  return(textConnection(tree))
}


# lbmpt.to.mpt
lbmpt.to.mpt <- function(model.list, outfile = NULL){
  if(length(model.list)== 1) .make.tree(model.list[[1]])
  else{
    vec <- c()
    y <- 1
    for(l in seq_along(model.list)){
      parsed <- .parse(model.list[[l]])
      raw.model <- .renderEquation(parsed)
      m.line <- vector("character", length(raw.model))
      i <- 1
      for(key in names(raw.model)){
        m.line[as.numeric(key)] <- paste(raw.model[[key]], "# category", key)
        i <- i+1
      }
      for(q in seq_along(m.line)){
        vec[y] <- m.line[q]
        y <- y+1
      }
      
      vec[y] <- ""
      y <- y+1
    }
    if (is.null(outfile)) return(writeLines(vec))
    else writeLines(vec, con = outfile)
  }
}

# test
# lbmpt.to.mpt(list(c("a", "1", "b", "2", "3"),c("x", "b", "1", "2", "c", "x", "1", "3", "e", "4", "3"),c("a", "b","1", "2", "c","3", "4") ))
# 
# lbmpt.to.mpt(list(c("a", "1", "b", "2", "3"),c("x", "b", "1", "2", "c", "x", "1", "3", "e", "4", "3"),c("a", "b","1", "2", "c","3", "4") ), outfile="test1.txt")

