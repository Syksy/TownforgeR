#' tf_plot_influence
#'
#' Plot buildings' influence map
#'
#' @param url TODO
#' @param building.type TODO
#' @param effect.type TODO
#' @param cut.out.flags TODO
#'
#' @return TODO
#'
#' @examples
#' c()
#'
#'
#' @export
#' @import Matrix
tf_plot_influence <- function(url, building.type, effect.type, cut.out.flags = TRUE) {
  
  stopifnot(all(building.type %in% colnames(infl.effects.ls[[1]])))
  
  stopifnot(effect.type %in% c("bonus", "need", "penalty"))
  
  infl.grid.ls <- tf_infl_grid(url = url, building.type = building.type, effect.type = effect.type)
  
  if ( ! is.null(infl.grid.ls$error)) {
    plot(0, 0, type = "n", main = infl.grid.ls$error, cex.main = 1)
    return(invisible())
  }
  
  unique.effect <- unique(infl.grid.ls$infl.grid@x)
  max.effect <- max(unique.effect)
  
  if (cut.out.flags) {
    cutouts.grid <- tf_flag_bounds(url, grid.dim = dim(infl.grid.ls$infl.grid), 
      coords.origin = infl.grid.ls$coords.origin)$bounds.grid #dim(infl.grid)) c(2500, 2500)
    infl.grid.ls$infl.grid[cutouts.grid == 1] <- 0L
    #infl.grid2 <- infl.grid2[nrow(infl.grid2):1, ]
    #infl.grid2 <- infl.grid2[, ncol(infl.grid2):1]
    #infl.grid2 <- Matrix::t(infl.grid2)
    # Dont need these manipulations; plot is oriented correctly
    
  }
  
  infl.grid.ls$infl.grid <- infl.grid.ls$infl.grid[Matrix::rowSums(infl.grid.ls$infl.grid) > 0L, Matrix::colSums(infl.grid.ls$infl.grid) > 0L]
  # Trim down to just where we have data to plot
  
  switch(effect.type,
    bonus = {
      infl.grid.ls$infl.grid <- infl.grid.ls$infl.grid * 5
      # to represent 5% boost
      
      Matrix::image(infl.grid.ls$infl.grid, useRaster = TRUE, 
        col.regions = hcl.colors(max.effect, palette = "Hawaii", rev = TRUE), 
        colorkey = list(tick.number = max.effect), cuts = max.effect - 1,
        main = paste0("Production boost for ", building.type, ", in percent") )
      # useRaster = TRUE is faster
      # ALSO: useRaster flips the chart vertically.
      
    },
    need = {
      Matrix::image(infl.grid.ls$infl.grid, useRaster = TRUE,
        main = paste0("Area where ", building.type, " is able to be built") )
    },
    penalty = {
      infl.grid.ls$infl.grid <- infl.grid.ls$infl.grid * (-5)
      # to represent 5% penalty
      
      Matrix::image(infl.grid.ls$infl.grid, useRaster = TRUE, 
        col.regions = hcl.colors(max.effect, palette = "ag_GrnYl"), 
        colorkey = list(tick.number = max.effect), cuts = max.effect - 1,
        main = paste0("Production penalty for ", building.type, ", in percent") )
    }
    # TODO: military's complex effects not calculated yet. Maybe best to combined penalty and boost calculations
    # https://townforge.net/manual/
    # "BxPy: gets a 5% bonus from every building of that type up to x of them, except if there are y or more, in which case it's a 5% penalty per such building"
    # AND how to deal with this logic?
    # "For a building to be deemed to be under the influence of a given building type, at least 50% of the building's surface area needs to be within the influence range of any building of the given type.
    # "Similarly, to get double bonus, at least 150% of the tiles should be within the influence or any building of the given type (any tile within the influence of N buildings will count N times), 250% for triple bonus, etc. "
    
    # TODO: does not check yet if the buildings are close enough in economic power to benefit
    
    
  )
  
}




#' tf_flag_bounds
#'
#' Get boundaries of flags
#'
#' @param url TODO
#' @param grid.dim TODO
#' @param coords.origin TODO
#' @param coords.offset TODO
#'
#' @return TODO
#'
#' @examples
#' c()
#'
#'
#' @export
#' @import Matrix
tf_flag_bounds <- function(url, grid.dim = NULL, coords.origin = NULL, coords.offset = 1000) {
  
  coords.offset <- as.integer(coords.offset)
  
  flags.ret <- TownforgeR::tf_rpc_curl(method = "cc_get_flags", url = url)$result$flags
  max.flag.id <- flags.ret[[length(flags.ret)]]$id
  
  role.names.tmp <- colnames(infl.effects.ls[[1]])
  role.names <- 0:(length(role.names.tmp) - 1)
  names(role.names) <- role.names.tmp
  
  coords.mat <- matrix(NA_real_, nrow = max.flag.id, ncol = 4, 
    dimnames = list(NULL, c("x0", "x1", "y0", "y1")) )
  owner <- vector(mode = "numeric", length = max.flag.id)
  influence <- vector(mode = "numeric", length = max.flag.id)
  role <- vector(mode = "numeric", length = max.flag.id)
  
  for (i in 1:max.flag.id) {
    if (i == 21 & packageVersion("TownforgeR") == "0.0.14") { next }
    # far away flag in testnet
    ret <- TownforgeR::tf_rpc_curl(method = "cc_get_flag", params = list(id = i), url = url)
    if (any(names(ret) == "error")) { next }
    coords.mat[i, "x0"] <- ret$result$x0
    coords.mat[i, "x1"] <- ret$result$x1
    coords.mat[i, "y0"] <- ret$result$y0
    coords.mat[i, "y1"] <- ret$result$y1
    owner[i] <- ret$result$owner
    influence[i] <- ret$result$influence
    role[i] <- ret$result$role
  }
  
  # x.min.map <- min(coords.mat[, "x0"], na.rm = TRUE)
  # y.min.map <- min(coords.mat[, "y0"], na.rm = TRUE)
  
  if (is.null(coords.origin)) {
    coords.origin <- c(x = min(coords.mat[, "x0"], na.rm = TRUE), y = min(coords.mat[, "y0"], na.rm = TRUE) )
  }
  
  coords.mat[, "x0"] <- coords.mat[, "x0"] - coords.origin["x"] + coords.offset
  coords.mat[, "x1"] <- coords.mat[, "x1"] - coords.origin["x"] + coords.offset
  coords.mat[, "y0"] <- coords.mat[, "y0"] - coords.origin["y"] + coords.offset
  coords.mat[, "y1"] <- coords.mat[, "y1"] - coords.origin["y"] + coords.offset
  
  coords.mat.complete <- complete.cases(coords.mat)
  
  owner <- owner[coords.mat.complete]
  influence <- influence[coords.mat.complete]
  role <- role[coords.mat.complete]
  coords.mat <- coords.mat[coords.mat.complete, ]
  
  mode(coords.mat) <- "integer"
  
  if (is.null(grid.dim)) {
    grid.dim <- c(max(coords.mat[, "y1"], na.rm = TRUE), max(coords.mat[, "x1"], na.rm = TRUE) )
    grid.dim <- grid.dim + coords.offset
  }
  
  bounds.grid <- Matrix::sparseMatrix(NULL, NULL, dims = grid.dim)
  
  for (i in seq_len(nrow(coords.mat))) {
    bounds.grid.tmp <- expand.grid(coords.mat[i, "y0"]:coords.mat[i, "y1"], coords.mat[i, "x0"]:coords.mat[i, "x1"])
    bounds.grid.tmp <- bounds.grid.tmp[bounds.grid.tmp[, 1] <= grid.dim[1] & bounds.grid.tmp[, 2] <= grid.dim[2] &
        bounds.grid.tmp[, 1] > 0L & bounds.grid.tmp[, 2] > 0L,  ]
    # Trim to the grid.dim
    if (nrow(bounds.grid.tmp) == 0) {next}
    bounds.grid <- bounds.grid + Matrix::sparseMatrix(bounds.grid.tmp[, 1], bounds.grid.tmp[, 2], x = 1L, 
      dims = grid.dim)
  }
  
  # list(characteristics = data.frame(
  #  owner, role, role.name = names(role.names)[match(role, role.names)],  stringsAsFactors = FALSE), 
  #  geo = bounds.grid)
  
  list(bounds.grid = bounds.grid, coords.origin = coords.origin)
  
}

#' tf_infl_grid
#'
#' Get boundaries of building influence
#'
#' @param url TODO
#' @param building.type TODO
#' @param effect.type TODO
#' @param disaggregated return object disaggregates matrix by building type
#'
#' @return TODO
#'
#' @examples
#' c()
#'
#'
#' @export
#' @import Matrix
tf_infl_grid <- function(url, building.type, effect.type, grid.dim, coords.origin, disaggregated = FALSE) {
  # effect.type is "bonus" "need" or "penalty"
  infl.effects.mat <- infl.effects.ls[[effect.type]]
  # infl.effects.ls comes from package's data
  #ROW is affected by COLUMN
  
  infl.effects.v <- infl.effects.mat[building.type, ]
  
  if ( all(is.na(infl.effects.v)) ) {
    return(list(error = "ERROR: No influence effects for this building type"))
  }
  
  infl.grid.ls <- tf_infl_location(url = url, building.type = names(na.omit(infl.effects.v)), 
    coords.origin = coords.origin, grid.dim = grid.dim)
  # TODO: more efficient for tf_infl_location to create a ngCMatrix or have dgCMatrix with 1's? how does it affect Reduce("+",) ?
  if ( length(infl.grid.ls$geo) == 0) {
    return(list(error = "ERROR: Building types that influence this building have not yet been built"))
  }
  
  if (disaggregated) {
    infl.grid.ret <- vector("list", length(unique(infl.grid.ls$characteristics$role.name)))
    names(infl.grid.ret) <- unique(infl.grid.ls$characteristics$role.name)
  } else {
    infl.grid.ret <- Matrix::sparseMatrix(NULL, NULL, dims = dim(infl.grid.ls$geo[[1]]))
  }

  for (i in unique(infl.grid.ls$characteristics$role.name)  ) {
    
    infl.grid.tmp <- infl.grid.ls$geo[infl.grid.ls$characteristics$role.name == i]
    
    infl.grid.tmp <- Reduce("+", infl.grid.tmp)
    # https://stackoverflow.com/questions/11641701/sum-a-list-of-matrices
    # TODO: be more efficient. Maybe with Rcpp and sparse arrays.What about the  SparseM package?
    if (effect.type != "need") {
      infl.grid.tmp@x[infl.grid.tmp@x > infl.effects.v[i] ] <- infl.effects.v[i]
      # if higher than the maximum bonus (or penalty) , then limit it
    }
    if (disaggregated) {
      infl.grid.ret[[i]] <- infl.grid.tmp
    } else {
      infl.grid.ret <- infl.grid.ret + infl.grid.tmp
    }
    
  }
  
  if (effect.type == "need") {
    infl.grid.ret@x[infl.grid.ret@x > 1L ] <- 1L
  }
  
  list(infl.grid = infl.grid.ret, coords.origin = infl.grid.ls$coords.origin)
  
}


#' tf_infl_location
#'
#' Get boundaries of building influence
#'
#' @param url TODO
#' @param building.type TODO
#' @param coords.origin TODO
#' @param grid.dim TODO
#' @param coords.offset TODO
#'
#' @return TODO
#'
#' @examples
#' c()
#'
#'
#' @export
#' @import Matrix
tf_infl_location <- function(url, building.type = "all", coords.origin = NULL, grid.dim = NULL, coords.offset = 1000) {
  # Remember coords.offset is 1000! Provides buffer so influence range doesn't go below zero
  # prevents problems with integers being too large. Avoids:
  # "NAs introduced by coercion to integer range"
  # building types:
  # 1 = ag
  # 2 = craft
  # 5 = basic residential
  # 10 = stonecutter
  # 11 = sawmill
  # 14 = workforce
  # 15 = road
  # ROLE_EMPTY = 0
  # ROLE_AGRICULTURAL = 1
  # ROLE_CRAFT = 2
  # ROLE_RESIDENTIAL1 = 5
  # ROLE_SAWMILL = 11
  # ROLE_STONECUTTER = 10
  # ROLE_WORKFORCE = 14
  
  coords.offset <- as.integer(coords.offset)
  
  role.names.tmp <- colnames(infl.effects.ls[[1]])
  role.names <- 0:(length(role.names.tmp) - 1)
  names(role.names) <- role.names.tmp
  
  stopifnot(length(building.type) > 0L && all(building.type %in% c("all", names(role.names)) ))
  
  if (length(building.type) == 1 && building.type == "all") {
    building.type <- role.names
  } else {
    building.type <- role.names[building.type]
  }
  
  # url <- "http://127.0.0.1:28881/json_rpc"
  
  flags.ret <- TownforgeR::tf_rpc_curl(method = "cc_get_flags", url = url)$result$flags
  max.flag.id <- flags.ret[[length(flags.ret)]]$id
  
  coords.mat <- matrix(NA_real_, nrow = max.flag.id, ncol = 4, dimnames = list(NULL, c("x0", "x1", "y0", "y1")) )
  # NA_real_ instead of integer since CURL us going to read the data as numeric, anyway
  owner <- vector(mode = "numeric", length = max.flag.id)
  influence <- vector(mode = "numeric", length = max.flag.id)
  role <- vector(mode = "numeric", length = max.flag.id)
  
  for (i in 1:max.flag.id) {
    if (i == 21 & packageVersion("TownforgeR") == "0.0.14") { next }
    ret <- TownforgeR::tf_rpc_curl(method = "cc_get_flag", params = list(id = i), url = url)
    if (any(names(ret) == "error")) { next }
    if ( ! ret$result$role %in% building.type) { next }
    # TODO: want to check if ret$result$active == TRUE
    coords.mat[i, "x0"] <- ret$result$x0
    coords.mat[i, "x1"] <- ret$result$x1
    coords.mat[i, "y0"] <- ret$result$y0
    coords.mat[i, "y1"] <- ret$result$y1
    # TODO: Optimize this
    owner[i] <- ret$result$owner
    influence[i] <- ret$result$influence
    role[i] <- ret$result$role
  }
  
  x.min.map <- min(coords.mat[, "x0"], na.rm = TRUE)
  y.min.map <- min(coords.mat[, "y0"], na.rm = TRUE)
  
  if ( ! is.null(coords.origin)) {
    x.min.map <- coords.origin["x"]
    y.min.map <- coords.origin["y"]
  }
  
  coords.mat[, "x0"] <- coords.mat[, "x0"] - x.min.map + coords.offset
  coords.mat[, "x1"] <- coords.mat[, "x1"] - x.min.map + coords.offset
  coords.mat[, "y0"] <- coords.mat[, "y0"] - y.min.map + coords.offset
  coords.mat[, "y1"] <- coords.mat[, "y1"] - y.min.map + coords.offset
  coords.origin <- c(x = x.min.map, y = y.min.map)
  
  infl.mat <- matrix(c(-1, 1), byrow = TRUE, nrow = nrow(coords.mat), ncol = 4 )
  
  infl.mat <- coords.mat + infl.mat * 
    matrix(rep(influence, times = 4), nrow = nrow(infl.mat), ncol = 4) 
  # element by element multiplication
  
  coords.mat.complete <- complete.cases(coords.mat)
  
  owner <- owner[coords.mat.complete]
  influence <- influence[coords.mat.complete]
  role <- role[coords.mat.complete]
  infl.mat <- infl.mat[coords.mat.complete, ]
  coords.mat <- coords.mat[coords.mat.complete, ]
  
  mode(infl.mat) <- "integer"
  
  if ( is.null(grid.dim)) {
    grid.dim <- c(max(infl.mat[, "y1"], na.rm = TRUE), max(infl.mat[, "x1"], na.rm = TRUE) )
    grid.dim <- grid.dim + coords.offset
  }
  
  infl.grid.ls <- vector(mode = "list", length = nrow(infl.mat))
  
  for (i in seq_len(nrow(infl.mat))) {
    infl.grid.tmp <- expand.grid(infl.mat[i, "y0"]:infl.mat[i, "y1"], infl.mat[i, "x0"]:infl.mat[i, "x1"])
    infl.grid.ls[[i]] <- Matrix::sparseMatrix(infl.grid.tmp[, 1], infl.grid.tmp[, 2], # x = 1L, 
      dims = grid.dim )
  }
  # dense array would be 5GB of data -- no-go
  
  list(characteristics = data.frame(
    owner, influence, role, role.name = names(role.names)[match(role, role.names)],  stringsAsFactors = FALSE), 
    geo = infl.grid.ls,
    coords.origin = coords.origin)
}



#' tf_min_flag_size
#'
#' Get boundaries of building influence
#'
#' @param building.role TODO
#' @param economic.power TODO
#' @param min.size.scale.df TODO
#'
#' @return TODO
#'
#' @examples
#' c()
#'
#'
#' @export
#' @import Matrix
tf_min_flag_size <- function(building.role, economic.power) {
  stopifnot(economic.power >= 100 & economic.power <= 300)
  if ( ! any(min.size.scale.df$building.role == building.role)) {
    ret <- 8
  } else {
  ret <- with(min.size.scale.df[min.size.scale.df$building.role == building.role, , drop = FALSE], 
    floor(lowest + (highest - lowest) * (economic.power - 100) / 200 ))
  # From line 409 of cc.cpp
  }
  min(c(ret, 256))
}


