#' tf_search_best_flags
#'
#' Brute force search for flags with best production and return on investment
#'
#' @param url TODO
#' @param building.type TODO
#' @param economic.power TODO
#' @param get.flag.cost TODO
#' @param city TODO
#' @param grid.density.params TODO
#'
#' @return TODO
#'
#' @examples
#' c()
#'
#'
#' @export
#' @import Matrix
tf_search_best_flags <- function(url, building.type, economic.power, get.flag.cost = TRUE, 
  city = 0, grid.density.params = c(10, 10)) {
  # density.params: first element is y (north-south) and second is x (east-west)
  # TODO: make the flag cutouts include inactive flags
  
  #library(Matrix)
  
  #uint32_t city;
  #uint32_t x0;
  #uint32_t y0;
  #uint32_t x1;
  #uint32_t y1;
  #uint8_t role;
  #uint32_t economic_power;
  
  role.id <- building.names.df$id[building.names.df$abbrev == building.type]
  role.name <- building.names.df$role.name[building.names.df$abbrev == building.type]
  
  infl.grid.disagg  <- TownforgeR::tf_infl_grid(url, building.type, "bonus", disaggregated = TRUE)
  
  cutouts.grid <- TownforgeR::tf_flag_bounds(url, grid.dim = dim(infl.grid.disagg$infl.grid[[1]]), 
    coords.origin = infl.grid.disagg$coords.origin) 
  
  grid.dim <- dim(infl.grid.disagg$infl.grid[[1]])
  
  
  candidates.mat <- as.matrix(expand.grid(
    y = floor(seq(1, grid.dim[1], length.out = grid.density.params[1])), 
    x = floor(seq(1, grid.dim[2], length.out = grid.density.params[2]))))
  
  infl.thresholds <- c(0, 1, seq(1.5, 101, 1)) 
  
  candidates.ls <- vector("list", nrow(candidates.mat))
  names(candidates.ls) <- paste0("c(", candidates.mat[, "y"], ", ", candidates.mat[, "x"], ")")
  
  for (i.candidate in seq_along(candidates.ls)) {
    
    cat(i.candidate, " of ", length(candidates.ls), base::date(), "\n")
    
    if (cutouts.grid[as.matrix(candidates.mat[i.candidate, , drop = FALSE]) ] != 0 ) {
      next
    }
    # If the initial point is within another flag, then go to the next initial starting point candidate
    
    init.south.coord <- south.coord <- unname(candidates.mat[i.candidate, 1])
    init.east.coord  <- east.coord  <- unname(candidates.mat[i.candidate, 2])
    
    min.flag.size <- TownforgeR::tf_min_flag_size(role.name, economic.power)
    south.coord <- south.coord + min.flag.size - 1
    east.coord <- east.coord + min.flag.size - 1
    
    expand.south <- TRUE
    expand.east <- TRUE
    
    candidates.ls[[i.candidate]] <- vector("list", 256 * 256)
    
    while(expand.south || expand.east) {
      
      if (expand.south) {
        if ( (south.coord - init.south.coord + 1) > 256 ||
            any(dim(cutouts.grid) < c(south.coord + 1, east.coord)) ||
            sum(cutouts.grid[init.south.coord:(south.coord + 1), init.east.coord:east.coord]) != 0 ) {
          # if the search runs grows bigger than maximum allowed size, goes into an existing flag or  
          # "goes off the map", then stop expanding in this direction
          expand.south <- FALSE
        } else {
          
          south.coord <- south.coord + 1
          
          base.production.ls <- TownforgeR::tf_rpc_curl(url,
            method ="cc_get_production",
            params = list(
              city = city, 
              x0 = infl.grid.disagg$coords.origin[["x"]] + init.east.coord,
              y0 = infl.grid.disagg$coords.origin[["y"]] + init.south.coord,
              x1 = infl.grid.disagg$coords.origin[["x"]] + east.coord,
              y1 = infl.grid.disagg$coords.origin[["y"]] + south.coord,
              role = role.id,
              economic_power = economic.power
            ), keep.trying.rpc = TRUE )$result$item_production
          
          base.production.ls <- sapply(base.production.ls, FUN = function(x) {
            ret <- list(x[["amount"]])
            names(ret) <- paste0("base.production.item_", x[["type"]])
            ret
          })
          
          infl.boost <- sapply(infl.grid.disagg$infl.grid, FUN = function(x) {
            infl.coverage <- x[init.south.coord:south.coord, init.east.coord:east.coord]
            (-1) + as.numeric(cut(mean(infl.coverage), breaks = infl.thresholds, right = FALSE) )
            # NOTE: using as.numeric to coerce the factor
          })
          
          infl.boost <- sum(infl.boost)
          
          if (get.flag.cost) {
            flag.cost <- TownforgeR::tf_rpc_curl(url,
              method ="cc_get_new_flag_cost",
              params = list(
                city = city, 
                x0 = infl.grid.disagg$coords.origin[["x"]] + init.east.coord,
                y0 = infl.grid.disagg$coords.origin[["y"]] + init.south.coord,
                x1 = infl.grid.disagg$coords.origin[["x"]] + east.coord,
                y1 = infl.grid.disagg$coords.origin[["y"]] + south.coord
              ), keep.trying.rpc = TRUE )$result$cost
          } else {
            flag.cost <- NULL
          }
          
          cand.ret <- list(y0 = init.south.coord, x0 = init.east.coord, y1 = south.coord, x1 = east.coord, 
            infl.boost = unname(infl.boost), flag.cost = flag.cost)
          
          candidates.ls[[i.candidate]][[length(candidates.ls[[i.candidate]]) + 1]] <- 
            as.data.frame(c(cand.ret, base.production.ls), stringsAsFactors = FALSE)
        }
      }
      
      if (expand.east) {
        if (  (east.coord - init.east.coord + 1) > 256 ||
            any(dim(cutouts.grid) < c(south.coord, east.coord + 1)) ||
            sum(cutouts.grid[init.south.coord:south.coord, init.east.coord:(east.coord + 1)]) != 0 ) {
          expand.east <- FALSE
        } else {
          
          east.coord <- east.coord + 1
          
          base.production.ls <- TownforgeR::tf_rpc_curl(url,
            method ="cc_get_production",
            params = list(
              city = city, 
              x0 = infl.grid.disagg$coords.origin[["x"]] + init.east.coord,
              y0 = infl.grid.disagg$coords.origin[["y"]] + init.south.coord,
              x1 = infl.grid.disagg$coords.origin[["x"]] + east.coord,
              y1 = infl.grid.disagg$coords.origin[["y"]] + south.coord,
              role = role.id,
              economic_power = economic.power
            ), keep.trying.rpc = TRUE )$result$item_production
          
          base.production.ls <- sapply(base.production.ls, FUN = function(x) {
            ret <- list(x[["amount"]])
            names(ret) <- paste0("base.production.item_", x[["type"]])
            ret
          })
          
          infl.boost <- sapply(infl.grid.disagg$infl.grid, FUN = function(x) {
            infl.coverage <- x[init.south.coord:south.coord, init.east.coord:east.coord]
            (-1) + as.numeric(cut(mean(infl.coverage), breaks = infl.thresholds, right = FALSE) )
            # NOTE: using as.numeric to coerce the factor
          })
          
          infl.boost <- sum(infl.boost)
          
          if (get.flag.cost) {
            flag.cost <- TownforgeR::tf_rpc_curl(url,
              method ="cc_get_new_flag_cost",
              params = list(
                city = city, 
                x0 = infl.grid.disagg$coords.origin[["x"]] + init.east.coord,
                y0 = infl.grid.disagg$coords.origin[["y"]] + init.south.coord,
                x1 = infl.grid.disagg$coords.origin[["x"]] + east.coord,
                y1 = infl.grid.disagg$coords.origin[["y"]] + south.coord
              ), keep.trying.rpc = TRUE )$result$cost
          } else {
            flag.cost <- NULL
          }
          
          cand.ret <- list(y0 = init.south.coord, x0 = init.east.coord, y1 = south.coord, x1 = east.coord, 
            infl.boost = unname(infl.boost), flag.cost = flag.cost)
          
          candidates.ls[[i.candidate]][[length(candidates.ls[[i.candidate]]) + 1]] <- 
            as.data.frame(c(cand.ret, base.production.ls), stringsAsFactors = FALSE)
          
        }
      }
      
    }
    
    candidates.ls[[i.candidate]] <- candidates.ls[[i.candidate]][ lengths(candidates.ls[[i.candidate]]) > 0]
    
  }
  
  candidates.ls <- unlist(candidates.ls, recursive = FALSE, use.names = FALSE)
  
  candidates.df <- plyr::rbind.fill(candidates.ls)
  # NOTE: "base.production.type_" columns might not be in ascending order of item ID. 
  # That should not matter for later use
  
  candidates.df
}





