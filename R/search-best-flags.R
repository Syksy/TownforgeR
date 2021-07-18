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
#' @param in.shiny TODO
#' @param waitress TODO
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
  city = 0, grid.density.params = c(10, 10), in.shiny = FALSE, waitress = NULL) {
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
  
  
  flag.bounds.ls <- TownforgeR::tf_flag_bounds(url, grid.dim = NULL, 
    coords.origin = NULL) 
  
  cutouts.grid <- flag.bounds.ls$bounds.grid
  
  grid.dim <- dim(cutouts.grid)
  
  infl.grid.disagg  <- TownforgeR::tf_infl_grid(url, building.type, "bonus", 
    grid.dim = grid.dim, coords.origin = flag.bounds.ls$coords.origin, disaggregated = TRUE)
  
  subject.to.influence <- ( ! "error" %in% names(infl.grid.disagg) )
  
  grid.density.y <- floor(seq(1, grid.dim[1], length.out = grid.density.params[1] + 1))
  grid.density.x <- floor(seq(1, grid.dim[2], length.out = grid.density.params[2] + 1))
  
  candidates.mat <- as.matrix(expand.grid(
    y = grid.density.y[ (-1) * length(grid.density.y)], 
    x = grid.density.x[ (-1) * length(grid.density.x)] ) )
  
  infl.thresholds <- c(0, 1, seq(1.5, 101, 1)) 
  
  candidates.ls <- vector("list", nrow(candidates.mat))
  names(candidates.ls) <- paste0("c(", candidates.mat[, "y"], ", ", candidates.mat[, "x"], ")")
  
  for (i.candidate in seq_along(candidates.ls)) {
    
    cat(i.candidate, " of ", length(candidates.ls), "    ", base::date(), "\n", sep = "")
    
    if (in.shiny) {
      
      #waitress$update(
      #  html = paste0("Expanding flag from candidate grid point ", i.candidate, " of ", length(candidates.ls), ""))
      
      waitress$set(i.candidate / length(candidates.ls))
      
      shiny::setProgress(
        value = i.candidate / length(candidates.ls),
        message = "Searching for best flag placements...",
        detail = paste0("Expanding flag from candidate grid point ", i.candidate, " of ", length(candidates.ls), "")
      )
    }
    
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
              x0 = flag.bounds.ls$coords.origin[["x"]] + init.east.coord,
              y0 = flag.bounds.ls$coords.origin[["y"]] + init.south.coord,
              x1 = flag.bounds.ls$coords.origin[["x"]] + east.coord,
              y1 = flag.bounds.ls$coords.origin[["y"]] + south.coord,
              role = role.id,
              economic_power = economic.power
            ), keep.trying.rpc = TRUE )$result$item_production
          
          base.production.ls <- sapply(base.production.ls, FUN = function(x) {
            ret <- list(x[["amount"]])
            names(ret) <- paste0("base.production.item_", x[["type"]])
            ret
          })
          
          if (subject.to.influence) {
            infl.boost <- sapply(infl.grid.disagg$infl.grid, FUN = function(x) {
              infl.coverage <- x[init.south.coord:south.coord, init.east.coord:east.coord]
              (-1) + as.numeric(cut(mean(infl.coverage), breaks = infl.thresholds, right = FALSE) )
              # NOTE: using as.numeric to coerce the factor
            })
          } else {
            infl.boost <- 0
          }
          
          infl.boost <- sum(infl.boost)
          
          if (get.flag.cost) {
            flag.cost <- TownforgeR::tf_rpc_curl(url,
              method ="cc_get_new_flag_cost",
              params = list(
                city = city, 
                x0 = flag.bounds.ls$coords.origin[["x"]] + init.east.coord,
                y0 = flag.bounds.ls$coords.origin[["y"]] + init.south.coord,
                x1 = flag.bounds.ls$coords.origin[["x"]] + east.coord,
                y1 = flag.bounds.ls$coords.origin[["y"]] + south.coord
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
              x0 = flag.bounds.ls$coords.origin[["x"]] + init.east.coord,
              y0 = flag.bounds.ls$coords.origin[["y"]] + init.south.coord,
              x1 = flag.bounds.ls$coords.origin[["x"]] + east.coord,
              y1 = flag.bounds.ls$coords.origin[["y"]] + south.coord,
              role = role.id,
              economic_power = economic.power
            ), keep.trying.rpc = TRUE )$result$item_production
          
          base.production.ls <- sapply(base.production.ls, FUN = function(x) {
            ret <- list(x[["amount"]])
            names(ret) <- paste0("base.production.item_", x[["type"]])
            ret
          })
          
          if (subject.to.influence) {
            infl.boost <- sapply(infl.grid.disagg$infl.grid, FUN = function(x) {
              infl.coverage <- x[init.south.coord:south.coord, init.east.coord:east.coord]
              (-1) + as.numeric(cut(mean(infl.coverage), breaks = infl.thresholds, right = FALSE) )
              # NOTE: using as.numeric to coerce the factor
            })
          } else {
            infl.boost <- 0
          }
          
          infl.boost <- sum(infl.boost)
          
          if (get.flag.cost) {
            flag.cost <- TownforgeR::tf_rpc_curl(url,
              method ="cc_get_new_flag_cost",
              params = list(
                city = city, 
                x0 = flag.bounds.ls$coords.origin[["x"]] + init.east.coord,
                y0 = flag.bounds.ls$coords.origin[["y"]] + init.south.coord,
                x1 = flag.bounds.ls$coords.origin[["x"]] + east.coord,
                y1 = flag.bounds.ls$coords.origin[["y"]] + south.coord
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
  
  candidates.df$area <- with(candidates.df, (y1 - y0 + 1) * (x1 - x0 + 1))
  candidates.df$flag.cost <- candidates.df$flag.cost / gold.unit.divisor
  
  #browser()
  
  for (i in grep("base.production.item_", colnames(candidates.df))) {
    
    item.num <- stringr::str_extract(colnames(candidates.df)[i], "[0-9]+")
    
    candidates.df[, paste0("total.production.item_", item.num)] <- 
      candidates.df[, i] * (1 + 0.05 * candidates.df$infl.boost)
    
    candidates.df[, paste0("total.production.per.area.item_", item.num)] <- 
      candidates.df[, paste0("total.production.item_", item.num)] / candidates.df$area
    
    candidates.df[, paste0("ROI.item_", item.num)] <- 
      candidates.df[, paste0("total.production.item_", item.num)] / candidates.df$flag.cost
  }
  
  candidates.df
}





#' tf_get_best_flag_map
#'
#' Calculates matrix of best flag(s) map
#'
#' @param url TODO
#' @param candidates.df TODO
#' @param chosen.item.id TODO
#' @param number.of.top.candidates TODO
#' @param building.type TODO
#' @param display.perimeter TODO
#'
#' @return TODO
#'
#' @examples
#' c()
#'
#'
#' @export
#' @import Matrix
tf_get_best_flag_map <-  function(url, candidates.df, chosen.item.id, 
  number.of.top.candidates, building.type, display.perimeter = TRUE) {
  
   #<- 256
   #<- 3
   #<- "WOR"
  # "http://127.0.0.1:28881/json_rpc"
  stopifnot(number.of.top.candidates <= length(c(LETTERS, letters)))
  # can't have more than 2*26 = 52 top candidates since letters are used for labels
  
  candidates.df <- do.call(rbind, by(candidates.df, list(candidates.df$y0, candidates.df$x0), FUN = function(x) {
    x[which.max(x[, paste0("ROI.item_", chosen.item.id)]), , drop = FALSE]
  }) )
  
  candidates.df <- candidates.df[order(candidates.df[, paste0("ROI.item_", chosen.item.id)], decreasing = TRUE), ]
  candidates.df <- candidates.df[seq_len(min(c(number.of.top.candidates, nrow(candidates.df)))), ]
  
  flag.bounds.ls <- tf_flag_bounds(url, grid.dim = NULL, coords.origin = NULL)
  
  cutouts.grid <- flag.bounds.ls$bounds.grid
  
  for (i in seq_len(nrow(candidates.df))) {
    bounds.grid.tmp <- expand.grid(candidates.df[i, "y0"]:candidates.df[i, "y1"], candidates.df[i, "x0"]:candidates.df[i, "x1"])
    #bounds.grid.tmp <- bounds.grid.tmp[bounds.grid.tmp[, 1] <= grid.dim[1] & bounds.grid.tmp[, 2] <= grid.dim[2] &
    #    bounds.grid.tmp[, 1] > 0L & bounds.grid.tmp[, 2] > 0L,  ]
    # Trim to the grid.dim
    if (nrow(bounds.grid.tmp) == 0) {next}
    cutouts.grid <- cutouts.grid + 2 * Matrix::sparseMatrix(bounds.grid.tmp[, 1], bounds.grid.tmp[, 2], x = 1L, 
      dims = dim(cutouts.grid))
    
    if (display.perimeter) {
      perim.df <- rbind(
        data.frame(y = candidates.df[i, "y0"],                        x = candidates.df[i, "x0"]:candidates.df[i, "x1"]),
        data.frame(y = candidates.df[i, "y0"]:candidates.df[i, "y1"], x = candidates.df[i, "x0"]),
        data.frame(y = candidates.df[i, "y1"],                        x = candidates.df[i, "x0"]:candidates.df[i, "x1"]),
        data.frame(y = candidates.df[i, "y0"]:candidates.df[i, "y1"], x = candidates.df[i, "x1"]) )
      cutouts.grid <- cutouts.grid + 3 * Matrix::sparseMatrix(perim.df[, 1], perim.df[, 2], x = 1L, 
        dims = dim(cutouts.grid))
    }
    
  }
  
  if (display.perimeter) {cutouts.grid@x[cutouts.grid@x > 2]  <- 3 }
  
  label <- c(LETTERS, letters)[seq_len(nrow(candidates.df))]
  
  candidates.df <- cbind(data.frame(label, stringsAsFactors = FALSE), candidates.df)
  
  list(map.mat = cutouts.grid, candidates.df = candidates.df)
  
}
