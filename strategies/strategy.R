getOrders <- function(store,
                      newRowList,
                      currentPos,
                      info,
                      params) {
  ###########################################################################
  # You do not need to edit this next part of the code
  ###########################################################################
  allzero  <-
    rep(0, length(newRowList)) # used for initializing vectors
  pos <- allzero
  
  if (is.null(store))
    store <- initStore(newRowList)
  else
    store <- updateStore(store, newRowList)
  ###########################################################################
  
  ###########################################################################
  # This next code section is the only one you
  # need to edit for getOrders
  #
  # The if condition is already correct:
  # you should only start computing the moving
  # averages when you have enough (close) prices
  # for the long moving average
  ###########################################################################
  if (store$iter > params$lookbacks$long) {
    # ENTER STRATEGY LOGIC HERE
    
    # You will need to get the current_close
    # either from newRowList or from store$cl
    
    # You will also need to get prices
    # from store$cl
    
    # With these you can use getTMA, getPosSignFromTMA
    # and getPosSize to assign positions to the vector
    # print(length(newRowList))
    for (i in 1:length(newRowList)) {
      #print(c("aa",newRowList[[params$series[i]]]$Close))
      #print(c(i,length(store$cl[[i]])))
      if (length(store$cl[[i]]) > params$lookbacks$long) {
        current_close <- newRowList[[i]]$Close
        #print(current_close)
        current_sign <-
          getPosSignFromTMA(getTMA(last(
            store$cl[[i]], params$lookbacks$long
          ), params$lookbacks))
        pos[[i]] <- getPosSize(current_close) * current_sign
        
        
      }
    }
    
  }
  
  ###########################################################################
  
  ###########################################################################
  # You do not need to edit the rest of this function
  ###########################################################################
  marketOrders <- -currentPos + pos
  
  return(
    list(
      store = store,
      marketOrders = marketOrders,
      limitOrders1 = allzero,
      limitPrices1 = allzero,
      limitOrders2 = allzero,
      limitPrices2 = allzero
    )
  )
}

###############################################################################

getTMA <- function(prices, lookbacks) {
  # prices should be an xts with one of its columns called "Close"
  
  # lookbacks should be list with exactly three elements:
  # lookbacks$short  is an integer
  # lookbacks$medium is an integer
  # lookbacks$long   is an integer
  
  # It should be the case that:
  # lookbacks$short < lookbacks$medium < lookbacks$long
  
  ###########################################################################
  ## START OF 6 CHECKS
  # For E01..E06: Replace TRUE to match the message in the call to stop()
  if (!all(c("long", "short", "medium") %in% names(lookbacks)))
    stop("E01: 'short', 'medium', or 'long' is missing from names(lookbacks)")
  if (!all(sapply(lookbacks, function(i)
    is.integer(i))))
    stop("E02: At least one lookback is not an integer according to is.integer()")
  if (lookbacks$short >= lookbacks$medium |
      lookbacks$medium >= lookbacks$long)
    stop("E03: lookbacks$short < lookbacks$medium < lookbacks$long doesn't hold")
  if (!is.xts(prices))
    stop("E04: prices is not an xts according to is.xts()")
  if ("Close" %in% names(prices))
    if (nrow(prices$Close) < lookbacks$long)
      stop("E05: prices does not enough rows")
  if (!"Close" %in% names(prices))
    stop("E06: prices does not contain a column 'Close'")
  ## END OF 6 CHECKS
  ###########################################################################
  
  ret <- list(short = 0,
              medium = 0,
              long = 0)
  
  # You need to replace the assignment to ret so that the returned object:
  #    - is a list
  #    - has the right names (short, medium, long), and
  #    - contains numeric and not xts objects
  #    - and contains the correct moving average values, which should
  #      have windows of the correct sizes that all end in the
  #      same period, be the last row of prices
  for (i in 1:length(ret)) {
    res <- SMA(prices$Close, n = lookbacks[[i]])
    ret[[i]] <- as.numeric(res)[nrow(res)]
  }
  
  return(ret)
}

getPosSignFromTMA <- function(tma_list) {
  # This function takes a list of numbers tma_list with three elements
  # called short, medium, and long, which correspond to the SMA values for
  # a short, medium and long lookback, respectively.
  
  # Note that if both this function and getTMA are correctly implemented
  # then the following should work with correct input arguments:
  # getPositionFromTMA(getTMA(prices,lookbacks))
  
  # This function should return a single number that is:
  #       -1 if the short SMA < medium SMA < long SMA
  #        1 if the short SMA > medium SMA > long SMA
  #        0 otherwise
  if (tma_list$short < tma_list$medium &
      tma_list$medium < tma_list$long)
    return(-1)
  else if (tma_list$short > tma_list$medium &
           tma_list$medium > tma_list$long)
    return(1)
  else
    return(0)
}

getPosSize <- function(current_close, constant = 1000) {
  # This function should return (constant divided by current_close)
  # rounded down to the nearest integer
  return(floor(constant / current_close))
}

###############################################################################
# The functions below do NOT need to be edited
###############################################################################
initClStore  <- function(newRowList) {
  clStore <- lapply(newRowList, function(x)
    x$Close)
  return(clStore)
}
updateClStore <- function(clStore, newRowList) {
  clStore <-
    mapply(function(x, y)
      rbind(x, y$Close), clStore, newRowList, SIMPLIFY = FALSE)
  return(clStore)
}
initStore <- function(newRowList, series) {
  return(list(iter = 1, cl = initClStore(newRowList)))
}
updateStore <- function(store, newRowList) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl, newRowList)
  return(store)
}
