getOrders <- function(store, newRowList, currentPos, info, params) {

    ###########################################################################
    # You do not need to edit this next part of the code
    ###########################################################################
    allzero  <- rep(0,length(newRowList)) # used for initializing vectors
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
        # and getPosSize to assign positions to the vector pos
    }
    ###########################################################################

    ###########################################################################
    # You do not need to edit the rest of this function
    ###########################################################################
    marketOrders <- -currentPos + pos

    return(list(store=store,marketOrders=marketOrders,
	                    limitOrders1=allzero,limitPrices1=allzero,
	                    limitOrders2=allzero,limitPrices2=allzero))
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
    if (TRUE)
        stop("E01: 'short', 'medium', or 'long' is missing from names(lookbacks)")
    if (TRUE)
        stop("E02: At least one lookback is not an integer according to is.integer()")
    if (TRUE) 
        stop("E03: lookbacks$short < lookbacks$medium < lookbacks$long doesn't hold")
    if (TRUE)
        stop("E04: prices is not an xts according to is.xts()")
    if (TRUE)
        stop("E05: prices does not enough rows")
    if (TRUE)
        stop("E06: prices does not contain a column 'Close'")
    ## END OF 6 CHECKS 
    ###########################################################################

    ret <- 0

    # You need to replace the assignment to ret so that the returned object:
    #    - is a list 
    #    - has the right names (short, medium, long), and
    #    - contains numeric and not xts objects
    #    - and contains the correct moving average values, which should 
    #      have windows of the correct sizes that all end in the 
    #      same period, be the last row of prices

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
    return(0)
}

getPosSize <- function(current_close,constant=1000) {
    # This function should return (constant divided by current_close) 
    # rounded down to the nearest integer
    return(0)
}

###############################################################################
# The functions below do NOT need to be edited
###############################################################################
initClStore  <- function(newRowList) {
  clStore <- lapply(newRowList, function(x) x$Close)
  return(clStore)
}
updateClStore <- function(clStore, newRowList) {
  clStore <- mapply(function(x,y) rbind(x,y$Close),clStore,newRowList,SIMPLIFY=FALSE)
  return(clStore)
}
initStore <- function(newRowList,series) {
  return(list(iter=1,cl=initClStore(newRowList)))
}
updateStore <- function(store, newRowList) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList) 
  return(store)
}
