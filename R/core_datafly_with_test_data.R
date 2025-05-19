#' Datafly algorithm for k-anonymity
#'
#' The datafly algorithm uses generalization and suppression to achieve k-anonymity.
#' Sweeney, L. (1998). Datafly: A system for providing anonymity in medical data. Database Security XI: Status and Prospects, 356-381.
#' @param data A data frame containing the quasi-identifiers and sensitive attributes.
#' @param test_data A data frame containing the quasi-identifiers and sensitive attributes for testing.
#' @param quid A list of column names representing the quasi-identifiers.
#' @param k The minimum number of records that share the same quasi-identifier values.
#' @param gen_try The maximum number of generalization attempts.
#' @param taxonomy A list representing the current hierarchy structure.
#' @return A list containing the training and testing data frames with k-anonymity applied.
#'
#'
#' @export

core_datafly <- function(data, test_data, quid, k=5, gen_try=20, taxonomy){
  if(!is.null(quid)){
    n_old <- nrow(data)
    data[,quid] <- lapply(data[,quid], function(x) if(!is.factor(x)){return(as.factor(x))}else{return(x)})
    if(!is.null(test_data)){
    test_data[,quid] <- lapply(test_data[,quid], function(x) if(!is.factor(x)){return(as.factor(x))}else{return(x)})
    }
    runAgain <- TRUE
    iter <- 0
    while(runAgain) {
      runAgain <- FALSE
      iter <- iter + 1
      freq_tab <- as.data.frame(table(data[,quid]))
      freq_tab[,quid] <- lapply(freq_tab[,quid], as.character)
      freq_tab <- freq_tab[freq_tab$Freq<k & freq_tab$Freq>0,]
      n_new <- nrow(freq_tab)
      i <- 1
      if(n_new==n_old & NROW(freq_tab)>0){
        # surpess
        data <-  data %>% group_by(data[,quid]) %>% mutate(freq=n()) %>% as.data.frame()
        surpress <- function(data, quid_affected, k) {
          # surpress QUID of all records that violate k
          old_levels <- levels(data[,quid_affected])
          data[,quid_affected] <- as.integer(data[,quid_affected])
          data[data$freq<k,quid_affected] <- length(old_levels) + 1
          new_levels <- c(old_levels,"*")
          new_levels <- new_levels[sort(unique(data[,quid_affected]))]
          data[,quid_affected] <- factor(data[,quid_affected], labels = new_levels)
          data[,"freq"] <- NULL
          return(data)
        }
        # sort quid in decreasing order of distinct values (first should be max)
        quids <- sapply(quid, function(x){nlevels(data[,x])})
        quids <- names(sort(quids, decreasing = TRUE))
        for (q in quids) {
          freq_tab <- as.data.frame(table(data[,quid]))
          freq_tab <- freq_tab[freq_tab$Freq<k & freq_tab$Freq>0,]
          if(NROW(freq_tab)>0){
            data <-  data %>% group_by(data[,quid]) %>% mutate(freq=n()) %>% as.data.frame()
            data <- surpress(data, q, k)
          }
        }
        runAgain <- FALSE
      } else if(NROW(freq_tab)>0){
        #generalize
        for(i in 1:nrow(freq_tab)){
          quid_max <- names(which.max(sapply(data[,quid], function(x) length(unique(x))))) # location of (first) max
          child <- freq_tab[i, quid_max]
          hierarchy <- taxonomy[[quid_max]]
          data[,quid_max] <- traverse_hierachy(children = child, attribute=data[,quid_max], hierarchy)
          if(!is.null(test_data)){
          test_data[,quid_max] <- traverse_hierachy(children = child, attribute=test_data[,quid_max], hierarchy)
          }
          runAgain <- TRUE
        }
      } # for loop
      else{
        runAgain <- FALSE   # k holds
      }
      n_old <- NROW(freq_tab)
    } # end while loop
    data <-  data %>% group_by(data[,quid]) %>% mutate(freq=n()) %>% as.data.frame()
     data <- data[data$freq>=k,]
     data[,"freq"] <- NULL
      if(!is.null(test_data)){
      return(list(training=data,testing=test_data))
    }else{
      return(data)
    }
  }
}
