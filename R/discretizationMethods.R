#' @title Wang Discretization
#' @author Maria Roy and Girish Pulinkala (fixes)
#' @description It is a supervised method that uses the class distribution diversity (CDD) as a measure to discriminate the power of an interval and three CDD-based discretization criteria. CDD is a measure of how different class distributions are between intervals.
#' @param df A decision table
#' @param m Number of intervals to divide the range of each feature when computing candidate cutpoints. Controls the granularity of discretization. Default is 20.
#' @param alpha Threshold between 0 and 1 to define if a state is informative. Default is 0.1.
#' @param lambda Threshold to define if the weaker discrimination is still strong and we have one or two boundaries. Default is 0.05.
#' @returns A list of discretized decision table and cutpoints to use for external validation
#' @export
#' @examples
#' # ros_data_discretized <- wang_discretize(ros_data, m = 50, alpha = 0.1, lambda = 0.05)

cdd_discretize <- function(df, m = 20, alpha = 0.1, lambda = 0.05) {
  # alpha: threshold between 0 and 1 to define if a state is informative
  # lambda: threshold to define if the weaker discrimination is still strong
  # and we have one or two boundaries

  classes <- df[[ncol(df)]]
  class_levels <- unique(classes)

  N1 <- sum(classes == class_levels[1])
  #print(paste("N1:", N1))
  N2 <- sum(classes == class_levels[2])
  #print(paste("N2:", N2))

  # Discretize a single gene column
  discretize_gene <- function(values, classes) {

    # Step 2: min & max
    xmin <- min(values)
    xmax <- max(values)

    # Step 3: uniformly divide [min, max] into m intervals
    cutpoints <- seq(xmin, xmax, length.out = m + 1)
    left_bounds  <- cutpoints[-length(cutpoints)] # remove last element
    right_bounds <- cutpoints[-1] # remove first element

    # Step 4: Compute CDD for all left-open intervals
    # loop over each candidate threshold l in right bounds
    # For each l, compute cdd
    # The result cdds is a vector of length m storing the CDDs for all intervals (−∞, l]
    cdds <- sapply(right_bounds, function(l) {
      # Each candidate interval is (−∞, l]
      in_interval <- values <= l
      n1 <- sum(classes[in_interval] == class_levels[1])
      n2 <- sum(classes[in_interval] == class_levels[2])
      (n1 / N1) - (n2 / N2)
    })

    Dmax <- max(cdds)
    Dmin <- min(cdds)

    Lmax <- right_bounds[which.max(cdds)]
    Lmin <- right_bounds[which.min(cdds)]

    # Step 5: Global CDD (Eq.2)
    Delta <- abs(Dmax - Dmin)

    # Step 6 — Criterion 1: 1-state?
    if (Delta < alpha) {
      return(rep(1, length(values)))
    }

    # Step 7 — Criterion 2. Should we use 3 states?
    d <- min(abs(Dmax), abs(Dmin))  # Eq(3)

    # Step 8 — Criterion 3
    if (d >= lambda) {
      # 3 states using both cutpoints (sort them)
      cuts <- sort(c(Lmax, Lmin))
      states <- cut(values, breaks = c(-Inf, cuts, Inf), labels = c(1,2,3))

      return(list(states= states, cuts = cuts))

    } else {
      # 2 states using whichever is stronger
      cutpoint <- if (abs(Dmax) >= abs(Dmin)) Lmax else Lmin
      states <- cut(values, breaks = c(-Inf, cutpoint, Inf), labels = c(1,2))
      return(list(states = states, cuts = cutpoint))

    }
  }


  # Iterate over genes
  cut_info <- list()   # store cutpoints per gene
  result <- data.frame(row = rownames(df))

  for (gene in colnames(df)[-ncol(df)]) {
    res <- discretize_gene(df[[gene]], classes)
    if(is.atomic(res)){
      result[[gene]] <- res
      cut_info[[gene]] <- NULL
    }else{
      result[[gene]] <- res$states
      cut_info[[gene]] <- res$cuts
    }

  }

  result$decision <- classes
  rownames(result) <- result$row
  result$row <- NULL

  return(list(discretized = result, cutpoints = cut_info))
}





#' @title Top-X Discretization
#' @author Maria Roy and Girish Pulinkala (fixes)
#' @description This method discretizes numeric columns in a dataset by selecting the top X% of values in each column. For each feature it computes the cutoff value corresponding to the top X% using quantiles.
#' @param df  Input dataframe containing numeric values and decision column.
#' @param x User-specified percentage.
#' @returns A  discretized decision table
#' @import tibble
#' @import dplyr
#' @export
#' @examples
#' # ros_data_top <- top_x(ros_data, x = 10, decision_col = "protectionStatus")
#'

# Top X%
top_x <- function(df, x) {

  decision_col<-names(df)[ncol(df)]

  # Get row names from dataframe
  discretized_data <- data.frame(rownames(df))

  # Iterate over each column
  for (i in 1:(ncol(df)-1)) {

    gene_values <- df[,i]

    cutoff <- quantile(gene_values, probs = 1- x/100, na.rm = TRUE)

    discretized_col <- ifelse(gene_values >= cutoff, 1, 0)

    discretized_data[, names(df)[i]] <- discretized_col


  }

  # Add decision column to discretized data
  discretized_data[[decision_col]] <- df[[decision_col]]

  discretized_data <- discretized_data %>% tibble::column_to_rownames(.,'rownames.df.')
  return(discretized_data)

}


#' @title MID RANGE METHOD Discretization
#' @author Maria Roy and Girish Pulinkala (fixes)
#' @description This method discretizes numeric columns in a dataset by using the midrange (the average of the minimum and maximum values) of each column as a threshold. For each feature compute the midrange threshold and obtain a binary outcome.
#' @param df  Input dataframe containing numeric values and decision column.
#'
#' @returns A discretized decision table
#' @import tibble
#' @import dplyr
#' @export
#' @examples
#' # ros_data_midrange <- mid_range(ros_data, decision_col = "protectionStatus")

# MID RANGE METHOD
mid_range <- function(df) {

  decision_col<-names(df)[ncol(df)]

  # Get row names from dataframe
  discretized_data <- data.frame(rownames(df))

  # Iterate over each column
  for (i in 1:(ncol(df)-1)) {
    # For each column (gene) calculate min and max values
    min_value <- min(df[,i])
    max_value <- max(df[,i])

    threshold <- (max_value + min_value ) / 2

    discretized_col <- ifelse(df[,i] > threshold, 1, 0)

    discretized_data[, names(df)[i]] <- discretized_col

  }

  # Add decision column to discretized data
  discretized_data[[decision_col]] <- df[[decision_col]]

  discretized_data <- discretized_data %>% tibble::column_to_rownames(.,'rownames.df.')
  return(discretized_data)

}

#' @title Median Discretization
#' @author Maria Roy and Girish Pulinkala (fixes)
#' @description Discretize numeric columns using the median of each column as a threshold. Each feature will be converted into a binary category, depending if the value is greater than the median or not.
#' @param df  Input dataframe containing numeric values and decision column.
#'
#' @returns A discretized decision table
#' @import tibble
#' @import dplyr
#' @export
#' @examples
#' # ros_data_median <- median_discretization(ros_data, decision_col = "protectionStatus")

# MEDIAN
median_discretization <- function(df) {

  decision_col<-names(df)[ncol(df)]

  # Get row names from dataframe
  discretized_data <- data.frame(rownames(df))

  # Iterate over each column
  for (i in 1:(ncol(df)-1)) {
    # For each column (gene) calculate median values
    median_value <- median(df[,i])

    discretized_col <- ifelse(df[,i] > median_value, 1, 0)

    discretized_data[, names(df)[i]] <- discretized_col

  }

  # Add decision column to discretized data
  discretized_data[[decision_col]] <- df[[decision_col]]

  discretized_data <- discretized_data %>% tibble::column_to_rownames(.,'rownames.df.')
  return(discretized_data)

}


#' @title Mean plus Standard deviation Discretization
#' @author Maria Roy and Girish Pulinkala (fixes)
#' @description This behaves as the mean method but modified to obtain three categories based on their deviation from the mean. In this case, for each feature, the mean and the standard deviation are computed to obtain a threshold multiplier and define different levels.
#' @param df  Input dataframe containing numeric values and decision column.
#' @param alpha  Threshold between 0 and 1 to define if a state is informative. Default is 1. #NOT SURE HERE
#'
#' @returns A discretized decision table
#' @import tibble
#' @import dplyr
#' @export
#' @examples
#' # ros_data_meansd <- mean_sd(ros_data, decision_col = "protectionStatus")

# MEAN PLUS STANDARD DEVIATION
mean_sd <- function(df, alpha = 1) {

  decision_col<-names(df)[ncol(df)]

  # Initialize output dataframe with row names
  discretized_data <- data.frame(sample = rownames(df))

  for (i in 1:(ncol(df)-1)) {

    gene_values <- df[, i]
    mean_value <- mean(gene_values, na.rm = TRUE)
    sd_value <- sd(gene_values, na.rm = TRUE)

    # 3-level discretization: -1, 0, 1
    discretized_col <- ifelse(
      gene_values < mean_value - alpha * sd_value, -1,
      ifelse(
        gene_values > mean_value + alpha * sd_value, 1, 0
      )
    )

    discretized_data[, names(df)[i]] <- discretized_col
  }

  # Add decision column
  discretized_data[[decision_col]] <- df[[decision_col]]

  discretized_data <- discretized_data %>% tibble::column_to_rownames(var = "sample")

  return(discretized_data)
}


#' @title Max X% Max Discretization
#' @author Maria Roy and Girish Pulinkala (fixes)
#' @description This method discretizes numeric columns in a dataset based on a percentage of their maximum value. Each feature is converted into a binary category based on a threshold. The threshold is computed as: Max * (1− x/100).
#' @param df  Input dataframe containing numeric values and decision column.
#' @param x  User-specified percentage.
#'
#' @returns A discretized decision table
#' @import tibble
#' @import dplyr
#' @export
#' @examples
#' # ros_data_MaxXMax <- max_x_max(ros_data, x=10, decision_col = "protectionStatus")


# Max - X% Max
max_x_max <- function(df, x) {

  decision_col<-names(df)[ncol(df)]

  # Get row names from dataframe
  discretized_data <- data.frame(rownames(df))

  # Iterate over each column
  for (i in 1:(ncol(df)-1)) {
    # For each column (gene) calculate max value
    max_value <- max(df[,i])

    threshold <- max_value * (1 - x/100)

    discretized_col <- ifelse(df[,i] > threshold, 1, 0)

    discretized_data[, names(df)[i]] <- discretized_col


  }

  # Add decision column to discretized data
  discretized_data[[decision_col]] <- df[[decision_col]]

  discretized_data <- discretized_data %>% tibble::column_to_rownames(.,'rownames.df.')
  return(discretized_data)

}

#' @title Kmeans clustering Discretization
#' @author Maria Roy and Girish Pulinkala (fixes)
#' @description K-means is an unsupervised method, it groups similar data points into clusters without prior knowledge (labeled data). Organize data based on similarity / distance to cluster centers.
#' @param df  Input dataframe containing numeric values and decision column.
#' @param k Number of desired distinct clusters
#'
#' @returns A discretized decision table
#' @import tibble
#' @import dplyr
#' @export
#' @examples
#' # ros_data_kmeans <- kmeans_f(ros_data, k= 3, decision_col = "protectionStatus")
#'

# K-MEANS CLUSTERING

kmeans_discretization <- function(df, k = 3) {

  decision_col<-names(df)[ncol(df)]

  set.seed(123)
  message("Setting seed to 123 for reproducible results")

  # Remove last column, kmeans function accepts numerical values
  df_numerical <- df[, -ncol(df)]

  # Create dataframe with rownames form df
  discretized_data <- data.frame(row.names = rownames(df))

  # Iterate through df columns
  for (col_name in colnames(df_numerical)) {
    # Calculate kmeans of each gene
    km_res <- stats::kmeans(df_numerical[[col_name]], centers = k)

    # Order clusters by their center values (low to high)
    center_order <- order(km_res$centers[, 1])

    # Create a mapping: rank[old_cluster] = new_cluster
    rank <- numeric(k)
    rank[center_order] <- 1:k

    discretized_data[[col_name]] <- rank[km_res$cluster]
  }

  # Add the discretized column to the resulting dataframe
  discretized_data$decision_col <- df[[ncol(df)]]
  return(discretized_data)
}

#' @title Fayyad–Irani - Minimum Description Length (MDL)
#' @author Maria Roy and Girish Pulinkala (fixes)
#' @description It is a supervised univariate discretization method centered around scoring the target variable entropy. It consists of a greedy search method that recursively discretizes each partitions at a cutpoint, which minimizes the joint entropy of the two resulting subintervals until a stopping criterion based on MDL:
#' @param df  Input dataframe containing numeric values and decision column.
#'
#' @returns A list of discretized decision table and cutpoints
#' @import tibble
#' @import discretization
#' @export
#' @examples
#' # ros_data_kmeans <- kmeans_f(ros_data, k= 3, decision_col = "protectionStatus")
#'

# Fayyad–Irani MDLP
mdl_fi <- function(df) {


  decision_col<-names(df)[ncol(df)]

  # Get row names from dataframe
  discretized_data <- data.frame(rownames(df))

  # Calculate MDL in entire dataframe
  mdl_result <- discretization::mdlp(df)

  # Add the discretized column to the resulting dataframe
  discretized_data <- mdl_result$Disc.data

  cut_points <- mdl_result$cutp

  # Add decision column to discretized data
  discretized_data[[decision_col]] <- df[[decision_col]]

  return(list(
    data = discretized_data,
    cuts = mdl_result$cutp
  ))


}

#' @title Equal width binning
#' @author Maria Roy and Girish Pulinkala (fixes)
#' @description a data preprocessing technique that divides a continuous numerical variable into a specified number of categories (bins), where each bin covers the exact same interval range
#' @param df  Input dataframe containing numeric values and decision column.
#' @param n_bins number of bins
#' @returns A discretized decision table
#' @import tibble
#' @import infotheo
#' @export
#' @examples
#' # ros_data_ewb <- equal_width_bin(ros_data, decision_col = "protectionStatus", n_bins=3)
#'

# EQUAL WIDTH BINNING
equal_width_bin <- function(df, n_bins = 3) {

  decision_col<-names(df)[ncol(df)]

  # Perform equal-width discretization on all columns except the decision column
  feature_cols <- setdiff(names(df), decision_col)
  discretized_features <- infotheo::discretize(df[feature_cols], method = "equalwidth", nbins = n_bins)

  # Convert to data frame (in case discretize returns a matrix)
  discretized_data <- as.data.frame(discretized_features)

  # Add decision column as last column
  discretized_data[[decision_col]] <- df[[decision_col]]

  # Preserve original row names
  rownames(discretized_data) <- rownames(df)

  return(discretized_data)
}



#' @title Entropy based discretization

# ENTROPY BASED DISCRETIZATION
# TDT part
TDT_discretize <- function(df) {
  # Load packages
  library(dplyr)
  library(tibble)

  # Create a new df to store discretized data, preserving row names
  discretized_data <- data.frame(rownames = rownames(df))

  # Iterate over each column except the last (decision label)
  for (i in 1:(ncol(df)-1)) {
    gene_values <- df[, i]  # Get expression values for the current gene
    n <- length(gene_values)

    # Sort the gene expression values
    sorted_vals <- sort(gene_values)

    # Initialize variables to track the best split
    best_var_sum <- Inf
    best_split <- NA

    # Try all possible split points (except the first and last, to avoid empty clusters)
    for (e in 2:(n-2)) {
      S1 <- sorted_vals[1:e]       # First cluster
      S2 <- sorted_vals[(e+1):n]   # Second cluster

      # Sum of variances of the two clusters
      var_sum <- var(S1) + var(S2)

      # Update if we find a smaller variance sum
      if (var_sum < best_var_sum) {
        best_var_sum <- var_sum
        best_split <- e
      }
    }

    # Compute TDT threshold as midpoint between the last element of S1 and first of S2
    threshold <- (sorted_vals[best_split] + sorted_vals[best_split + 1]) / 2

    # Discretize gene values: -1 if <= threshold, +1 if > threshold
    discretized_col <- ifelse(gene_values <= threshold, -1, 1)

    # Add the discretized column to the resulting dataframe
    discretized_data[, names(df)[i]] <- discretized_col
  }

  # Add decision column to discretized data
  discretized_data$decision <- df$decision

  # Restore row names
  discretized_data <- discretized_data %>% column_to_rownames(var = 'rownames')

  return(discretized_data)
}



#' @title BAYESIAN DISCRETIZATION
#' @author Maria Roy and Girish Pulinkala (fixes)
#' @description a data preprocessing technique that divides a continuous numerical variable into a specified number of categories (bins), where each bin covers the exact same interval range
#' @param df  Input dataframe containing numeric values and decision column.
#' @param desired_intervals Desired number of bins/levels. Default is set to 3.
#' @returns A list of discretized decision table and cutpoints
#' @import tibble
#' @import infotheo
#' @export
#' @examples
#' # ros_data_bayesian <- ebd_discretize_df(ros_data, decision = "protectionStatus" , desired_intervals = 3)

# BAYESIAN DISCRETIZATION



# Efficient Bayesian Discretization
ebd_discretize_df <- function(df, desired_intervals = 3) {


  decision_col<-names(df)[ncol(df)]

  # Compute Prior(k) using Eq 10 - place a cut after each unique X value
  compute_prior_vec <- function(X, lambda) {
    n <- length(X) # store unique X values
    Prior <- numeric(n + 1) # vector for each candidate cutpoint - initially at 0
    Prior[1] <- 1        # lowest interval
    Prior[n + 1] <- 1    # highest interval
    d_total <- max(X) - min(X) # total range of X

    for (k in 1:(n - 1)) {
      d_k <- X[k + 1] - X[k] # gap between consecutive unique X values
      # prob of a cut after X[k]
      Prior[k + 1] <- 1 - exp(-lambda * d_k / d_total)
    }
    return(Prior)
  }

  # Compute interval prior using Eq 8 (log-space)
  compute_interval_prior <- function(Prior_vec, a, b) {
    # Prior_vec[k] = prob of placing a cut BEFORE unique value k
    # 1 - Prior_vec[k] = prob of NOT placing a cut before k
    if (b == a) return(log(Prior_vec[b]))
    # prob of no cuts inside the interval ->
    # P (interval[a,b]) = (prob of no cuts from a to b-1) x (prob of cut at b)
    prod_term <- prod(1 - Prior_vec[a:(b-1)])
    # prob of cut at the end of the interval
    prior_interval <- prod_term * Prior_vec[b]
    return(log(prior_interval))
  }

  # Log marginal likelihood (Eq 7)
  # Compute the Dirichlet Multinomial log-likelihood for counts U
  log_marginal_likelihood <- function(U) {
    J <- length(U)
    n_i <- sum(U)
    if (n_i == 0) return(-Inf) # if interval has no data
    log_num <- sum(lgamma(U + 1)) + lgamma(J) # lgamma = factorial
    log_den <- lgamma(J + n_i)
    return(log_num - log_den)
  }

  # Column discretization
  ebd_discretize_column <- function(X, Z, desired_intervals = 3) {

    # Convert target variable to factor
    Zf <- as.factor(Z)
    class_levels <- levels(Zf)
    J <- length(class_levels) # number of unque labels

    # Sort X in ascending order and Z
    ord <- order(X)
    Xs <- X[ord]; Zs <- Zf[ord]
    valid_idx <- which(!is.na(Xs) & !is.na(Zs)) # remove NA
    Xv <- Xs[valid_idx]; Zv <- Zs[valid_idx]

    # Unique predictor values
    uniqX <- sort(unique(Xv))
    n_unique <- length(uniqX)

    # Counts of class j per unique X
    W <- matrix(0L, nrow = n_unique, ncol = J)
    Zv_idx <- as.integer(Zv)
    uniq_index <- match(Xv, uniqX)
    for (i in seq_along(uniq_index)) {
      W[uniq_index[i], Zv_idx[i]] <- W[uniq_index[i], Zv_idx[i]] + 1L
    }

    # Cumulative sum to get interval counts quickly
    cumW <- apply(W, 2, cumsum)
    interval_counts <- function(a, b) {
      if (a == 1) return(cumW[b, ])
      return(cumW[b, ] - cumW[a - 1, ])
    }

    # Compute Prior vector (Eq 10) - distance-based method
    #lambda <- (desired_intervals - 1) / (max(Xv) - min(Xv))
    lambda <- desired_intervals - 1   # expected number of cut points
    Prior_vec <- compute_prior_vec(uniqX, lambda)

    # DP arrays
    logV <- rep(-Inf, n_unique) # best log score
    backptr <- integer(n_unique) # stores starting index of last interval in optimal split

    # DP loop: for each end index a, try all possible start indices b
    for (a in seq_len(n_unique)) {
      best_log <- -Inf
      best_b <- 1L
      for (b in seq_len(a)) {
        U <- interval_counts(b, a)
        log_ml <- log_marginal_likelihood(U)
        prev_log <- if (b == 1) 0 else logV[b - 1] # best log score up to b-1
        log_prior <- compute_interval_prior(Prior_vec, b, a)
        total_log <- prev_log + log_ml + log_prior # total log score
        if (total_log > best_log) {
          best_log <- total_log
          best_b <- b
        }
      }
      logV[a] <- best_log
      backptr[a] <- best_b
    }

    # Reconstruct intervals
    intervals <- list()
    a <- n_unique
    while (a >= 1) {
      b <- backptr[a]
      intervals[[length(intervals) + 1]] <- c(b, a)
      if (b == 1) break
      a <- b - 1
    }
    intervals <- rev(intervals)

    # Compute cutpoints.
    # Take last index of each interval (Except last one)
    # Place cutpoint between consecutive unique X values
    cut_positions <- sapply(intervals[-length(intervals)], function(iv) iv[2])
    cut_values <- numeric(0)
    if (length(cut_positions) > 0) {
      for (pos in cut_positions) {
        cut_values <- c(cut_values, (uniqX[pos] + uniqX[pos + 1]) / 2)
      }
    }


    # Assign labels
    if (length(cut_values) == 0) {
      # If no cuts -> assign all X to single bin
      labels <- factor(rep(1L, length(X)), levels = 1L)
    } else {
      # Else -> discretize X into bins using cut()
      breaks <- c(-Inf, cut_values, Inf)
      labels <- cut(X, breaks = breaks, labels = seq_len(length(cut_values) + 1),
                    include.lowest = TRUE, right = FALSE)
    }

    return(list(
      cuts = cut_values,
      labels = labels,
      intervals = intervals,
      log_score = logV[n_unique],
      n_intervals = length(intervals)
    ))
  }



  discretized <- df # copy df
  cuts_list <- list() # store cutpoints

  # Get predictor names
  predictors <- setdiff(names(df), decision)

  # Iterate over predictor columns
  for (col in predictors) {
    cat("EBD discretizing:", col, "...\n")
    res <- ebd_discretize_column(df[[col]], df[[decision]], desired_intervals = desired_intervals)
    discretized[[col]] <- res$labels # replace original col with discretized label
    cuts_list[[col]] <- res$cuts # store cutpoints
    cat("  -> bins:", length(res$cuts) + 1,
        "cuts:", if(length(res$cuts) > 0) paste(round(res$cuts, 5), collapse=", ") else "(none)", "\n")
  }

  return(list(data = discretized, cuts = cuts_list))
}













