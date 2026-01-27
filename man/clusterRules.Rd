\name{clusterRules}
\alias{clusterRules}
\title{
  Hierarchical clustering of rules
}
\description{
  Prints a heatmap of clustered rules and objects. 
}
\usage{
  cluster_rules(training_df,recal,support=7,show_colnames=FALSE,show_rownames=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{training_df}{
    a dataframe used for training. e.g. 'autcon' dataset
  }
  \item{recal}{
    a dataframe consisting of recalulated rules 
  }
  \item{support}{
    an integer specifying minimum support inorder to trim rules
  }
  \item{show_colnames}{
    Logical. If TRUE the column names will be displayed
  }
  \item{show_rownames}{
    Logical. If TRUE the row names will be displayed
  }
  \item{fontsize}{
    Integer. The value sets the	base fontsize for the plot
  }
  
  
}
\details{
  %%  ~~ If necessary, more details than the description above ~~
}


\value{
  \item{output}{A heatmap showing the hierarchically clustered rules and objects. 
}
\references{
  %% ~put references to the literature/web site here ~
}
\author{
  Girish Pulinkala
}
\note{
  %%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
  
  \seealso{
    %% ~~objects to See Also as \code{\link{help}}, ~~~
  }
\examples{
  library(R.ROSETTA)
  set.seed(1)
  
  out <- rosetta(autcon)
  rules <- out$main
  
  
  #change 1,2,3 gene levels into down,nochange,up
  IfThenRulesDNU <- viewRules(rules, setLabels=TRUE, labels=c("down","nochange", "up"))
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
