\name{PrevEstimate}
\alias{PrevEstimate}
\title{Estimate of prevalence for a given set of interactions}
\description{
Estimate prevalences of interactions using min-wise hashing.
}
\usage{
PrevEstimate(interactions, z, L = 100L, output_list = FALSE)
}
\arguments{
  \item{interactions}{the list of interactions (as int vectors).
  }
  \item{z}{data matrix where each row corresponds to an observation and columns correspond to variables. Can be in sparse matrix format (inherit from class "sparseMatrix" in the \pkg{Matrix} package).
  }
  \item{L}{number of rows of the min-wise hash matrix used to estimate prevalences. A larger value will result in more accurate estimates, but computation time will increase linearly with \code{L}.
}
  \item{output_list}{if \code{FALSE} returns each interaction set as a string with variable indices separated by spaces. If \code{TRUE} returns each interaction set as an integer vector.
}
}
\details{
  This is a chunk of code from \code{RIT}:  we ran minhash over a given
  set of interactions.
}
\value{
If \code{output_list} is \code{FALSE} (the default), the output is either a data frame (if \code{z0} is omitted) or list of two data frames (if \code{z0} is supplied). The data frames have first column a character vector of interaction sets with the variables in the sets separated by spaces, and second column the estimated prevalences. When \code{z0} is supplied, the interactions in the first component of the list named \code{Class1} are those which are prevalent in \code{z} and their prevalences in \code{z}, \code{z0} are reported. The second component named named \code{Class0} contains those interactions prevalent in \code{z0} and their prevalences in \code{z}, \code{z0}.

When \code{output_list} is \code{TRUE}, each interaction is reported as an integer vector and so the collection of interactions is a list of such vectors.
}
\references{
Shah, R. D. and Meinshausen, N. (2014) Random Intersection Trees. \emph{Journal of Machine Learning Research}, \bold{15}, 629--654.
}
\author{
Hyun Jik Kim, Rajen D. Shah
}
\examples{
## Generate a binary matrices
z <- matrix(rbinom(250*500, 1, 0.3), 250, 500)

## Make the first and second cols of z identical
## so the set 1, 2 has prevalence roughly 0.3 compared
## to roughly 0.09 for any other pair of columns
z[, 1] <- z[, 2]

## Calculate prevalences

PrevEstimate(list(c(1,2), c(1,3), c(1,2,3), c(1,3,4)), z)
PrevEstimate(list(c(1,2), c(1,3), c(1,2,3), c(1,3,4)), z,
             output_list=TRUE)

}
