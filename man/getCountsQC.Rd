\name{getCountsQC}
\alias{getCountsQC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create table with both Total and Antigens Counts Data
}
\description{
This function joins together the total counts data (counts_output) and the antigen-specific counts data (antigen_counts_output).
}
\usage{
getCountsQC(antigen_counts_output, counts_output)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{antigen_counts_output}{Output from `getAntigenCounts` (reactive).}
  \item{counts_output}{Output from `getCounts` (reactive).}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Table with all counts data per antigen and overall.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Dionne Argyropoulos
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
getCountsQC(antigen_counts_output, counts_output)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
