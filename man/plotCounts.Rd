\name{plotCounts}
\alias{plotCounts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Bead Count Data
}
\description{
This function gets the count data and plots the plate image, creating a new facet (i.e., panel) for each antigen and each line represents the different plates so that they can be visualised.
}
\usage{
plotCounts(counts_output, experiment_name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{counts_output}{Output from `getCounts` (reactive).}
  \item{experiment_name}{User-input experiment name (reactive).}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Tile Plot showing binary result of "sufficient beads" with cut-off >15 beads and "repeat" ≤15 beads (ggplot).
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Shazia Ruybal-Pesantez, Dionne Argyropoulos
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
plotCounts(counts_output, experiment_name)
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
