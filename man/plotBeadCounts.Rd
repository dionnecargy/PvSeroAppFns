\name{plotBeadCounts}
\alias{plotBeadCounts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Bead Counts per Antigen
}
\description{
Enhances the `plotCounts` output by providing greater resolution, displaying antigens per plate, and enabling SampleID name visibility via hover (transformed to Plotly in server.R).
}
\usage{
plotBeadCounts(antigen_counts_output)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{antigen_counts_output}{Output from `getAntigenCounts` (reactive).}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Dot plot with values > 15 threshold coloured in blue (sufficient beads) and ≤15 beads coloured in red (repeat) faceted by each antigen (ggplot).
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
plotBeadCounts(antigen_counts_output)
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
