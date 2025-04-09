\name{check_repeats}
\alias{check_repeats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check Beads to Repeat
}
\description{
This function gets the count data and outputs a table of the isolates to repeat or a statement to confirm that none need to be repeated.
}
\usage{
check_repeats(counts_output)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{counts_output}{Output from `getCounts` (reactive).}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Data frame with wells to "repeat", OR if no "repeats" found will return text "No repeats necessary".
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
check_repeats(counts_output)
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
