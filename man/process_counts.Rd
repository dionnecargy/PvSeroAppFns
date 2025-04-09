\name{process_counts}
\alias{process_counts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Process Counts data from readAntigens output.
}
\description{
A helper function to process counts data.
}
\usage{
process_counts(antigens_output)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{antigens_output}{Output from `readAntigens` (reactive). }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a long table of counts with "Warning" category (<15 == 1 and ≥ 15 == 0) for downstream wrangling.
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
process_counts(antigens_output)
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
