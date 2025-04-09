\name{plotMFI}
\alias{plotMFI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotMFI(mfi_to_rau_output, location)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mfi_to_rau_output}{Output from `MFItoRAU` (reactive).}
  \item{location}{PNG or ETH (reactive).}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Box plots with MFI values for each protein (ggplot).
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
plotMFI(mfi_to_rau_output, "ETH")
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
