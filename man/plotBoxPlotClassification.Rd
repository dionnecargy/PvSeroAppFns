\name{plotBoxPlotClassification}
\alias{plotBoxPlotClassification}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot RAU by Classification
}
\description{
One example of data visualisation to detect the median and interquartile range of the RAU values per antigen for seropositive and seronegative individuals. Please note that the `classify_final_results` function must be run first.
}
\usage{
plotBoxPlotClassification(all_classifications, selected_threshold)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{all_classifications}{Data frame of `classify_final_results()` for all Sens_Spec thresholds.}
  \item{selected_threshold}{String with the threshold (reactive).}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Box plots with RAU values for each protein stratified by classification (ggplot).
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
plotBoxPlotClassification(all_classifications, "antibody_model")
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
