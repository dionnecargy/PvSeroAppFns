\name{classify_final_results}
\alias{classify_final_results}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Random Forest Classification
}
\description{
This function classifies unknown samples as recently exposed or not (Note: MFItoRAU() needs to be run first to convert to RAU).
}
\usage{
classify_final_results(mfi_to_rau_output, algorithm_type, Sens_Spec, counts_QC_output)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mfi_to_rau_output}{Output from `MFItoRAU` (reactive).}
  \item{algorithm_type}{User-selected algorithm choice:
  (i) "antibody_model" (PvSeroTaT model; default), or
  (ii) "antibody_model_excLF016" (PvSeroTat excluding LF016).
  }
  \item{Sens_Spec}{User-selected Sensitivity/Specificity threshold:
  (i) "maximised" (default),
  (ii) "85\% sensitivity",
  (iii) "90\% sensitivity",
  (iv) "95\% sensitivity",
  (v) "85\% specificity",
  (vi) "90\% specificity".
  (vii) "95\% specificity".
  }
  \item{counts_QC_output}{Output from `getCountsQC` (reactive).}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Data frame with exposure status for every sample.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Lauren Smith, Dionne Argyropoulos
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
classify_final_results(mfi_to_rau_output, "antibody_model", "maximised", counts_QC_output)
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
