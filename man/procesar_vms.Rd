\name{procesar_vms}
\alias{procesar_vms}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
procesar datos SISESAT
}
\description{
Procesa de manera automarica los datos raw SISESAT para las 8 empresas: AUSTRAL, CAPRICORNIO, CENTINELA, CFG, DIAMANTE, EXALMAR HAYDUK y TASA. Para ello en necesario instalar el paquete vmsR --> install_github("PabloMBooster/vmsR").
}
\usage{
procesar_vms(temporada = temporada, dir_data = dir_data, empresas = empresas)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{temporada}{
ID  de la temporada de pesca
}
  \item{dir_data}{
directorio de los datos
}
  \item{empresas}{
nombre de las 8 empresas
}
}
\details{
en el caso de incluir otras empresas
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

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
