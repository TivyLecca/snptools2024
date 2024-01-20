\name{obtener_vms}
\alias{obtener_vms}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Obtener datos SISESAT
}
\description{
Carga los datos de los viajes y las trayectorias de pesca SISESAT que ta han sido generado de manera automatica en procesar_vms.

-Los datos de los viajes de pesca se encuentra en la carpeta OUTPUT de cada empresa.

-Los datos de las trayectorias de los viajes de pesca se encuentra en la carpeta TRAYECTORIA de la carpeta DATOS.


}
\usage{
obtener_vms(dir_data = dir_data, empresas = empresas, temporada = temporada, inicio_temporada = inicio_temporada, fin_temporada = fin_temporada)
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
  \item{inicio_temporada}{
fecha de inicio de la temporada de pesca
}
  \item{fin_temporada}{
fecha de fin de la temporada de pesca
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
