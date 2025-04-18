% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-portfolio.R
\name{Portfolio}
\alias{Portfolio}
\title{Portfolio (R6 Object)}
\description{
R6 Class representing a portfolio object.
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Portfolio-new}{\code{Portfolio$new()}}
\item \href{#method-Portfolio-print}{\code{Portfolio$print()}}
\item \href{#method-Portfolio-get_nav}{\code{Portfolio$get_nav()}}
\item \href{#method-Portfolio-get_position}{\code{Portfolio$get_position()}}
\item \href{#method-Portfolio-clone}{\code{Portfolio$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Portfolio-new"></a>}}
\if{latex}{\out{\hypertarget{method-Portfolio-new}{}}}
\subsection{Method \code{new()}}{
Create New Portfolio R6 object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Portfolio$new(long_name, short_name, nav, positions)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{long_name}}{Portfolio Long Name}

\item{\code{short_name}}{Portfolio Short Name}

\item{\code{nav}}{NAV of portfolio}

\item{\code{positions}}{list of position items}

\item{\code{id}}{Portfolio id}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Portfolio-print"></a>}}
\if{latex}{\out{\hypertarget{method-Portfolio-print}{}}}
\subsection{Method \code{print()}}{
Print
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Portfolio$print()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Portfolio-get_nav"></a>}}
\if{latex}{\out{\hypertarget{method-Portfolio-get_nav}{}}}
\subsection{Method \code{get_nav()}}{
Get Fund NAV
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Portfolio$get_nav()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Portfolio-get_position"></a>}}
\if{latex}{\out{\hypertarget{method-Portfolio-get_position}{}}}
\subsection{Method \code{get_position()}}{
Get list of positions in portfolio
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Portfolio$get_position(id = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{Ticker}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Portfolio-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Portfolio-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Portfolio$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
