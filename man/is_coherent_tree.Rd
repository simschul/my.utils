% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_tree_tools.R
\name{is_coherent_tree}
\alias{is_coherent_tree}
\title{Checks if a data.tree is coherent, i.e. all child nodes sum correctly to their parent nodes}
\usage{
is_coherent_tree(tree, attribute, tol = 0.001)
}
\arguments{
\item{tree}{a data.tree object}

\item{attribute}{the attribute to test for coherency (needs to be numeric!)}

\item{tol}{}
}
\value{

}
\description{
Checks if a data.tree is coherent, i.e. all child nodes sum correctly to their parent nodes
}
