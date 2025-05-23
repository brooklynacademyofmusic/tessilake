% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tessi.R
\name{tessi_list_tables}
\alias{tessi_list_tables}
\title{tessi_list_tables}
\usage{
tessi_list_tables()
}
\value{
data.table of configured Tessitura tables with columns short_name, long_name, base_table and primary_keys
}
\description{
The list of Tessitura tables is configured in the extdata/tessi_tables.yml file in the package directory
supplemented by the tessi_tables dictionary in config.yml
}
\details{
\subsection{yml format}{

\if{html}{\out{<div class="sourceCode">}}\preformatted{\{short_name\}:
   long_name: \{name of table/view to be loaded\}
   base_table: \{the underlying table being queried that has primary_keys\}
   primary_keys: \{the primary key(s) as a value or a list of values\}
}\if{html}{\out{</div>}}
}
}
\examples{
# customers:
#   long_name: BI.VT_CUSTOMER
#   base_table: T_CUSTOMER
#   primary_keys: customer_no

tessi_list_tables()[short_name == "customers"]

}
