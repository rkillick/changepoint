.onAttach <- function(libname, pkgname)
{
	f <- read.dcf(file.path(libname, pkgname, "DESCRIPTION"),
                      c("Version", "Date"))
        packageStartupMessage('Successfully loaded changepoint package version ',
		f[1,1],
		'\n Note that the default method in the cpt.* functions has changed from AMOC to PELT',
		 '\n See NEWS for details of changes.')
}
