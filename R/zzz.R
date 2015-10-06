.onAttach <- 
function(libname, pkgname) {
    ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), fields="Version")
    packageStartupMessage(" ")
    packageStartupMessage(paste("This is", pkgname, ver))
    packageStartupMessage(" ")
    packageStartupMessage("Type changes(\"diezeit\") to see changes/bug fixes, help(diezeit) for documentation")
    packageStartupMessage("or citation(\"diezeit\") for how to cite diezeit.")
    packageStartupMessage(" ")
}

changes <- 
function(pkg="diezeit") {
    if(pkg=="diezeit") file.show(file.path(system.file(package="diezeit"), "NEWS"))
}
