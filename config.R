
machine <<- (Sys.info()["nodename"])
pth.dropbox <<- "/home/users/wellerm/"
if (machine == "M11") pth.dropbox <<- "C:/Users/Matt/Dropbox/"
if (machine == "DESKTOP") pth.dropbox <<- "D:/Dropbox/Dropbox/"
if (machine == "IDEA-PC") pth.dropbox <<- "C:/Users/welle_000/Dropbox/"

pth.dropbox.data <<- paste(pth.dropbox, "HEC/IRI_DATA/", sep = "")
pth.dropbox.code <<- paste(pth.dropbox, "HEC/Code/iri/", sep = "")


if (pth.dropbox == "/home/users/wellerm/") {
	pth.dropbox.data <<- paste(pth.dropbox, "IRI_DATA/", sep = "")
	pth.dropbox.code <<- paste(pth.dropbox, "projects/iri/", sep = "")
}
