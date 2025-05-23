###########################################################################
# Offline interface to Gplates


#Mac examples
#	x<- fetch("pared", "public")[,c("longitude", "latitude")]
#	mo <- fetch("paleomap", "model")
#	reconstruct(x, age=10, model=mo, verbose=TRUE)
#reconstruct(x, age=10, model=mo, verbose=TRUE, path.gplates="/Users/Nick/Downloads/GPlates-2.2.0/gplates.app/Contents/MacOS/gplates")

reconstructGPlates <- function(x, age, model, path.gplates=NULL,dir=NULL, verbose=FALSE, cleanup=TRUE, plateperiod=FALSE, gmeta=FALSE, partitioning="static_polygons", check=TRUE){
	# shortcut - will be put in a matrix automatically
	if(is.na(age)) return(NA)
	if(!inherits(model, "platemodel")) stop("You need a GPlates tectonic object for this method.")
	# 0. Process the attributes
	if(inherits(model@features, "data.frame")){
		if(check){
			# check for the ages!
			if(is.character(x)){
				checkThis <- x
			#fall through, use the partitioning!
			}else{
				checkThis <- partitioning
			}
			# the part corresponding to this
			if(!any(checkThis==rownames(model@features))) stop(paste0("The feature collection '",checkThis,"' is not found."))
			thisPart <-model@features[ checkThis, ] 

			# get the ages
			young <- thisPart[ , "to"]
			old <- thisPart[ , "from"]
			if(!(age>=young && age <=old) ) stop(paste0("Invalid age. '", checkThis, "' is valid between ", old, " Ma and ", young, " Ma."))
		}

		# omit the age info, use characters as features
		nam <- rownames(model@features)
		model@features <- model@features[, "feature_collection"]
		names(model@features) <- nam
	}	

	# if there are multiple rotation files, make sure to merge them first
	if(length(model@rotation)>1) model@rotation <- mergeRotations(model@rotation)


    # 1. FIND GPlates
		# A. get operating system
		os <- getOS()

		# assume a non-retarded operating system...
		win <- FALSE

		# B. should the program look for a default path for gplates?
		if(is.null(path.gplates)){
			# depending on the os
			if(os=="linux"){
				# the GPlates executable itself
				gplatesExecutable <- "gplates"

				# what the user would have entered
				path.gplates <- gplatesExecutable

				# leave the model intact in the namespace (easier debug)
				rotation <- model@rotation
				plateFeatures<- model@features

				# separator character between directories
				dirSep <- "/"

			}
			if(os=="windows"){
				win <- TRUE
				# 1. find GPLATES exectutable if possible
				# directory and executable
				gplatesPaths <- winDefaultGPlates()
				#path to executable
				path.gplates <- paste(gplatesPaths, collapse="/")
				# system call to executable
				gplatesExecutable <- paste("\"", path.gplates, "\"", sep="")

				# 2. replace model paths with \\
				rotation <- gsub("/","\\\\", model@rotation)
				plateFeatures<- gsub("/","\\\\", model@features)

				# characters to include directory 
				dirSep <- "\\\\"

			}
			if(os=="osx"){
				# the GPlates executable itself
				# default
				# gplatesExecutable <- "/Applications/GPlates-2.2.0/gplates.app/Contents/MacOS/gplates"
				gplatesPaths <- macDefaultGPlates()
				gplatesExecutable <- paste(gplatesPaths, collapse="/")
				
				# what the user would have entered
				path.gplates <- gplatesExecutable

				# leave the model intact in the namespace (easier debug)
				rotation <- model@rotation
				plateFeatures <- model@features

				# separator character between directories
				dirSep <- "/"
				
			}

		# look for given path
		}else{
			# separate to form a length 2 vector
			gplatesExecutable <- path.gplates
			
			# leave the model intact in the namespace (easier debug)
			rotation <- model@rotation
			plateFeatures <- model@features

			# separator character between directories
			dirSep <- "/"
			
			# windows needs special treatment
			if(os=="windows"){
				win <- TRUE
			
				# system call to executable
				gplatesExecutable <- paste("\"", path.gplates, "\"", sep="")

				# 2. replace model paths with \\
				rotation <- gsub("/","\\\\", model@rotation)
				plateFeatures <- gsub("/","\\\\", model@features)

				# characters to include directory 
				dirSep <- "\\\\"

			}
			
		}

		# C. one moretest whether gplates was detected or not
			gpTest <- testGPlates(gplatesExecutable, verbose=verbose)

			# if gplates is not present:
			if(!gpTest) stop(paste("The GPlates executable\n	\"", path.gplates,"\"\nwas not found.", sep=""))

	# 2. Setup reconstruction environment
		# folder where files will be executed
		if(is.null(dir)) tempd <- tempdir() else tempd <- dir

		# copy all model feature to working directory - unless it is a shapefile
		sources <- plateFeatures

		for(i in 1:length(sources)){

			# if it is not a shapefile, copy it over
			if(!isShapefile(sources[i])){
				# a single
				plateFeature <-file.path(tempd, fileFromPath(sources[i], win=win))

				# bug fix for windows
				if (win) plateFeature <- gsub("/","\\\\", plateFeature)

				# copy it over
				results <- file.copy(sources[i], plateFeature)
			}else{
				plateFeature <- sources[i]
			}
			plateFeatures[i] <- plateFeature
		}
		names(plateFeatures) <- names(sources)

		# prepare x
		# create a SpatialPointsDataFrame from long-lat matrix

		originals <- NULL 
		if(inherits(x, "matrix")){
			originals <- x
			x <- as.data.frame(x)
		}
			
		# data.frame
		if(inherits(x, "data.frame") & !(inherits(x, "sf") | inherits(x, "Spatial"))){
			# the original
			if(is.null(originals)) originals <- x
			# enforce column names
			colnames(x) <- c("lng", "lat")
			x$ID <- paste0("a", 1:nrow(x))
			theID <- x$ID

			# transform to sf
			x <- sf::st_as_sf(x, coords=c("lng", "lat"))
			
		}
		
	
		# spatial original, enforce sf!
		if(inherits(x, "Spatial")){
			# conserve the originals to know what to have
			originals <- x
			x <- sf::st_as_sf(x)
		}
	
		# for non -spatial/sf this is null
		originalCRS <- NULL
		# enforce projection!
		# if originally an sf!
		if(inherits(x, "sf")){
			if(is.null(originals)) originals <- x
			# the original CRS
			originalCRS <- sf::st_crs(x)
			if(!is.na(originalCRS)){
				xTransform <- sf::st_transform(x, "EPSG:4326")
			}else{
				sf::st_crs(x) <- "EPSG:4326"
				xTransform <- x
			}
		}


		# default is to use this
		gpmlExt <- "gpml"

		# in case stat
		if(!is.character(x)){
		# write 'x' as a shapefile
			layer<- paste(randomString(length=3), age, sep="_")
			if(verbose) message(paste("Exported data identified as ", layer))
			pathToFileNoEXT <- paste(tempd, "/", layer,sep="")
			if (win) pathToFileNoEXT <- gsub("/","\\\\", pathToFileNoEXT)
			if(verbose) message("Exporting 'x' as a shapefile.")
#			rgdal::writeOGR(xTransform, dsn=paste(pathToFileNoEXT, ".shp", sep=""), layer=layer, driver="ESRI Shapefile")
			sf::st_write(xTransform, dsn=paste(pathToFileNoEXT, ".shp", sep=""),
				layer=layer, driver="ESRI Shapefile", quiet=!verbose)
			# select partitioning feature
			# test whether the partitioning feature collection is there
			if(all(partitioning!=names(plateFeatures)))
				stop(paste0("The partitioning feature collection '", partitioning, "'\n  is not part of the platemodel object."))
			# if there: select the partitioning feature collection
			platePolygons <- plateFeatures[partitioning]

			# and then check if it a shapefile
			if(isShapefile(platePolygons)){
				# if it is, then it must be converted to a gpml
				platePolygons <- shp_to_gpml(platePolygons,  dir=tempd,
					gplatesExecutable=gplatesExecutable, winin=win, winout=win, verbose=verbose)
			}


		}else{
		# feature to reconstruct is the static polygons
			if(length(x)!=1) stop("Only one feature collection can be reconstructed with this method.")
			# look for x in the feature set
			validFeature <- any(x==names(plateFeatures))
			if(validFeature){

				# if it a shapefile, then it needs to be converted to a gpml
				if(isShapefile(plateFeatures[x])){
					# convert here
					plateFeatures[x] <- shp_to_gpml(plateFeatures[x],  dir=tempd,
						gplatesExecutable=gplatesExecutable, winin=win, winout=win, verbose=verbose)
				}

				# which file version is used? .gmpl or .gpmlz?
				withZ <- grepl(".gpmlz", plateFeatures[x])
				if(withZ){
					gpmlExt <- "gpmlz"
				}

				# use original one - even for windows.
				pathToFileNoEXT <- gsub(paste0(".", gpmlExt), "",plateFeatures[x])
				if (win) pathToFileNoEXT <- gsub("/","\\\\", pathToFileNoEXT)
			}else{
				stop("The requested feature collection is not part of the model. ")
			}
		}

		# inheritance of appearance and disappearance dates
		if(plateperiod){
			pPer <- 1
		}else{
			pPer <- 0
		}
		
	# 3. Execute GPlates commands
		# convert to gpml
		if(!is.character(x)){
			if(verbose) message("Converting shapefile to .gpml.")
			conversion <- paste(gplatesExecutable, " convert-file-format -l \"",pathToFileNoEXT,".shp\" -e gpml", sep="")
			system(conversion, ignore.stdout=!verbose,ignore.stderr=!verbose)
		}
		# do the plate assignment
		if(!is.character(x)){
			if(verbose) message("Assigning plate IDs to .gpml file.")
			assignment <- paste(gplatesExecutable, " assign-plate-ids -e ",pPer," -p \"", platePolygons, "\" -l \"",pathToFileNoEXT,".gpml\"", sep="")
			system(assignment, ignore.stdout=!verbose,ignore.stderr=!verbose)
		}
		# do reconstruction
		if(!is.character(x)) if(verbose) message("Reconstructing coordinates.")
		if(is.character(x)) if(validFeature) if(verbose) message(paste0("Reconstructing '",  x, "'."))
			reconstruction <- paste(gplatesExecutable, " reconstruct -l \"",pathToFileNoEXT,".",gpmlExt,"\" -r \"",
					rotation, "\" -e shapefile -t ", age, " -o \"", pathToFileNoEXT,"_reconstructed\" -w 1", sep="") 
			system(reconstruction, ignore.stdout=!verbose,ignore.stderr=!verbose)

	# 4. Processing output
		# Was the output multiple?
		multiple <- FALSE

		# reading coordinates
		if(verbose) message("Reading reconstructed geometries.")

		# the single file
		targetSingle <- paste(pathToFileNoEXT,"_reconstructed.shx",	sep="")
		targetSingleNoPath <- fileFromPath(targetSingle, win=win)

		if(verbose) message(paste0("Reading in: ", targetSingle))


		# produced directory? 
		targetDir<- paste(pathToFileNoEXT,"_reconstructed",	sep="")
		targetDirNoPath <- fileFromPath(targetDir, win=win)

		# is this a single file? 
		allFiles <- list.files(tempd)

		# assume that nothing was calculated. The formatting will be taken care of by the front-end functions
		rotated <- NA
	
		# is the target single file created?
		if(any(allFiles==targetSingleNoPath)){
			if(verbose) message("Found single output geometry files.")
			rotated <- sf::st_read(targetSingle, quiet=!verbose)
		}

		# did GPlates produce a whole directory? 
		if(any(allFiles==targetDirNoPath)){
			# Was an output multiple?
			multiple <- TRUE

			if(verbose) message("Found multiple output geometry files.")
			# read in files from there
			allDirFiles <- list.files(targetDir)	

			# files to read in
			toRead <- allDirFiles[grep(".shx", allDirFiles)]

			# the container
			rotatedList <- NULL
			for(i in 1:length(toRead)){
				# read in one bit
				rotatedList[[i]] <- sf::st_read(file.path(targetDir, toRead[i]), quiet=!verbose)
			}

			# make this just one object
			rotated <- NULL
			# the final set of columns
			allColumns <- unique(unlist(lapply(rotatedList, colnames)))
			allColumns <- c(allColumns[allColumns!="geometry"], "geometry")

			for(i in 1:length(rotatedList)){

				# focus on one
				one <- rotatedList[[i]]

				# if columns are missing, add them
				missingColumns <- allColumns[!allColumns%in%colnames(one)]
				if(length(missingColumns)>0){
					for (j in 1:length(missingColumns)){
						one[[missingColumns[j]]] <- NA
					}
				}
				# reorder the columns
				rotated <- rbind(rotated, one[,allColumns])

			}

		}
		
		# if the originals were sf - nothing happens
		if(inherits(originals, "sf") | inherits(originals, "Spatial") | inherits(x, "character")){
			# omission of gplates metadata? 
			if(!gmeta){
				rotated <- 	rotated[, -which(colnames(rotated)%in%c("ANCHOR", "TIME", "FILE1", "RECONFILE1"))]
			}
		}
		
		# but if there was a crs, make sure to change projection to it!
		# is CRS even applicable? 
		if(!is.null(originalCRS)){
			# if it was given in the first place
			if(!is.na(originalCRS)){
				if(verbose) message("Converting back to original CRS.")
				rotated <- sf::st_transform(rotated, originalCRS)
			}
		}
		
	
		# if originals are spatial, make the output spatial
		if(inherits(originals, "Spatial")){
			if(verbose) message("Converting sf to Spatial.")
			# different treatment dependending on whether multiple files or a single one is outpu
			# sp cannot handle heterogeneous geometries
			if(!multiple){
				rotated <- sf::as_Spatial(rotated)
			}else{
				warning("There were multiple reconstruction output, returning as sf.")
			}
		}

		# if originals are not sf and not spatial
		if(!inherits(originals, "sf") & !inherits(originals, "Spatial")){

			# and they are either a matrix or a data frame transform object back to whatever it was
			if((inherits(originals, "matrix") | inherits(originals, "data.frame")) ){
				# run this, except when missing value is indicated
				if(!all(is.na(rotated))){
					# some coordinates probably were missing
					# get the coorindates
					#  reconstructed
					rotatedSF <- rotated
					coords <- sf::st_coordinates(rotated)

					# where to put the new coordinates
					rotated <- originals
					rotated[] <- NA

					# use the IDs to make sure that things really match!
					rownames(rotated) <- theID
					rotated[rotatedSF$ID, ] <- coords

					# copy over the rownames
					rownames(rotated) <- rownames(originals)
				}

			}else{
				if(!is.null(originals)) stop(paste("Unknown output class", class(originals)))
			}
		}


	# 5. Finish
		# remove temporary files
		if(cleanup){		
			if(!inherits(x, "character")){
				system(paste("rm -r ",tempd, "/",layer,"*", sep=""))
			}
		}
	return(rotated)
}

# function to find Gplates installation directory
winDefaultGPlates<-function(){

	# default installation paths
	basic <- c(
		"C:/Program Files/GPlates",
		"C:/Program Files (x86)/GPlates"
		
	)

	versioned <- NULL
	inWhich <- NULL

	# search both possible folders 
	for(i in 1:length(basic)){
		# enter program files
		gpver <- list.files(basic[i])
	
		found <- grep("GPlates", gpver)
	
		# grab the latest version
		if(length(found)>0){
			versioned <- gpver[found[length(found)]]
			inWhich <- i
		}
	}
	if(is.null(inWhich)) stop("Could not locate GPlates.")

	# add it 
	dir <- file.path(basic[inWhich], versioned)

	# search executable
	gpfiles <- list.files(dir)

	# grab gplates executable file
	gplat <- grep("gplat",gpfiles)
	potExe <- gpfiles[gplat]
	exe <- potExe[grep("exe$",potExe)]

	return(c(dir=dir, exe=exe))
}


macDefaultGPlates <-function(){
# default installation path
	basic <- "/Applications"
	# enter program files
	gpver <- list.files(basic)

	found <- grep("GPlates", gpver)

	# grab the latest version
	if(length(found)>0){
		gpver<- gpver[found[length(found)]]
	}else{
		stop("Could not locate GPlates.")
	}
	dir <-file.path(basic,gpver, "gplates.app/Contents/MacOS")
	exe <-"gplates"
	return(c(dir=dir, exe=exe))
}



testGPlates<- function(gplatesExecutable, verbose){
# ask version
	gplatesTest <- paste(gplatesExecutable, "--v")
	
	# "\"C:/Program Files (x86)/GPlates/GPlates 2.1.0/gplates-2.1.0.exe\" --v"
	# default version
	ver <- NULL
	
	# depending on how much the user wants to see
	if(!verbose){
		opt <- options(show.error.messages = FALSE)
		# revert even if command below fails for some reason
		on.exit(options(opt))

		try(ver <- system(gplatesTest, intern=TRUE,ignore.stdout = TRUE, 
				ignore.stderr = TRUE))
	}else{
		try(ver <- system(gplatesTest, intern=TRUE))
	}
	
	# if gplates is not present
	return(!is.null(ver))
}


# if the model is made up of multiple rotation files, they need to be merged
# will create a new, temporary rotation file in the temp directory
mergeRotations <- function(x){

	# store everything in here
	allLines <- NULL
	for(i in 1:length(x)){
		allLines <- c(allLines, readLines(x[i]))
	}

	# write them into the temporary directory
	tempor <- tempdir()

	# where to write the file
	newFile <- file.path(tempor, "jointRotation.rot")

	# write it
	cat(allLines, file=newFile, sep="\n")

	# return the name of the new rotation file
	return(newFile)

}


isShapefile <- function(x){
	# get filename
	filename <- fileFromPath(x)
	grepl("\\.shx$",filename) | grepl("\\.shp$",filename)
}

shp_to_gpml <- function(x, dir=file.path(tempdir(), "newgpml"), gplatesExecutable, winin=FALSE, winout=FALSE, verbose=FALSE){
	if(verbose){
		message("Transforming shapefile to gpml:")
		message(x)
	}

	# first ensure that the directory exists
	dir.create(dir, showWarnings=FALSE)

	if(winin) x <- gsub("\\\\", "/", x)

	# copy over all relevant files
	filename <- fileFromPath(x)

	# the filename stem
	fileStem <- unlist(lapply(strsplit(filename, "\\."), function(x) paste(x[-length(x)], collapse="")))

	# origdir
	origdir <- unlist(lapply(strsplit(x, "/"), function(x) paste(x[-length(x)], collapse="/")))

	# list out all the files (shapefiles should have the same stem
	allFiles <- list.files(origdir)


	# copy the relevant files
	toCopy <- allFiles[grep(fileStem, allFiles)]


	for(i in 1:length(toCopy)){
		file.copy(file.path(origdir,toCopy[i]),file.path(dir, toCopy[i]))
	}

	# look for the shapefile (shp)
	shpFile <- toCopy[grep("\\.shp$",toCopy)]
	pathToFile <- file.path(dir, shpFile)

	# now check the OS
	if (winout) pathToFile <- gsub("/","\\\\", pathToFile)

	# then use gplates
	conversion <- paste(gplatesExecutable, " convert-file-format -l \"",pathToFile,"\" -e gpml",sep="")

	# execute conversion
	system(conversion, ignore.stdout=!verbose,ignore.stderr=!verbose)

	# return path to gpml file
	result <- paste0(dir, "/", fileStem, ".gpml")

	# ensure homogeneous path output
	if(winout){
		result <- gsub("/","\\\\", result)
	}else{
		result <- gsub("\\\\","/", result)
	}

	return(result)
}
