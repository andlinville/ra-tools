NewProject <- function(name, hospitalCase) {
  #
  # Creates a new project directory in the working directory.
  #
  # Args:
  #   name: The name of the project. This will also be the name of the
  #   directory.
  #   hospitalCase: A boolean value: true if the project is a hospital case,
  #   false if it is not a hospital case.
  #
  # Returns:
  #   Does not return a value. Creates the directory structure in the current
  #   working directory as a side effect. If hospitalCase is set to true, some
  #   of the standard keys used for a hospital case will be included in the
  #   appropriate folders.
  
  dir.create(name)
  dir.create(paste(name, "raw", sep="/"))
  dir.create(paste(name, "vb", sep="/"))
  dir.create(paste(name, "documentation", sep="/"))
}

NewSubProject <- function(subName) {
  #
  # Creates a new subproject directory in the working directory.
  #
  # Args:
  #   subName: the name of the subproject. This will also be the name of the
  #   directory
  #
  # Returns:
  #   Does not return a value. Creates the directory structure in the current
  #   working directory as a side effect.
  
  dir.create(subName)
  dir.create(paste(subName, "scripts", sep="/"))
  dir.create(paste(subName, "input", sep="/"))
  dir.create(paste(subName, "output", sep="/"))
  dir.create(paste(subName, "temp", sep="/"))
  dir.create(paste(subName, "data", sep="/"))
}