# Function importSlicerChart: Reads the Slicer Chart printed in XML and then plots it
#
importSlicerChart <- function(xmlSchemaPath, chartPath){

  # Load the XML Library. It should be installed before running this code from:
  # http://cran.r-project.org/web/packages/XML/index.html
  library("XML")

  xsd = xmlParse(xmlSchemaPath, isSchema =TRUE) # Parse the XML Schema
  doc = xmlInternalTreeParse(chartPath) # Parse the document in the tree version

  # Validate the XML file
  validationResult = xmlSchemaValidate(xsd, doc)

  # If XML passed the validation test
  if(validationResult[1] == 0) {

    # Parse the XML Document
    doc = xmlRoot(xmlTreeParse(chartPath))

    # Code to find out the number of properties there are
    chartProperties = doc[[2]]
    numProperties = length(names(chartProperties))

    # Initialize the vectors where the chart properties are stored
    chartPropertyNames <- vector()
    chartPropertyValues <- vector()
    # get all the chart level properties
    for (i in 1:numProperties){
      propertyAttributes = xmlAttrs(doc[[2]][[i]])
      chartPropertyNames = c(chartPropertyNames, propertyAttributes[1])
      chartPropertyValues = c(chartPropertyValues, propertyAttributes[2])
    }

    # Determine the number of arrays
    allArrays = doc[["arrays"]]
    numArrays = length(names(allArrays))

    # Initialize the vectors where the array names are stored
    arrayNames <- vector()
    # Extract the array Names
    for (i in 1:numArrays){
      arrayAttributes = xmlAttrs(doc[["arrays"]][[i]])
      arrayNames = c(arrayNames, arrayAttributes[1])
    }

    # determine the chart Type
    chartTypeIndex = match('type', chartPropertyNames)
    if(is.na(chartTypeIndex)){
      chartType = "Line"
    } else {
      chartType = chartPropertyValues[chartTypeIndex]
    }

    # determine the chart title
    showTitle = match('showTitle', chartPropertyNames)
    chartTitle = match('title', chartPropertyNames)
    title = ""
    if((!is.na(showTitle)) & (chartPropertyValues[showTitle] == "on") & (!is.na(chartTitle))){
      title = chartPropertyValues[chartTitle]
    }

    # determine the chart x-axis label
    showXAxisLabel = match('showXAxisLabel', chartPropertyNames)
    xAxisLabel = match('xAxisLabel', chartPropertyNames)
    xLabel=""
    if((!is.na(showXAxisLabel)) & (chartPropertyValues[showXAxisLabel] == "on") & (!is.na(xAxisLabel))){
      xLabel = chartPropertyValues[xAxisLabel]
    }

    # determine the chart y-axis label
    showYAxisLabel = match('showYAxisLabel', chartPropertyNames)
    yAxisLabel = match('yAxisLabel', chartPropertyNames)
    yLabel=""
    if((!is.na(showYAxisLabel)) & (chartPropertyValues[showYAxisLabel] == "on") & (!is.na(yAxisLabel))){
      yLabel = chartPropertyValues[yAxisLabel]
    }

    # Specify default Color
    defaultColors = c("#ec2c33", "#128b4a", "#165da7", "#f37b2d", "#65328f", "#a11c23", "#b23c93", "#010202")

    plotDataList = list()
    plotColors <- vector()

    # Plot the charts depending on what type of charts they are
    if(chartType == "Box"){
      # Extract the data from the Arrays and format it specifically for boxplots

      for (i in 1:numArrays){

        arrayProperties = doc[["arrays"]][[i]][[1]]
        numArrayProperties = length(names(arrayProperties))

        arrayPropertyNames <- vector()
        arrayPropertyValues <- vector()

        if(numArrayProperties > 0) {
          # get all the array level properties
          for (j in 1:numArrayProperties){
            propertyAttributes = xmlAttrs(doc[["arrays"]][[i]][[1]][[j]])
            arrayPropertyNames = c(arrayPropertyNames, propertyAttributes[1])
            arrayPropertyValues = c(arrayPropertyValues, propertyAttributes[2])
          }
        }


        lookupTableFlag = 0
        lookupTable = match('lookupTable', chartPropertyNames)
        if(!is.na(lookupTable)){
          lookupTableFlag = 1
        }

        if (lookupTableFlag == 1) {
          if (i == 1) {
            # Extract the data for the bar charts and format it to be plotted
            lookupTableData = doc[["arrays"]][[1]][[3]]
            lookupTableVals <- xmlSApply(lookupTableData, function(x) xmlSApply(x, xmlValue))
            lookupTableVals_df <- data.frame(t(lookupTableVals),row.names=NULL)

            # Extract the colors for all the bars from the lookup table
            colorsList = data.frame(lookupTableVals_df[,1])
            maxLength <- max(sapply(colorsList, length))
            ll <- lapply(colorsList, function(X) {c(as.character(X), rep("", times = maxLength - length(X)))})
            out <- do.call(cbind, ll)
            plotColors = apply(out, 2, as.character)
          }
        } else {

          # Find colors for individual boxes or us default color
          color = match('color', arrayPropertyNames)
          if(!is.na(color)){
            plotColors = c(plotColors, arrayPropertyValues[color])
          } else {
            plotColors = c(plotColors, defaultColors[1])
          }
        }
        arrayData = doc[["arrays"]][[i]][[2]]
        arrayVals <- xmlSApply(arrayData, function(x) xmlSApply(x, xmlValue))
        arrayVals_df <- data.frame(t(arrayVals),row.names=NULL)
        plotDataList = c(plotDataList, data.frame(arrayVals_df[,2]))
      }
      maxLength <- max(sapply(plotDataList, length))
      ll <- lapply(plotDataList, function(X) {c(as.character(X), rep("", times = maxLength - length(X)))})
      out <- do.call(cbind, ll)
      colnames(out) <- arrayNames
      plotData = apply(out, 2, as.numeric)


      # Plot box plot
      boxplot(plotData, border = plotColors, xlab=xLabel, ylab=yLabel, main=title)

    } else if(chartType == "Bar"){

      arrayProperties = doc[["arrays"]][[1]][[1]]
      numArrayProperties = length(names(arrayProperties))
      arrayPropertyNames <- vector()
      arrayPropertyValues <- vector()
      if(numArrayProperties > 0) {
        # get all the array level properties
        for (j in 1:numArrayProperties){
          propertyAttributes = xmlAttrs(doc[["arrays"]][[1]][[1]][[j]])
          arrayPropertyNames = c(arrayPropertyNames, propertyAttributes[1])
          arrayPropertyValues = c(arrayPropertyValues, propertyAttributes[2])
        }
      }
      # Check to see if there is a lookup table for the bar color info
      lookupTable = match('lookupTable', arrayPropertyNames)
      lookupTableFlag = 0
      if(!is.na(lookupTable)){
        lookupTableFlag = 1
      } else {
        lookupTable = match('lookupTable', chartPropertyNames)
        if(!is.na(lookupTable)){
          lookupTableFlag = 1
        }
      }
      color = match('color', arrayPropertyNames)
      if(!is.na(color)){
        plotColors = c(plotColors, arrayPropertyValues[color])
      } else {
        plotColors = c(plotColors, defaultColors[1])
      }
      xlabelsList = list()
      # Extract the data for the bar charts and format it to be plotted
      arrayData = doc[["arrays"]][[1]][[2]]
      arrayVals <- xmlSApply(arrayData, function(x) xmlSApply(x, xmlValue))
      arrayVals_df <- data.frame(t(arrayVals),row.names=NULL)

      plotDataList = c(plotDataList, data.frame(arrayVals_df[,2]))
      maxLength <- max(sapply(plotDataList, length))
      ll <- lapply(plotDataList, function(X) {c(as.character(X), rep("", times = maxLength - length(X)))})
      out <- do.call(cbind, ll)
      plotData = apply(out, 2, as.numeric)

      # Extract the labels for all the bars
      xlabelsList = c(xlabelsList, data.frame(arrayVals_df[,1]))
      maxLength <- max(sapply(xlabelsList, length))
      ll <- lapply(xlabelsList, function(X) {c(as.character(X), rep("", times = maxLength - length(X)))})
      out <- do.call(cbind, ll)

      xAxisType = match('xAxisType', chartPropertyNames)
      if(!is.na(xAxisType) && chartPropertyValues[xAxisType] != "quantitative"){
        labels = apply(out, 2, as.character)
      } else {
        labels = apply(out, 2, as.numeric)
      }

      if (lookupTableFlag == 1) {
        # Extract the data for the bar charts and format it to be plotted
        lookupTableData = doc[["arrays"]][[1]][[3]]
        lookupTableVals <- xmlSApply(lookupTableData, function(x) xmlSApply(x, xmlValue))
        lookupTableVals_df <- data.frame(t(lookupTableVals),row.names=NULL)

        # Extract the colors for all the bars from the lookup table
        colorsList = data.frame(lookupTableVals_df[,1])
        maxLength <- max(sapply(colorsList, length))
        ll <- lapply(colorsList, function(X) {c(as.character(X), rep("", times = maxLength - length(X)))})
        out <- do.call(cbind, ll)
        barColors = apply(out, 2, as.character)

        # Extract the labels for all the bars
        xlabelsList = data.frame(lookupTableVals_df[,3])
        maxLength <- max(sapply(xlabelsList , length))
        ll <- lapply(xlabelsList , function(X) {c(as.character(X), rep("", times = maxLength - length(X)))})
        out <- do.call(cbind, ll)
        labels = apply(out, 2, as.character)
      } else {
        barColors = plotColors
      }

      # Plot the bar chart
      barplot(c(plotData), names.arg=labels, col=barColors, xlab=xLabel, ylab=yLabel, main=title)

    } else if(chartType == "Scatter"){

      ## Determine pre-existing global defaults from the chart (could be overwritten by the array properties)
      lineFlag = 0
      showLines = match('showLines', chartPropertyNames)
      if(!is.na(showLines) && chartPropertyValues[showLines] == "on"){
        lineFlag = 1
      }

      showMarkersFlag = 1
      showMarkers = match('showMarkers', chartPropertyNames)
      if( (!is.na(showMarkers)) & (chartPropertyValues[showMarkers] == "off") ){
        showMarkersFlag = 0
      }

      linePatternFlag = -1
      linePattern = match('linePattern', chartPropertyNames)
      if(!is.na(linePattern)){
        linePatternFlag = 1
        linePatternValue = chartPropertyValues[linePattern]
      }

      # Get array data
      xAxisVals = list()
      for (i in 1:numArrays){

        arrayData = doc[["arrays"]][[i]][[2]]
        arrayVals <- xmlSApply(arrayData, function(x) xmlSApply(x, xmlValue))
        arrayVals_df <- data.frame(t(arrayVals),row.names=NULL)
        plotDataList = c(plotDataList, data.frame(arrayVals_df[,2]))
        xAxisVals = c(xAxisVals, data.frame(arrayVals_df[,1]))
      }

      # format y data
      maxLength <- max(sapply(plotDataList, length))
      ll <- lapply(plotDataList, function(X) {c(as.character(X), rep("", times = maxLength - length(X)))})
      out <- do.call(cbind, ll)
      colnames(out) <- arrayNames
      plotData = apply(out, 2, as.numeric)

      # format x data
      maxLength <- max(sapply(xAxisVals, length))
      ll <- lapply(xAxisVals, function(X) {c(as.character(X), rep("", times = maxLength - length(X)))})
      out <- do.call(cbind, ll)
      colnames(out) <- arrayNames
      xVals = apply(out, 2, as.numeric)

      # determine range
      xRange = range(xVals)
      yRange = range(plotData)
      xAxisPad = match('xAxisPad', chartPropertyNames)
      if(!is.na(xAxisPad)){
        ammountPadding = as.numeric(chartPropertyValues[xAxisPad]) * abs(xRange[2] - xRange[1])
        xRange[1] = floor(xRange[1] - (ammountPadding/2))
        xRange[2] = ceiling(xRange[2] + (ammountPadding/2))
      }
      yAxisPad = match('yAxisPad', chartPropertyNames)
      if(!is.na(yAxisPad)){
        ammountPadding = as.numeric(chartPropertyValues[yAxisPad]) * abs(yRange[2] - yRange[1])
        yRange[1] = floor(yRange[1] - (ammountPadding/2))
        yRange[2] = ceiling(yRange[2] + (ammountPadding/2))
      }

      lineColorCounter = 1;
      for (i in 1:numArrays){

        # get Array properties
        arrayProperties = doc[["arrays"]][[i]][[1]]
        numArrayProperties = length(names(arrayProperties))

        arrayPropertyNames <- vector()
        arrayPropertyValues <- vector()

        if(numArrayProperties > 0) {
          # get all the array level properties
          for (j in 1:numArrayProperties){
            propertyAttributes = xmlAttrs(doc[["arrays"]][[i]][[1]][[j]])
            arrayPropertyNames = c(arrayPropertyNames, propertyAttributes[1])
            arrayPropertyValues = c(arrayPropertyValues, propertyAttributes[2])
          }
        }

        lookupTableFlag = 0
        lookupTable = match('lookupTable', chartPropertyNames);
        if(!is.na(lookupTable)){
          lookupTableFlag = 1
        }

        if (lookupTableFlag == 1) {
          if (i == 1) {
            # Extract the data for the bar charts and format it to be plotted
            lookupTableData = doc[["arrays"]][[1]][[3]]
            lookupTableVals <- xmlSApply(lookupTableData, function(x) xmlSApply(x, xmlValue))
            lookupTableVals_df <- data.frame(t(lookupTableVals),row.names=NULL)

            # Extract the colors for all the bars from the lookup table
            colorsList = data.frame(lookupTableVals_df[,1])
            maxLength <- max(sapply(colorsList, length))
            ll <- lapply(colorsList, function(X) {c(as.character(X), rep("", times = maxLength - length(X)))})
            out <- do.call(cbind, ll)
            plotColors = apply(out, 2, as.character)
          }
        } else {
          # Determine Line Color
          color = match('color', arrayPropertyNames)
          if(!is.na(color)){
            plotColors = c(plotColors, arrayPropertyValues[color])
          } else{
            plotColors = c(plotColors, defaultColors[lineColorCounter])
            if(lineColorCounter == 8) {
              lineColorCounter = 1
            } else {
              lineColorCounter = lineColorCounter + 1
            }
          }
        }

        # determine whether or not to show the lines for this array
        lineArrayFlag = 0
        showLines = match('showLines', arrayPropertyNames)
        if(!is.na(showLines) ){
          if (arrayPropertyValues[showLines] == "on") {
            lineArrayFlag = 1
          }
        } else {
          if(lineFlag == 1 ){
            lineArrayFlag = 1
          }
        }

        # determine the line Pattern
        linePatternArrayFlag = 0
        linePattern = match('linePattern', arrayPropertyNames)
        if(!is.na(linePattern)){
          linePatternArrayFlag = 1
          linePatternArrayValue = arrayPropertyValues[linePattern]
        } else if(linePatternFlag == 1) {
          linePatternArrayFlag = 1
          linePatternArrayValue = linePatternValue;
        }

        lty = 0 # Default no line is shown
        if( lineArrayFlag == 1 ){
          lty = 1
          if(linePatternArrayFlag == 1 ){
            if(linePatternArrayValue == "dashed"){
              lty = 2
            } else if(linePatternArrayValue == "dotted"){
              lty = 3
            } else if(linePatternArrayValue == "dashed-dotted"){
              lty = 4
            } else{
              lty = 1
            }
          }
        }

        # determine whether or not to show markers with the lines
        showMarkersArrayFlag = 1
        # Check Array level properties for showMarkers if chart level properties did not have it
        showMarkers = match('showMarkers', arrayPropertyNames)
        if(!is.na(showMarkers)){
          if (arrayPropertyValues[showMarkers] == "off"){
            showMarkersArrayFlag = 0
          }
        } else {
          if(showMarkersFlag == 0) {
            showMarkersArrayFlag = 0
          }
        }

        # Generate the type of plot
        if( (lineArrayFlag == 1)  & (showMarkersArrayFlag == 1) ) {
          linePlotType = "b"
        } else if( (lineArrayFlag == 1)  &  (showMarkersArrayFlag == 0) ) {
          linePlotType = "l"
        } else if( (lineArrayFlag == 0)  &  (showMarkersArrayFlag == 1) ) {
          linePlotType = "p"
        } else {
          linePlotType = "n"
        }

        if(i==1){
          plot(xVals[,i], plotData[,i], type=linePlotType, ylim=yRange, xlim=xRange, col=plotColors[i], lty=lty, pch=1, xlab=xLabel, ylab=yLabel, main=title)
          par(new=TRUE)
        } else {
          plot(xVals[,i], plotData[,i], type=linePlotType, ylim=yRange, xlim=xRange, col=plotColors[i], lty=lty, pch=1, xlab="", ylab="")
          par(new=TRUE)
        }

      }

    } else { # Line Plot

      ## Determine pre-existing global defaults from the chart (could be overwritten by the array properties)
      lineFlag = 1
      showLines = match('showLines', chartPropertyNames)
      if(!is.na(showLines) && chartPropertyValues[showLines] == "off"){
        lineFlag = 0
      }

      showMarkersFlag = -1
      showMarkers = match('showMarkers', chartPropertyNames)
      if( (!is.na(showMarkers)) & (chartPropertyValues[showMarkers] == "on") ){
        showMarkersFlag = 1
      } else if( (!is.na(showMarkers)) & (chartPropertyValues[showMarkers] == "off") ){
        showMarkersFlag = 0
      }

      linePatternFlag = -1
      linePattern = match('linePattern', chartPropertyNames)
      if(!is.na(linePattern)){
        linePatternFlag = 1
        linePatternValue = chartPropertyValues[linePattern]
      }

      # Get data from Array
      xAxisVals = list()
      for (i in 1:numArrays){

        arrayData = doc[["arrays"]][[i]][[2]]
        arrayVals <- xmlSApply(arrayData, function(x) xmlSApply(x, xmlValue))
        arrayVals_df <- data.frame(t(arrayVals),row.names=NULL)
        plotDataList = c(plotDataList, data.frame(arrayVals_df[,2]))
        xAxisVals = c(xAxisVals, data.frame(arrayVals_df[,1]))
      }

      # format y data
      maxLength <- max(sapply(plotDataList, length))
      ll <- lapply(plotDataList, function(X) {c(as.character(X), rep("", times = maxLength - length(X)))})
      out <- do.call(cbind, ll)
      colnames(out) <- arrayNames
      plotData = apply(out, 2, as.numeric)

      # format x data
      maxLength <- max(sapply(xAxisVals, length))
      ll <- lapply(xAxisVals, function(X) {c(as.character(X), rep("", times = maxLength - length(X)))})
      out <- do.call(cbind, ll)
      colnames(out) <- arrayNames
      xVals = apply(out, 2, as.numeric)

      # determine range
      xRange = range(xVals)
      yRange = range(plotData)
      xAxisPad = match('xAxisPad', chartPropertyNames)
      if(!is.na(xAxisPad)){
        ammountPadding = as.numeric(chartPropertyValues[xAxisPad]) * abs(xRange[2] - xRange[1])
        xRange[1] = floor(xRange[1] - (ammountPadding/2))
        xRange[2] = ceiling(xRange[2] + (ammountPadding/2))
      }
      yAxisPad = match('yAxisPad', chartPropertyNames)
      if(!is.na(yAxisPad)){
        ammountPadding = as.numeric(chartPropertyValues[yAxisPad]) * abs(yRange[2] - yRange[1])
        yRange[1] = floor(yRange[1] - (ammountPadding/2))
        yRange[2] = ceiling(yRange[2] + (ammountPadding/2))
      }

      lineColorCounter = 1

      for (i in 1:numArrays){
        arrayProperties = doc[["arrays"]][[i]][[1]]
        numArrayProperties = length(names(arrayProperties))

        arrayPropertyNames <- vector()
        arrayPropertyValues <- vector()

        if(numArrayProperties > 0) {
          # get all the array level properties
          for (j in 1:numArrayProperties){
            propertyAttributes = xmlAttrs(doc[["arrays"]][[i]][[1]][[j]])
            arrayPropertyNames = c(arrayPropertyNames, propertyAttributes[1])
            arrayPropertyValues = c(arrayPropertyValues, propertyAttributes[2])
          }
        }

        lookupTableFlag = 0
        lookupTable = match('lookupTable', chartPropertyNames)
        if(!is.na(lookupTable)){
          lookupTableFlag = 1
        }

        if (lookupTableFlag == 1) {
          if (i == 1) {
            # Extract the data for the bar charts and format it to be plotted
            lookupTableData = doc[["arrays"]][[1]][[3]]
            lookupTableVals <- xmlSApply(lookupTableData, function(x) xmlSApply(x, xmlValue))
            lookupTableVals_df <- data.frame(t(lookupTableVals),row.names=NULL)

            # Extract the colors for all the bars from the lookup table
            colorsList = data.frame(lookupTableVals_df[,1])
            maxLength <- max(sapply(colorsList, length))
            ll <- lapply(colorsList, function(X) {c(as.character(X), rep("", times = maxLength - length(X)))})
            out <- do.call(cbind, ll)
            plotColors = apply(out, 2, as.character)
          }
        } else {
          # Determine Line Color
          color = match('color', arrayPropertyNames);
          if(!is.na(color)){
            plotColors = c(plotColors, arrayPropertyValues[color])
          } else{
            plotColors = c(plotColors, defaultColors[lineColorCounter])
            if(lineColorCounter == 8) {
              lineColorCounter = 1
            } else {
              lineColorCounter = lineColorCounter + 1
            }
          }
        }

        # determine whether or not to show the lines for this array
        lineArrayFlag = 1
        showLines = match('showLines', arrayPropertyNames)
        if(!is.na(showLines) ){
          if (arrayPropertyValues[showLines] == "off") {
            lineArrayFlag = 0
          }
        } else {
          if(lineFlag == 0 ){
            lineArrayFlag = 0
          }
        }

        # determine the line Pattern
        linePatternArrayFlag = 0
        linePattern = match('linePattern', arrayPropertyNames)
        if(!is.na(linePattern)){
          linePatternArrayFlag = 1
          linePatternArrayValue = arrayPropertyValues[linePattern]
        } else if(linePatternFlag == 1) {
          linePatternArrayFlag = 1
          linePatternArrayValue = linePatternValue
        }

        lty = 0 # Default no line is shown
        if( lineArrayFlag == 1 ){
          lty = 1
          if(linePatternArrayFlag == 1 ){
            if(linePatternArrayValue == "dashed"){
              lty = 2
            } else if(linePatternArrayValue == "dotted"){
              lty = 3
            } else if(linePatternArrayValue == "dashed-dotted"){
              lty = 4
            } else{
              lty = 1
            }
          }
        }

        # determine whether or not to show markers with the lines
        showMarkersArrayFlag = 0
        # Check Array level properties for showMarkers if chart level properties did not have it
        showMarkers = match('showMarkers', arrayPropertyNames)
        if(!is.na(showMarkers)){
          if (arrayPropertyValues[showMarkers] == "on"){
            showMarkersArrayFlag = 1
          }
        } else {
          if(showMarkersFlag == 1) {
            showMarkersArrayFlag = 1
          }
        }

        # Generate the type of plot
        if( (lineArrayFlag == 1)  & (showMarkersArrayFlag == 1) ) {
          linePlotType = "b"
        } else if( (lineArrayFlag == 1)  &  (showMarkersArrayFlag == 0) ) {
          linePlotType = "l"
        } else if( (lineArrayFlag == 0)  &  (showMarkersArrayFlag == 1) ) {
          linePlotType = "p"
        } else {
          linePlotType = "n"
        }

        # Plot the Values
        if(i==1){
          plot(xVals[,i], plotData[,i], type=linePlotType, ylim=yRange, xlim=xRange, col=plotColors[i], lty=lty, pch=1, xlab=xLabel, ylab=yLabel, main=title)
          par(new=TRUE)
        } else {
          plot(xVals[,i], plotData[,i], type=linePlotType, ylim=yRange, xlim=xRange, col=plotColors[i], lty=lty, pch=1, xlab="", ylab="")
          par(new=TRUE)
        }

      }

    }

    # Add Grid
    showGrid = match('showGrid', chartPropertyNames);
    if(!is.na(showGrid) & (chartPropertyValues[showGrid] == "on")){
      grid()
    }

    # Show Legend if the showLegend Property is on
    showLegend = match('showLegend', chartPropertyNames);
    if(!is.na(showLegend) & (chartPropertyValues[showLegend] == "on")){
      legend("topright", arrayNames, fill = plotColors)
    }


  } else {
    print("XML Validation Failed")
    print(validationResult[2])
  }

}
