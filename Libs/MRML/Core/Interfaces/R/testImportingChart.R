# Specify the full XML Schema path
xmlSchemaPath = "C:\\NAMIC\\SlicerSchema.xsd"

# Specify the full path to the XML file where the chart data was exported from slicer
chartPath = "C:\\NAMIC\\xmlSchemaTest_Chart_5.xml"

# Import the chart, validate it with schema, and then plot it.
importSlicerChart(xmlSchemaPath, chartPath)