/*=auto=========================================================================

  Portions (c) Copyright 2005 Brigham and Women's Hospital (BWH) All Rights Reserved.

  See COPYRIGHT.txt
  or http://www.slicer.org/copyright/copyright.txt for details.

  Program:   3D Slicer
  Module:    $RCSfile: vtkMRMLChartStorageNode.cxx,v $
  Date:      $Date: 2013/11/04 17:12:29 $
  Version:   $Revision: 1.0 $

=========================================================================auto=*/


#include <sstream>
#include <map>
#include <string>
#include <time.h>
#include <stdio.h>

#include "vtkObjectFactory.h"

#include <vtkStringArray.h>
#include <vtkDoubleArray.h>
#include <vtkStdString.h>

#include "vtkMRMLChartStorageNode.h"
#include "vtkMRMLChartNode.h"
#include "vtkMRMLDoubleArrayNode.h"
#include "vtkMRMLScene.h"
#include "vtkMRMLColorTableNode.h"



//----------------------------------------------------------------------------
vtkMRMLNodeNewMacro(vtkMRMLChartStorageNode);


//----------------------------------------------------------------------------
vtkMRMLChartStorageNode::vtkMRMLChartStorageNode()
{

}


//----------------------------------------------------------------------------
vtkMRMLChartStorageNode::~vtkMRMLChartStorageNode()
{

}

void vtkMRMLChartStorageNode::SetFileName(const char *fileName)
{
  this->writeFileName = fileName;
}

//----------------------------------------------------------------------------
void vtkMRMLChartStorageNode::WriteChartToXML(vtkMRMLNode *aNode)
{
  vtkMRMLChartNode *aChartNode = vtkMRMLChartNode::SafeDownCast(aNode);

  ofstream of;
  of.open(this->writeFileName);
  char indent[] = "  ";

  // Print the XML header
  of << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
  of << "<chart>\n";
  
  of << indent << "<name> " << aChartNode->GetName() << " </name>\n";
  
  // Print the chart Level Properties
  vtkStringArray* chartProperties = vtkStringArray::New();
  vtkStringArray* chartValues = vtkStringArray::New();

  int dateAxis = 0;
  int lookupTableFlag = 0;
  const char* lookupTableValue;
  int barTypeFlag = 0;

  aChartNode->GetProperties("default", chartProperties, chartValues);
  of << indent << "<properties>\n";
  if(chartProperties->GetNumberOfValues() > 0)
    {
    for (int i = 0; i < chartProperties->GetNumberOfValues(); i++)
      {
      of << indent << indent << "<property name = \"" << chartProperties->GetValue(i).c_str() << "\" value = \"" << chartValues->GetValue(i).c_str() << "\"/>\n";
      // Check to see if is it an axis type property
      if((strcmp(chartProperties->GetValue(i).c_str(),"xAxisType") == 0) && (strcmp(chartValues->GetValue(i).c_str(),"date") == 0))
        {
        dateAxis = 1;
        }
      // Check to see if lookup table is there
      if(strcmp(chartProperties->GetValue(i).c_str(),"lookupTable") == 0)
        {
        lookupTableFlag = 1;
        lookupTableValue = chartValues->GetValue(i).c_str();
        }
      //Check to see if it is a bar chart for getting the correct lookup table values if there is one
      if((strcmp(chartProperties->GetValue(i).c_str(),"type") == 0) && (strcmp(chartValues->GetValue(i).c_str(),"Bar") == 0))
        {
        barTypeFlag = 1;
        }
      // [TODO]: If there are default color values, print the following line to XML
      // of << indent << indent << "<property name = \"lookupTable\" value = \"default\"/>\n";
      }
    }

  of << indent << "</properties>\n";
  of << indent << "<arrays>\n";
  
  // Get Array Names and IDs
  vtkStringArray* arrayIDs = aChartNode->GetArrays();
  vtkStringArray* arrayNames = aChartNode->GetArrayNames();
  
  // Print information for each individual Array
  for (int i = 0; i< arrayIDs->GetNumberOfValues(); i++)
    {
    vtkStdString currentArrayID = arrayIDs->GetValue(i);
    
    // Get the Array from the scene by the ID
    vtkMRMLDoubleArrayNode *currentArray = vtkMRMLDoubleArrayNode::SafeDownCast(aChartNode->GetScene()->GetNodeByID(currentArrayID.c_str()));
    of << indent << indent << "<array name = \"" << arrayNames->GetValue(i).c_str() << "\">\n";
    
    
    // Print Array Level Properties
    vtkStringArray* arrayProperties = vtkStringArray::New();
    vtkStringArray* arrayValues = vtkStringArray::New();
    aChartNode->GetProperties(arrayNames->GetValue(i).c_str(), arrayProperties, arrayValues);
    int lookupTableArrayFlag = 0;
    const char* lookupTableArrayValue;
    of << indent << indent << indent << "<properties>\n";
    if(arrayProperties->GetNumberOfValues() > 0)
      {
      for (int i = 0; i < arrayProperties->GetNumberOfValues(); i++)
        {
        of << indent << indent << indent << indent << "<property name = \"" << arrayProperties->GetValue(i).c_str() << "\" value = \"" << arrayValues->GetValue(i).c_str() << "\"/>\n";
        if(strcmp(arrayProperties->GetValue(i).c_str(),"lookupTable") == 0)
          {
          lookupTableArrayFlag = 1;
          lookupTableArrayValue = arrayValues->GetValue(i).c_str();
          }
        }
      }

    of << indent << indent << indent << "</properties>\n";
    of << indent << indent << indent << "<data>\n";

    // Print the tuples
    int arraySize = currentArray->GetSize();
    vtkDoubleArray* colorIndices = vtkDoubleArray::New();
    for (int k = 0; k < arraySize; k++)
      {
      double x;
      double y;
      double yerr;
      
      int valuesFound = currentArray->GetXYValue(k, &x, &y, &yerr);
      if(valuesFound == 1)
        {
          if(barTypeFlag == 1)
            {
            colorIndices->InsertNextValue(x);
            }
          else
            {
            colorIndices->InsertNextValue(k);
            }

        of << indent << indent << indent << indent << "<row>\n";
        // Format and print the dates if the x-axis is type of date
        if(dateAxis == 1)
          {
          time_t rawtime = (time_t)x;
          struct tm * timeinfo;
          char buffer [80];
          timeinfo = localtime(&rawtime);
          strftime (buffer,80,"%Y-%m-%d",timeinfo);
          of << indent << indent << indent << indent << indent <<"<column1>" << buffer << "</column1>\n";
          }
        else
          {
          of << indent << indent << indent << indent << indent <<"<column1>" << x << "</column1>\n";
          }
        of << indent << indent << indent << indent << indent <<"<column2>" << y << "</column2>\n";
        of << indent << indent << indent << indent << indent <<"<column3>" << yerr << "</column3>\n";
        of << indent << indent << indent << indent << "</row>\n";
        }
      
      }
      
    of << indent << indent << indent << "</data>\n";

    // Print the lookuptable if there is one. For lines, scatter, and boxplots, it will only be printed with array one
    if(((lookupTableArrayFlag == 1) || (lookupTableFlag == 1)) && (i == 0))
      {
        /*****/
      const char* lookupTableName;
      if(lookupTableArrayFlag == 1)
        {
        of << indent << indent << indent << "<lookuptable name = \"" << lookupTableArrayValue << "\">\n";
        lookupTableName = lookupTableArrayValue;
        }
      else
        {
        of << indent << indent << indent << "<lookuptable name = \"" << lookupTableValue << "\">\n";
        lookupTableName = lookupTableArrayValue;
        }

      for (int cValsIdx = 0; cValsIdx < colorIndices->GetNumberOfTuples(); cValsIdx++)
        {

        vtkMRMLColorTableNode *currentColorArray = vtkMRMLColorTableNode::SafeDownCast(aChartNode->GetScene()->GetNodeByID(lookupTableArrayValue));
        const char* colorname = currentColorArray->GetColorName((int)colorIndices->GetValue(cValsIdx));
        double* colorVals = new double[4];
        currentColorArray->GetColor((int)colorIndices->GetValue(cValsIdx), colorVals);

        // Convert the color values from rgb to hex
        char hexColorVal[8];
        _snprintf(hexColorVal, sizeof(hexColorVal), "#%02x%02x%02x", int(colorVals[0]*255), int(colorVals[1]*255), int(colorVals[2]*255));

        // Print the lookup table info
        of << indent << indent << indent << indent << "<set>\n";
        of << indent << indent << indent << indent << indent << "<color> " << hexColorVal  << " </color>\n";
        of << indent << indent << indent << indent << indent << "<value> " << colorIndices->GetValue(cValsIdx)  << " </value>\n";
        of << indent << indent << indent << indent << indent << "<label> " << colorname << " </label>\n";
        of << indent << indent << indent << indent << "</set>\n";

        delete[] colorVals;
        }
      /*****/

      // [TODO]: If there are default color values, replace the code above surrounded by /*****/ markers to something like this:
      /*of << indent << indent << indent << "<lookuptable name = \"default\">\n";
      for (int cValsIdx = 0; cValsIdx < [INSERT NUMBER OF DEFAULT COLOR VALUES]; cValsIdx++)
        {
        // Print the lookup table info
        of << indent << indent << indent << indent << "<set>\n";
        of << indent << indent << indent << indent << indent << "<color> " << [PRINT COLOR VALUE IN HEX FORMAT]  << " </color>\n";
        of << indent << indent << indent << indent << indent << "<value> " << [PRINT COLOR INDEX] << " </value>\n";
        of << indent << indent << indent << indent << indent << "<label> " << [PRINT COLOR NAME]<< " </label>\n";
        of << indent << indent << indent << indent << "</set>\n";
        }*/

      of << indent << indent << indent << "</lookuptable>\n";
      }

    // delete the indices of lookup table values that should be extracted
    colorIndices->Delete();


    of << indent << indent << "</array>\n";

    // Delete properties for the current array
    arrayProperties->Delete();
    arrayValues->Delete();

    }

  of << indent << "</arrays>\n";
  of << "</chart>\n";

  of.close();

  // Delete chart properties
  chartProperties->Delete();
  chartValues->Delete();


}