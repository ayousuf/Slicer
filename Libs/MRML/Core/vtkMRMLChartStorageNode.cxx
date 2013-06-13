/*=auto=========================================================================

  Portions (c) Copyright 2005 Brigham and Women's Hospital (BWH) All Rights Reserved.

  See COPYRIGHT.txt
  or http://www.slicer.org/copyright/copyright.txt for details.

  Program:   3D Slicer
  Module:    $RCSfile: vtkMRMLChartStorageNode.cxx,v $
  Date:      $Date: 2006/03/19 17:12:29 $
  Version:   $Revision: 1.18 $

=========================================================================auto=*/


#include <sstream>
#include <map>
#include <string>

#include "vtkObjectFactory.h"

#include <vtkStringArray.h>
#include <vtkStdString.h>

#include "vtkMRMLChartStorageNode.h"
#include "vtkMRMLChartNode.h"
#include "vtkMRMLDoubleArrayNode.h"
#include "vtkMRMLScene.h"



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

  of << "<xml>\n";
  of << "<chart>\n";
  
  of << indent << "<name> " << aChartNode->GetName() << " </name>\n";
  
  
  // Print the chart Level Properties
  vtkStringArray* chartProperties = vtkStringArray::New();
  vtkStringArray* chartValues = vtkStringArray::New();

  aChartNode->GetProperties("default", chartProperties, chartValues);
  if(chartProperties->GetNumberOfValues() > 0)
    {
    of << indent << "<properties>\n";
    for (int i = 0; i < chartProperties->GetNumberOfValues(); i++)
      {
        of << indent << indent << "<property name = \"" << chartProperties->GetValue(i).c_str() << "\" value = \"" << chartValues->GetValue(i).c_str() << "\"/>\n";
      }
    of << indent << "</properties>\n";
    }

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
    if(arrayProperties->GetNumberOfValues() > 0)
      {
      of << indent << indent << indent << "<properties>\n";
      for (int i = 0; i < arrayProperties->GetNumberOfValues(); i++)
        {
          of << indent << indent << indent << indent << "<property name = \"" << arrayProperties->GetValue(i).c_str() << "\" value = \"" << arrayValues->GetValue(i).c_str() << "\"/>\n";
        }
      of << indent << indent << indent << "</properties>\n";
      }
    of << indent << indent << indent << "<data>\n";

    // Print the tuples
    int arraySize = currentArray->GetSize();
    for (int k = 0; k < arraySize; k++)
      {
      double x;
      double y;
      double yerr;
      
      int valuesFound = currentArray->GetXYValue(k, &x, &y, &yerr);
      if(valuesFound == 1)
        {
        of << indent << indent << indent << indent << "<row>" << x << ", " << y << ", " << yerr << "</row>\n";
        }
      
      }
      
    of << indent << indent << indent << "</data>\n";
    of << indent << indent << "</array>\n";

    arrayProperties->Delete();
    arrayValues->Delete();

    }


  of << indent << "</arrays>\n";
  of << "</chart>\n";
  of << "</xml>\n";

  of.close();

  chartProperties->Delete();
  chartValues->Delete();
  

}