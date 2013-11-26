/*=auto=========================================================================

  Portions (c) Copyright 2005 Brigham and Women's Hospital (BWH) All Rights Reserved.

  See COPYRIGHT.txt
  or http://www.slicer.org/copyright/copyright.txt for details.

  Program:   3D Slicer
  Module:    $RCSfile: vtkMRMLChartStorageNode.h,v $
  Date:      $Date: 2013/11/04 17:12:29 $
  Version:   $Revision: 1.0 $

=========================================================================auto=*/

#ifndef __vtkMRMLChartStorageNode_h
#define __vtkMRMLChartStorageNode_h

#include "vtkMRMLNode.h"
#include "vtkMRMLStorableNode.h"

class vtkDataObject;
class vtkStringArray;
class DoubleArrayIDMap;
class ChartPropertyMap;
class vtkMRMLChartNode;

#include <string>

/// brief MRML node for referencing a collection of data to plot.
class VTK_MRML_EXPORT vtkMRMLChartStorageNode : public vtkMRMLStorableNode
{
 public:
  //----------------------------------------------------------------
  /// Constants
  //----------------------------------------------------------------



  //----------------------------------------------------------------
  /// Standard methods for MRML nodes
  //----------------------------------------------------------------

  static vtkMRMLChartStorageNode *New();
  vtkTypeMacro(vtkMRMLChartStorageNode,vtkMRMLNode);

  virtual vtkMRMLNode* CreateNodeInstance();

  /// 
  /// Get node XML tag name (like Volume, Model)
  virtual const char* GetNodeTagName()
    {return "ChartStorage";};

  
  //----------------------------------------------------------------
  /// Access methods
  //----------------------------------------------------------------

  ///
  /// Method to set the file name for where the chart will be exported in XML format
  void SetFileName(const char *fileName);

  ///
  /// Method to print out the chart to an xml file
  void WriteChartToXML(vtkMRMLNode *aNode);
  
 protected:
  //----------------------------------------------------------------
  /// Constructor and destroctor
  //----------------------------------------------------------------
  vtkMRMLChartStorageNode();
  ~vtkMRMLChartStorageNode();
  vtkMRMLChartStorageNode(const vtkMRMLChartStorageNode&);
  void operator=(const vtkMRMLChartStorageNode&);

 protected:
  //----------------------------------------------------------------
  /// Data
  //----------------------------------------------------------------
  const char *writeFileName;

};



#endif

