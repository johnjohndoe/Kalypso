/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ui.editor.gmleditor.ui;

import org.eclipse.jface.viewers.ViewerDropAdapter;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.TransferData;
import org.eclipse.ui.views.navigator.LocalSelectionTransfer;
import org.kalypso.ogc.gml.selection.CommandableFeatureSelection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;

/**
 * 
 * TODO: insert type comment here
 * 
 * @author kuepfer
 */
public class GmlTreeDropAdapter extends ViewerDropAdapter
{

  private GmlTreeView m_viewer;

  /**
   *  
   */

  public GmlTreeDropAdapter( GmlTreeView viewer )
  {
    super( viewer.getTreeViewer() );
    m_viewer = viewer;
  }

  /**
   * @see org.eclipse.jface.viewers.ViewerDropAdapter#performDrop(java.lang.Object)
   */
  public boolean performDrop( Object data )
  {
    System.out.print( "performDrop - " );

    Object currentTargetObject = getCurrentTarget();
    Object selectedSourceObject = getSelectedObject();

    int currentOperation = getCurrentOperation();
    if( currentOperation == DND.DROP_COPY )
    {
      
    }
        return true;
  }

  /**
   * @see org.eclipse.jface.viewers.ViewerDropAdapter#validateDrop(java.lang.Object, int,
   *      org.eclipse.swt.dnd.TransferData)
   */
  public boolean validateDrop( Object target, int operation, TransferData transferType )
  {
    CommandableFeatureSelection structuredSelection = (CommandableFeatureSelection)m_viewer.getSelection();
    Feature[] selectedFeatures = structuredSelection.getSelectedFeatures();
    Feature selectedFeature = null;
    Feature sourceFeature = null;
    Feature targetFeature = null;
    FeatureType[] targetFt = null;
    FeatureType[] linkedTargetFt = null;
    FeatureType matchingFt = null;
    if( target instanceof FeatureAssociationTypeElement )
    {
      FeatureAssociationTypeElement targetFatElement = (FeatureAssociationTypeElement)target;
      FeatureAssociationTypeProperty associationTypeProperty = targetFatElement.getAssociationTypeProperty();
      targetFt = associationTypeProperty.getAssociationFeatureTypes();
    }
    if( target instanceof Feature )
    {
      targetFeature = (Feature)target;
      targetFt = new FeatureType[]
      { targetFeature.getFeatureType() };
    }
    if( target instanceof LinkedFeatureElement2 )
    {
      linkedTargetFt = new FeatureType[]
      { ( (LinkedFeatureElement2)target ).getDecoratedFeature().getFeatureType() };
    }
    if( !LocalSelectionTransfer.getInstance().isSupportedType( transferType ) )
      return false;

    if( selectedFeatures.length == 0 )
      return false;

    if( selectedFeatures.length > 1 )
    {
      FeatureType baseFeatureType = null;
      for( int i = 0; i < selectedFeatures.length; i++ )
      {
        FeatureType type = selectedFeatures[i].getFeatureType();
        if( i == 0 )
          baseFeatureType = selectedFeatures[i].getFeatureType();
        if( !type.equals( baseFeatureType ) )
          return false;
      }
      matchingFt = hasMatchingFeatureType( baseFeatureType, targetFt );
    }
    else
    {
      selectedFeature = selectedFeatures[0];
      matchingFt = hasMatchingFeatureType( selectedFeature.getFeatureType(), targetFt );
    }
    if( targetFt == null )
      return false;

    if( matchingFt != null )
    {
      if( selectedFeatures.length > 1 )
      {
        FeatureType property = targetFeature.getFeatureType();
        int maxOccurs = property.getMaxOccurs( matchingFt.getName() );
        //      TODO add the already existing number of feature and compare to maxOccurs
        if( maxOccurs >= selectedFeatures.length && ( operation == DND.DROP_COPY || operation == DND.DROP_MOVE ) )
          return true;
      }
    }
    if( selectedFeature != null && matchingFt != null )
      return true;
    return false;
  }

  private FeatureType hasMatchingFeatureType( FeatureType sourceFT, FeatureType[] targetFT )
  {
    for( int j = 0; j < targetFT.length; j++ )
    {
      FeatureType featureType = targetFT[j];
      if( featureType.equals( sourceFT ) )
        return featureType;
      FeatureTypeProperty[] properties = featureType.getProperties();
      for( int i = 0; i < properties.length; i++ )
      {
        FeatureTypeProperty property = properties[i];
        if( property instanceof FeatureAssociationTypeProperty )
        {
          FeatureType[] associationFeatureTypes = ( (FeatureAssociationTypeProperty)property )
              .getAssociationFeatureTypes();
          for( int k = 0; k < associationFeatureTypes.length; k++ )
          {
            FeatureType type = associationFeatureTypes[k];
            if( type.equals( null ) )
              return featureType;
          }
        }
      }
    }
    return null;
  }

  private boolean checkOccurence( FeatureType sourceFt, FeatureType[] targetFt )
  {

    return false;
  }
}
