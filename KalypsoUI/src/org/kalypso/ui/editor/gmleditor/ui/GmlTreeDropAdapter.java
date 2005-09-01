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
    Feature targetFeature = null;
    FeatureType targetFt = null;
    FeatureType[] linkedTargetFt = null;
    FeatureType matchingFt = null;
    FeatureAssociationTypeProperty targetAssocFtp = null;
    if( target instanceof FeatureAssociationTypeElement )
    {
      FeatureAssociationTypeElement targetFatElement = (FeatureAssociationTypeElement)target;
      targetFeature =  targetFatElement.getParentFeature();
//      hasMatchingFeatureType( selectedFeature)
      
    }
    if( target instanceof Feature )
    {
      targetFeature = (Feature)target;
      targetFt = targetFeature.getFeatureType();
      System.out.println( "target: " + targetFt.getName() + "\tsource: "
          + selectedFeatures[0].getFeatureType().getName() );
    }
    if( target instanceof LinkedFeatureElement2 )
    {
      linkedTargetFt = new FeatureType[]
      { ( (LinkedFeatureElement2)target ).getDecoratedFeature().getFeatureType() };
      System.out.println( "target: " + linkedTargetFt[0].getName() + " -> linked_feature\tsource: "
          + selectedFeatures[0].getFeatureType().getName() );
    }
    if( !LocalSelectionTransfer.getInstance().isSupportedType( transferType ) )
      return false;

    if( selectedFeatures.length == 0 )
      return false;
    // grundsätzlich ist es nicht erlaubt verschiedene Features zu verschieben, copieren und linken
    if( selectedFeatures.length > 1 )
    {
      //prüft ob alle selektierten Features vom selben typ sind
      FeatureType baseFeatureType = null;
      for( int i = 0; i < selectedFeatures.length; i++ )
      {
        FeatureType type = selectedFeatures[i].getFeatureType();
        if( i == 0 )
          baseFeatureType = selectedFeatures[i].getFeatureType();
        if( !type.equals( baseFeatureType ) )
        {
          System.out.print( "\tselectedFeatures.length > 1 = no type: " + false );
          return false;
        }
      }
      // wenn ja, versuche einen match zu finden
      matchingFt = hasMatchingFeatureType( baseFeatureType, new FeatureType[]
      { targetFt } );
      if( matchingFt == null )
        return false;
    }
    else
    {
      //ist nur ein Feature selektiert wird versucht ein match herzustelln
      selectedFeature = selectedFeatures[0];
      matchingFt = hasMatchingFeatureType( selectedFeature.getFeatureType(), new FeatureType[]
      { targetFt } );
    }
    if( matchingFt != null )
    {
      System.out.print( "\tmatchingFt != null " );
      if( selectedFeatures.length > 1 )
      {
        FeatureType property = targetFeature.getFeatureType();
        System.out.print( "\tmatchingFt != null  and selectedFeaturs.length > 1 -> targetFeature(FT name) "
            + property.getName() );
        int maxOccurs = property.getMaxOccurs( matchingFt.getName() );
        //      TODO add the already existing number of feature and compare to maxOccurs
        if( maxOccurs >= selectedFeatures.length && ( operation == DND.DROP_COPY || operation == DND.DROP_MOVE ) )
        {
          System.out.print( "\tmatchingFt != null : " + true );
          return true;
        }
      }
      else
      {
        System.out.print( "\tmatchingFt != null only one Feature Selected: " + true );
        return true;
      }
    }
    if( linkedTargetFt != null && selectedFeature != null )
    {
      System.out.print( "\tselectedFeature != null && linkedtargetFt != null : " + false );
      return false;
    }
    if( linkedTargetFt == null && operation == DND.DROP_LINK )
      if( selectedFeature != null && matchingFt != null )
      {
        System.out.print( "\tselectedFeature != null and matchingFt != null : " + true );
        return true;
      }
    return false;
  }

  /**
   * This method checks if there is a possible match between the supplied feature types
   * @param sourceFT the currently selected feature type
   * @param targetFT the target feature type under the curser
   * @return the feature type that matches the target
   * 			
   */
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
