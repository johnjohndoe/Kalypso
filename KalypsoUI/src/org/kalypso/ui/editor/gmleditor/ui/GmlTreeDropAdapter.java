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

import java.util.List;

import org.eclipse.jface.viewers.ViewerDropAdapter;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.TransferData;
import org.eclipse.ui.views.navigator.LocalSelectionTransfer;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author kuepfer
 */
public class GmlTreeDropAdapter extends ViewerDropAdapter
{
  private GmlTreeView m_viewer;

  public GmlTreeDropAdapter( final GmlTreeView viewer )
  {
    super( viewer.getTreeViewer() );

    m_viewer = viewer;
  }

  /**
   * @see org.eclipse.jface.viewers.ViewerDropAdapter#performDrop(java.lang.Object)
   */
  @Override
  public boolean performDrop( Object data )
  {
    // System.out.print( "performDrop - " );
    final Object currentTargetObject = getCurrentTarget();
    if( currentTargetObject instanceof FeatureAssociationTypeElement )
    {
      // IRelationType fatp = ((FeatureAssociationTypeElement) currentTargetObject).getAssociationTypeProperty();
      // Feature parentFeature = ((FeatureAssociationTypeElement) currentTargetObject).getParentFeature();
      // Object proptery = parentFeature.getProperty( fatp );
      // final int pos;
      // if( fatp.isList() )
      // {
      // pos = ((List) proptery).size();
      // }
      // else
      // pos = 0;
      // boolean b = workspace.isAggrigatedLink( parentFeature, fatp.getName(), pos );

    }
    Object selectedSourceObject = getSelectedObject();

    int currentOperation = getCurrentOperation();
    if( currentOperation == DND.DROP_COPY )
    {
      if( selectedSourceObject instanceof Feature && currentTargetObject instanceof Feature )
      {
        // Feature sourceFeature = (Feature)selectedSourceObject;
      }
    }
    return true;
  }

  /**
   * @see org.eclipse.jface.viewers.ViewerDropAdapter#validateDrop(java.lang.Object, int,
   *      org.eclipse.swt.dnd.TransferData)
   */
  @Override
  public boolean validateDrop( final Object target, final int operation, final TransferData transferType )
  {
    final IFeatureSelection featureSelection = (IFeatureSelection) m_viewer.getSelection();
    final Feature[] selectedFeatures = FeatureSelectionHelper.getFeatures( featureSelection );

    // System.out.println( "\nvalidateDrop -> " + selectedFeatures[0].getId() + "\tops: " + operation );

    Feature targetFeature = null;
    IFeatureType matchingFt = null;
    IRelationType targetAssocFtp = null;
    if( !isValidSelection( selectedFeatures ) )
      return false;
    if( target instanceof LinkedFeatureElement2 )
      return false;
    if( target instanceof FeatureAssociationTypeElement )
    {
      FeatureAssociationTypeElement targetFatElement = (FeatureAssociationTypeElement) target;
      targetAssocFtp = targetFatElement.getAssociationTypeProperty();
      // System.out.println( "FeatuerAssociationTypeElement:\n target: " + targetAssocFtp.getName() );
      // String propertyName = targetAssocFtp.getName();
      targetFeature = targetFatElement.getParentFeature();
      // try to find matching IFeatureType
      IFeatureType targetFeatureType = targetAssocFtp.getTargetFeatureType();
      matchingFt = hasMatchingFeatureType( selectedFeatures[0].getFeatureType(), GMLSchemaUtilities.getSubstituts( targetFeatureType, null, false, true ) );
      // System.out.println( "matchingFT = " + matchingFt.getName() );
      if( matchingFt == null )
        return false;
      int maxOccurs = targetAssocFtp.getMaxOccurs();
      boolean isList = targetAssocFtp.isList();

      if( isList && operation == DND.DROP_LINK )
        return false;
      if( isList && (operation == DND.DROP_COPY || operation == DND.DROP_MOVE) )
      {
        final List featureList = (List) targetFeature.getProperty( targetAssocFtp );
        System.out.println( "Diff = " + new Integer( maxOccurs - (featureList.size() + selectedFeatures.length) ) );
        if( maxOccurs >= featureList.size() + selectedFeatures.length || maxOccurs == IPropertyType.UNBOUND_OCCURENCY )
          return true;
        return false;
      }
      if( !isList && targetFeature.getProperty( targetAssocFtp ) == null )// && operation == DND.DROP_LINK )
        return true;
      System.out.println( "isList = " + isList );
    }
    if( target instanceof Feature )
    {
      targetFeature = (Feature) target;
      if( FeatureHelper.isCollection( targetFeature ) )
      {
        IFeatureType[] featureTypeFromCollection = FeatureHelper.getFeatureTypeFromCollection( targetFeature );
        for( int i = 0; i < featureTypeFromCollection.length; i++ )
        {
          final IFeatureType type = featureTypeFromCollection[i];
          // System.out.println( type.getName() );
          if( type.equals( selectedFeatures[0].getFeatureType() ) )
            matchingFt = type;
        }
      }
      // targetFt = targetFeature.getFeatureType();
      // System.out.println( "Feature:\ntargetFT = " + targetFt.getName() + "\tsourceFT = "
      // + selectedFeatures[0].getFeatureType().getName() );

      // matchingFt = hasMatchingFeatureType( selectedFeatures[0].getFeatureType(), new IFeatureType[]
      // { targetFt } );
      // matchingFt = targetFt;
      // if( matchingFt == null )
      // return false;
    }
    if( !LocalSelectionTransfer.getInstance().isSupportedType( transferType ) )
      return false;

    if( matchingFt != null )
    {
      System.out.println( "matchingFT = " + matchingFt.getQName().getLocalPart() );
      if( selectedFeatures.length > 1 )
      {
        IFeatureType property = targetFeature.getFeatureType();
        System.out.println( "\tmatchingFt != null  and selectedFeaturs.length > 1 -> targetFeature(FT name) " + property.getQName().getLocalPart() );
        // TODO Christoph was passiert hier
        // int maxOccurs = property.getMaxOccurs( matchingFt.getName() );
        // // TODO add the already existing number of feature and compare to maxOccurs
        // if( maxOccurs >= selectedFeatures.length && ( operation == DND.DROP_COPY || operation == DND.DROP_MOVE ) )
        // {
        // System.out.print( "\tmatchingFt != null check MaxOccurs -> " + true );
        return false;
        // }
      }
      else
      {
        return true;
      }
    }
    return false;
  }

  /**
   * This method checks if there is a possible match between the supplied feature types. If there are
   * FeatureAssociations in the Feature the method only check two levles into the feature for a potential match. NOT
   * RECURSIVE (performance problem).
   * 
   * @param sourceFT
   *          the currently selected feature type
   * @param targetFT
   *          the target feature type under the curser
   * @return the feature type that matches the target
   */
  private IFeatureType hasMatchingFeatureType( IFeatureType sourceFT, IFeatureType[] targetFT )
  {
    if( targetFT == null || sourceFT == null )
      return null;
    for( int j = 0; j < targetFT.length; j++ )
    {
      final IFeatureType featureType = targetFT[j];
      if( featureType.equals( sourceFT ) )
        return featureType;
      final IPropertyType[] properties = featureType.getProperties();
      for( int i = 0; i < properties.length; i++ )
      {
        final IPropertyType property = properties[i];
        if( property instanceof IRelationType )
        {
          final IFeatureType associationFeatureType = ((IRelationType) property).getTargetFeatureType();
          final IFeatureType[] associationFeatureTypes = GMLSchemaUtilities.getSubstituts( associationFeatureType, null, false, true );
          for( int k = 0; k < associationFeatureTypes.length; k++ )
          {
            final IFeatureType aFType = associationFeatureTypes[k];
            if( aFType.equals( sourceFT ) )
              return aFType;
          }
        }
      }
    }
    return null;
  }

  /**
   * This method checks if the selected Features are all of the same type and the array is neither null or empty.
   * 
   * @param features
   *          array of Features to check
   * @return true if all conditions above applay else false.
   */
  private boolean isValidSelection( Feature[] features )
  {
    // prüft ob alle selektierten Features vom selben typ sind und die Selection nicht leer ist
    if( features == null || features.length == 0 )
      return false;
    IFeatureType baseFeatureType = null;
    for( int i = 0; i < features.length; i++ )
    {
      IFeatureType type = features[i].getFeatureType();
      if( i == 0 )
        baseFeatureType = features[i].getFeatureType();
      if( !type.equals( baseFeatureType ) )
      {
        return false;
      }
    }
    return true;
  }
}
