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

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerDropAdapter;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.TransferData;
import org.eclipse.ui.views.navigator.LocalSelectionTransfer;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.CommandableFeatureSelection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;

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

    Object currentTarget2 = getCurrentTarget();
    Object selectedObject = getSelectedObject();
    if( currentTarget2 instanceof Feature && selectedObject instanceof Feature )
    {
      Feature targetFeature = (Feature)currentTarget2;
      Feature sourceFeature = (Feature)selectedObject;
      ISelection selection = m_viewer.getSelection();
      if( selection instanceof CommandableFeatureSelection )
      {
        CommandableFeatureSelection cfs = (CommandableFeatureSelection)selection;
        CommandableWorkspace workspace = cfs.getCommandableWorkspace();
        if( cfs.size() > 1 )
          return false;
        else
        {
          Feature element = (Feature)cfs.getFirstElement();
          System.out.println( "" );
        }
      }
    }
    if( selectedObject instanceof Feature && currentTarget2 instanceof FeatureAssociationTypeElement )
    {
      Feature sourceFeature = (Feature)selectedObject;
      FeatureAssociationTypeProperty fatpTarget = (FeatureAssociationTypeProperty)currentTarget2;

    }
    return true;
  }

  /**
   * @see org.eclipse.jface.viewers.ViewerDropAdapter#validateDrop(java.lang.Object, int,
   *      org.eclipse.swt.dnd.TransferData)
   */
  public boolean validateDrop( Object target, int operation, TransferData transferType )
  {
    String opsName = null;
    ISelection currentSelection = m_viewer.getSelection();
    GMLWorkspace workspace = null;
    Feature sourceFeature = null;
    Feature targetFeature = null;
    FeatureType targetFt = null;
    if( target instanceof FeatureAssociationTypeElement )
    {
      FeatureAssociationTypeElement targetFatElement = (FeatureAssociationTypeElement)target;
      FeatureAssociationTypeProperty associationTypeProperty = targetFatElement.getAssociationTypeProperty();
      targetFt = associationTypeProperty.getAssociationFeatureType();
    }
    if( target instanceof Feature )
    {
      targetFeature = (Feature)target;
      targetFt = targetFeature.getFeatureType();
    }
    if( currentSelection instanceof CommandableFeatureSelection )
    {
      workspace = ( (CommandableFeatureSelection)currentSelection ).getCommandableWorkspace();
      sourceFeature = (Feature)( (CommandableFeatureSelection)currentSelection ).getFirstElement();
    }
    boolean valid = isCompatibleFeatureType( sourceFeature.getFeatureType(), targetFeature.getFeatureType() );
    
    if( !LocalSelectionTransfer.getInstance().isSupportedType( transferType ) && !valid)
      return false;
    return true;
  }

  /**
   * @param featureType
   * @param featureType2
   * @return
   */
  private boolean isCompatibleFeatureType( FeatureType sourceFt, FeatureType targetFt )
  {
    FeatureTypeProperty[] properties = sourceFt.getProperties();
    for( int i = 0; i < properties.length; i++ )
    {
      FeatureTypeProperty property = properties[i];
      if( targetFt.equals( property ) )
        return true;
    }
    return false;
  }
}
