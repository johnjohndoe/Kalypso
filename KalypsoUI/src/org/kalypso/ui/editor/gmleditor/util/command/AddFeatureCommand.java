/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.editor.gmleditor.util.command;

import java.util.List;
import java.util.Map;

import org.kalypso.commons.command.ICommand;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * @author Gernot Belger
 */
public class AddFeatureCommand implements ICommand
{
  private final Feature m_parentFeature;

  private final int m_pos;

  private final IRelationType m_propName;

  private final IFeatureType m_type;

  private Feature m_newFeature = null;

  private final CommandableWorkspace m_workspace;

  /** A map with key=IPropertyType and value=Object to pass properties when the feature is newly created */
  private final Map<IPropertyType, Object> m_props;

  private final IFeatureSelectionManager m_selectionManager;

  private final int m_depth;

  public AddFeatureCommand( final CommandableWorkspace workspace, final IFeatureType type, final Feature parentFeature, final IRelationType propertyName, final int pos, final Map<IPropertyType, Object> properties, final IFeatureSelectionManager selectionManager, final int depth )
  {
    m_workspace = workspace;
    m_parentFeature = parentFeature;
    m_propName = propertyName;
    m_pos = pos;
    m_props = properties;
    m_type = type;
    m_selectionManager = selectionManager;
    m_depth = depth;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  public boolean isUndoable( )
  {
    return true;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  public void process( ) throws Exception
  {
    m_newFeature = m_workspace.createFeature( m_parentFeature, m_type, m_depth );

    if( m_props != null )
      setProperties();

    addFeature();
  }

  private void setProperties( )
  {
    IPropertyType[] properties = m_newFeature.getFeatureType().getProperties();
    for( int i = 0; i < properties.length; i++ )
    {
      IPropertyType ftp = properties[i];
      Object property = m_props.get( ftp );
      /** Skip all FeatureAssociationProperties, there is a special method to handel these props */
      if( ftp instanceof IRelationType )
        continue;
      if( property != null )
        m_newFeature.setProperty( ftp, property );
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {
    if( m_newFeature == null )
      return;

    addFeature();
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {
    if( m_newFeature == null )
      return;

    if( m_propName.isList() )
    {
      final List list = (List) m_parentFeature.getProperty( m_propName );
      list.remove( m_newFeature );
    }
    else
      m_parentFeature.setProperty( m_propName, null );

    m_workspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_workspace, m_parentFeature, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE ) );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return "Feature hinzufügen";
  }

  private void addFeature( ) throws Exception
  {
    m_workspace.addFeatureAsComposition( m_parentFeature, m_propName, m_pos, m_newFeature );
    m_workspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_workspace, m_parentFeature, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );

    if( m_selectionManager != null )
      m_selectionManager.changeSelection( FeatureSelectionHelper.getFeatures( m_selectionManager ), new EasyFeatureWrapper[] { new EasyFeatureWrapper( m_workspace, m_newFeature, m_parentFeature, m_propName ) } );

  }
}