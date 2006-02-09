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
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * @author belger
 */
public class AddFeatureCommand implements ICommand
{
  private final Feature m_parentFeature;

  private final int m_pos;

  private final IRelationType m_propName;

  private final IFeatureType m_type;

  private Feature newFeature = null;

  private final CommandableWorkspace m_workspace;

  // private final IFeatureSelectionManager m_selectionManager;

  /** A map with key=IPropertyType and value=Object to pass properties when the feature is newly created */
  private final Map<IPropertyType, Object> m_props;

  public AddFeatureCommand( final CommandableWorkspace workspace, final IFeatureType type, final Feature parentFeature, final IRelationType propertyName, final int pos, final Map<IPropertyType, Object> properties, final IFeatureSelectionManager selectionManager )
  {
    m_workspace = workspace;
    m_parentFeature = parentFeature;
    m_propName = propertyName;
    m_pos = pos;
    m_props = properties;
    m_type = type;
    // m_selectionManager = selectionManager;
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
    newFeature = m_workspace.createFeature( m_type );
    if( m_props != null )
      addProperties();
    addFeature();
  }

  /**
   *  
   */
  private void addProperties( )
  {
    IPropertyType[] properties = newFeature.getFeatureType().getProperties();
    for( int i = 0; i < properties.length; i++ )
    {
      IPropertyType ftp = properties[i];
      Object property = m_props.get( ftp );
      /** Skip all FeatureAssociationProperties, there is a special method to handel these props */
      if( ftp instanceof IRelationType )
        continue;
      if( property != null )
        newFeature.setProperty( ftp, property );
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {
    if( newFeature == null )
      return;
    addFeature();
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {
    if( newFeature == null )
      return;

    Object prop = m_parentFeature.getProperty( m_propName );
    Object properties[] = m_parentFeature.getProperties();
    int propIndex = 0;
    for( ; propIndex < properties.length; propIndex++ )
      if( properties[propIndex] == prop )
        break;

    int maxOccurs = m_parentFeature.getFeatureType().getProperties( propIndex ).getMaxOccurs();

    if( maxOccurs == 1 )
    {
      properties[propIndex] = null;
    }
    else if( maxOccurs > 1 || maxOccurs == IPropertyType.UNBOUND_OCCURENCY )
    {
      List list = (List) prop;
      list.remove( newFeature );
    }
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
    m_workspace.addFeatureAsComposition( m_parentFeature, m_propName, m_pos, newFeature );
    m_workspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_workspace, m_parentFeature, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );

    // if( m_selectionManager != null )
    // m_selectionManager.changeSelection( new Feature[0], new EasyFeatureWrapper[]
    // { new EasyFeatureWrapper( m_workspace, newFeature, m_parentFeature, m_propName ) } );

  }
}