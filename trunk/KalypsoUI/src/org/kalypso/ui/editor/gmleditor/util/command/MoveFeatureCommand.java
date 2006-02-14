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

import java.util.ArrayList;
import java.util.List;

import org.kalypso.commons.command.ICommand;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * @author belger
 */
public class MoveFeatureCommand implements ICommand
{
  private final Feature m_parentFeature;

  private final Object m_moveItem;

  public static int UP = 0;

  public static int DOWN = 1;

  private int m_type = -1;

  private int index = -1;

  private final IRelationType m_propName;

  private final GMLWorkspace m_workspace;

  public MoveFeatureCommand( final GMLWorkspace workspace, Feature parentFeature, IRelationType propName, Object moveItem,
      int type )
  {
    m_workspace = workspace;
    m_parentFeature = parentFeature;
    m_propName = propName;
    m_moveItem = moveItem;
    m_type = type;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  public boolean isUndoable()
  {
    return true;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  public void process() throws Exception
  {
    move();
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    move();
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    Object prop = m_parentFeature.getProperty( m_propName );
    Object properties[] = m_parentFeature.getProperties();
    int propIndex = 0;
    for( ; propIndex < properties.length; propIndex++ )
      if( properties[propIndex] == prop )
        break;

    final IPropertyType pt = m_parentFeature.getFeatureType().getProperties(propIndex);

    if( pt.isList())
    {
      final List list = (List)prop;
      index = list.indexOf( m_moveItem );
      if( m_type == UP && ( ( index - 1 ) >= 0 ) )
      {
        list.remove( m_moveItem );
        list.add( ( index + 1 ), m_moveItem );
      }
      else if( m_type == DOWN && ( ( index + 1 ) < list.size() ) )
      {
        list.remove( m_moveItem );
        list.add( ( index - 1 ), m_moveItem );
      }
      final List feList = new ArrayList();
      feList.add( m_parentFeature );
      m_workspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_workspace, m_parentFeature,
          FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_MOVE ) );
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Feature löschen";
  }

  private void move()
  {
    Object prop = m_parentFeature.getProperty( m_propName );
    Object properties[] = m_parentFeature.getProperties();
    int propIndex = 0;
    for( ; propIndex < properties.length; propIndex++ )
      if( properties[propIndex] == prop )
        break;

    final IPropertyType pt = m_parentFeature.getFeatureType().getProperties(propIndex);

    if( pt.isList())
    {
      List list = (List)prop;
      index = list.indexOf( m_moveItem );
      if( m_type == UP && ( ( index - 1 ) >= 0 ) )
      {
        list.remove( m_moveItem );
        list.add( ( index - 1 ), m_moveItem );
      }
      else if( m_type == DOWN && ( ( index + 1 ) < list.size() ) )
      {
        list.remove( m_moveItem );
        list.add( ( index + 1 ), m_moveItem );
      }

      final List feList = new ArrayList();
      feList.add( m_parentFeature );
      m_workspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_workspace, m_parentFeature,
          FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_MOVE ) );
    }
  }
}