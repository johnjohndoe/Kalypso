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

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventProvider;
import org.kalypso.util.command.ICommand;

/**
 * @author belger
 */
public class AddFeatureCommand implements ICommand
{
  private final Feature m_parentFeature;

  private final ModellEventProvider m_eventprovider;

  private GMLWorkspace m_workspace;

  private int m_pos = 0;

  private final String m_propName;

  private FeatureType m_type;

  private Feature newFeature = null;

  public AddFeatureCommand( final ModellEventProvider eventprovider, FeatureType type,
      Feature parentFeature, String propertyName, int pos )
  {
    m_eventprovider = eventprovider;
    m_parentFeature = parentFeature;
    m_propName = propertyName;
    m_pos = pos;
    m_type = type;
    m_workspace = (GMLWorkspace)eventprovider;
  }

  /**
   * @see org.kalypso.util.command.ICommand#isUndoable()
   */
  public boolean isUndoable()
  {
    return true;
  }

  /**
   * @see org.kalypso.util.command.ICommand#process()
   */
  public void process() throws Exception
  {
    addFeature();
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    m_workspace.addFeature( m_parentFeature, m_propName, m_pos, newFeature );
    m_eventprovider.fireModellEvent( new ModellEvent( m_eventprovider, ModellEvent.FULL_CHANGE ) );
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    if( newFeature == null )
      return;

    Object prop = m_parentFeature.getProperty( m_propName );
    Object properties[] = m_parentFeature.getProperties();
    int propIndex = 0;
    for( ; propIndex < properties.length; propIndex++ )
      if( properties[propIndex] == prop )
        break;

    int maxOccurs = m_parentFeature.getFeatureType().getMaxOccurs( propIndex );

    if( maxOccurs == 1 )
    {
      properties[propIndex] = null;
    }
    else if( maxOccurs > 1 || maxOccurs == FeatureType.UNBOUND_OCCURENCY )
    {
      List list = (List)prop;
      list.remove( newFeature );
    }
    if( m_eventprovider != null )
      m_eventprovider.fireModellEvent( new ModellEvent( m_eventprovider, ModellEvent.FULL_CHANGE ) );
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Feature addieren";
  }

  private void addFeature() throws Exception
  {
    newFeature = m_workspace.createFeature( m_type );
    m_workspace.addFeature( m_parentFeature, m_propName, m_pos, newFeature );
    m_eventprovider.fireModellEvent( new ModellEvent( m_eventprovider, ModellEvent.FULL_CHANGE ) );
  }
}