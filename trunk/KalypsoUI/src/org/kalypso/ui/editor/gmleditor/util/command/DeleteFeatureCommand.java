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

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventProvider;
import org.kalypso.util.command.ICommand;

/**
 * @author belger
 */
public class DeleteFeatureCommand implements ICommand
{
  private final Feature m_parentFeature;

  private final Object m_deleteItem;

  private final ModellEventProvider m_eventprovider;

  private int index = -1;

  private final String m_propName;

  public DeleteFeatureCommand( final ModellEventProvider eventprovider, Feature parentFeature,
      String propName, Object deleteItem )
  {
    m_eventprovider = eventprovider;
    m_parentFeature = parentFeature;
    m_propName = propName;
    m_deleteItem = deleteItem;
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
    delete();
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    delete();
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    Object prop = m_parentFeature.getProperty( m_propName );
    Object properties[] = m_parentFeature.getProperties();
    int propIndex = 0;
    for( ; propIndex < properties.length; propIndex++ )
      if( properties[propIndex] == prop )
        break;

    int maxOccurs = m_parentFeature.getFeatureType().getMaxOccurs( propIndex );

    if( maxOccurs == 1 )
    {
      properties[index] = m_deleteItem;
      index = -1;
    }
    else if( maxOccurs > 1 || maxOccurs == FeatureType.UNBOUND_OCCURENCY )
    {
      List list = (List)prop;
      list.add( index, m_deleteItem );
      index = -1;
    }
    if( m_eventprovider != null )
      m_eventprovider
          .fireModellEvent( new ModellEvent( m_eventprovider, ModellEvent.FEATURE_CHANGE ) );
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Feature l�schen";
  }

  private void delete()
  {
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
      index = propIndex;
    }
    else if( maxOccurs > 1 || maxOccurs == FeatureType.UNBOUND_OCCURENCY )
    {
      List list = (List)prop;
      index = list.indexOf( m_deleteItem );
      list.remove( m_deleteItem );
    }
    if( m_eventprovider != null )
      m_eventprovider.fireModellEvent( new ModellEvent( m_eventprovider, ModellEvent.FULL_CHANGE ) );
  }
}