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
package org.kalypso.ogc.gml.command;

import java.util.List;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypso.util.command.ICommand;

/**
 * DOCUMENT ME!
 * 
 * @author doemming
 */
public class JMMarkSelectCommand implements ICommand
{
  private final List m_features;

  private final int mySelectionId;

  private final int mySelectionMode;

  private final GMLWorkspace m_workspace;

  public JMMarkSelectCommand( final GMLWorkspace workspace, final List features, int selectionId,
      int selectionModus )
  {
    m_workspace = workspace;
    m_features = features;
    mySelectionId = selectionId;
    mySelectionMode = selectionModus;
  }

  public boolean isUndoable()
  {
    return true;
  }

  public void process() throws Exception
  {
    redo();
  }

  public void redo() throws Exception
  {
    for( int i = 0; i < m_features.size(); i++ )
    {
      Feature fe = (Feature)m_features.get( i );
      switch( mySelectionMode )
      {
      case JMSelector.MODE_SELECT:
        fe.select( mySelectionId );
        break;
      case JMSelector.MODE_UNSELECT:
        fe.unselect( mySelectionId );
        break;
      case JMSelector.MODE_TOGGLE:
        fe.toggle( mySelectionId );
        break;
      default:
        break;
      }
    }

    m_workspace.fireModellEvent( null );
  }

  public void undo() throws Exception
  {
    for( int i = 0; i < m_features.size(); i++ )
    {
      Feature fe = (Feature)m_features.get( i );
      switch( mySelectionMode )
      {
      case JMSelector.MODE_SELECT:
        fe.unselect( mySelectionId );
        break;
      case JMSelector.MODE_UNSELECT:
        fe.select( mySelectionId );
        break;
      case JMSelector.MODE_TOGGLE:
        fe.toggle( mySelectionId );
        break;
      default:
        break;
      }
    }

    m_workspace.fireModellEvent( null );
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "selectiert features";
  }
}