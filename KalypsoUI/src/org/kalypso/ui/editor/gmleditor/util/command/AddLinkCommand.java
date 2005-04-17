package org.kalypso.ui.editor.gmleditor.util.command;

import org.kalypso.util.command.ICommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/

public class AddLinkCommand implements ICommand
{

  private final Feature m_parentFeature;

  private int m_pos = 0;

  private final String m_propName;

  private Feature m_linkFeature;

  private final GMLWorkspace m_workspace;

  public AddLinkCommand( final GMLWorkspace workspace, Feature parentFeature, String propertyName,
      int pos, Feature linkFeature )
  {
    m_workspace = workspace;
    m_parentFeature = parentFeature;
    m_propName = propertyName;
    m_pos = pos;
    m_linkFeature = linkFeature;
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
    addLink();
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    addLink();
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    if( m_linkFeature == null )
      return;

    m_workspace.removeLinkedFeature( m_parentFeature, m_propName, m_linkFeature );

    m_workspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_workspace,
        m_parentFeature, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE ) );
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return null;
  }

  private void addLink() throws Exception
  {
    m_workspace.addLinkedFeature( m_parentFeature, m_propName, m_pos, m_linkFeature );
    m_workspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_workspace,
        m_parentFeature, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
  }
}