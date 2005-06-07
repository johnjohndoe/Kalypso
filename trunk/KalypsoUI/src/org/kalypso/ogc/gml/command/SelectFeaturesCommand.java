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

import java.util.Iterator;
import java.util.List;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree_impl.model.feature.visitors.GetSelectionVisitor;
import org.kalypsodeegree_impl.model.feature.visitors.UnselectFeatureVisitor;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.util.command.ICommand;

/**
 * Setzt die selektierten Features innerhalb eines Workspace
 * 
 * @author belger TODO rename to highlight...something
 */
public class SelectFeaturesCommand implements ICommand
{
  private final CommandableWorkspace m_workspace;

  private final Feature[] m_selection;

  private final int m_selectionID;

  private List m_selectedFeatures;

  private final UnselectFeatureVisitor m_unselectVisitor;

  public SelectFeaturesCommand( final CommandableWorkspace workspace, final Feature[] selection, final int selectionID )
  {
    m_workspace = workspace;
    m_selection = selection;
    m_selectionID = selectionID;

    m_selectedFeatures = GetSelectionVisitor.getSelectedFeatures( workspace, selectionID );
    m_unselectVisitor = new UnselectFeatureVisitor( m_selectionID );
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
    m_workspace.accept( m_unselectVisitor, m_workspace.getRootFeature(), m_selectionID );

    for( int i = 0; i < m_selection.length; i++ )
      m_selection[i].select( m_selectionID );

    m_workspace.fireModellEvent( new ModellEvent( m_workspace, ModellEvent.SELECTION_CHANGED ) );
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    process();
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    m_workspace.accept( m_unselectVisitor, m_workspace.getRootFeature(), m_selectionID );

    for( final Iterator selIt = m_selectedFeatures.iterator(); selIt.hasNext(); )
      ( (Feature)selIt.next() ).select( m_selectionID );

    m_workspace.fireModellEvent( new ModellEvent( m_workspace, ModellEvent.SELECTION_CHANGED ) );
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Objekte selektieren";
  }

}