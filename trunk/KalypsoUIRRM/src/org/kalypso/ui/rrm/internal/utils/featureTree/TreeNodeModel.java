/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.ui.rrm.internal.utils.featureTree;

import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.StructuredViewer;
import org.kalypso.commons.command.ICommand;
import org.kalypso.model.hydrology.timeseries.binding.IStationCollection;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.rrm.internal.timeseries.view.StationsByStationsStrategy;
import org.kalypsodeegree.model.feature.event.ModellEventListener;

/**
 * @author Gernot Belger
 */
public class TreeNodeModel implements ITreeNodeModel
{
  private final IStationCollection m_stations;

  private TreeNode[] m_nodes;

  private final CommandableWorkspace m_workspace;

  private final StructuredViewer m_viewer;

  public TreeNodeModel( final IStationCollection stations, final CommandableWorkspace workspace, final StructuredViewer viewer )
  {
    m_stations = stations;
    m_workspace = workspace;
    m_viewer = viewer;
  }

  @Override
  public TreeNode[] getRootElements( )
  {
    if( m_nodes == null )
    {
      // TODO: get strategy from outside
      final TreeNode rootNode = new StationsByStationsStrategy( this, m_stations ).buildNodes();
      m_nodes = rootNode.getChildren();
    }

    return m_nodes;
  }

  @Override
  public void clear( )
  {
    m_nodes = null;
  }

  @Override
  public void postCommand( final ICommand command ) throws Exception
  {
    m_workspace.postCommand( command );
  }

  @Override
  public void setSelection( final TreeNode... selection )
  {
    m_viewer.setSelection( new StructuredSelection( selection ) );
  }

  @Override
  public void addModellListener( final ModellEventListener modelListener )
  {
    m_workspace.addModellListener( modelListener );
  }

  @Override
  public void removeModellListener( final ModellEventListener modelListener )
  {
    m_workspace.removeModellListener( modelListener );
  }

  @Override
  public CommandableWorkspace getWorkspace( )
  {
    return m_workspace;
  }

  @Override
  public void refreshTree( final Object treeDataToSelect )
  {
    clear();

    m_viewer.refresh();

    // TODO: does not work properly: instead, find node with same tree data after clear and select it.
    final TreeNode nodeToSelect = new TreeNode( this, null, null, treeDataToSelect );
    setSelection( nodeToSelect );
  }
}