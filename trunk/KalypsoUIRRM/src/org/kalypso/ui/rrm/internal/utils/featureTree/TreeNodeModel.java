/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import java.util.LinkedHashSet;
import java.util.Set;

import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.StructuredViewer;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.event.ModellEventListener;

/**
 * @author Gernot Belger
 */
public class TreeNodeModel implements ITreeNodeModel
{
  private TreeNode[] m_nodes;

  private final CommandableWorkspace m_workspace;

  private final StructuredViewer m_viewer;

  private final ITreeNodeStrategy m_strategy;

  public TreeNodeModel( final ITreeNodeStrategy strategy, final CommandableWorkspace workspace, final StructuredViewer viewer )
  {
    m_strategy = strategy;
    m_workspace = workspace;
    m_viewer = viewer;
  }

  @Override
  public TreeNode[] getRootElements( )
  {
    if( m_nodes == null )
    {
      final TreeNode rootNode = m_strategy.buildNodes( this );
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
  public void setSelection( final TreeNode... incoming )
  {
    final TreeNode[] selection = convert( incoming );

    m_viewer.setSelection( new StructuredSelection( selection ), true );
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
    final TreeNode node = findNode( treeDataToSelect );

    setSelection( node );
  }

  private TreeNode[] convert( final TreeNode[] others )
  {
    final Set<TreeNode> own = new LinkedHashSet<>();

    for( final TreeNode other : others )
    {
      final TreeNode found = findNode( other );
      if( Objects.isNotNull( found ) )
        own.add( found );
    }

    return own.toArray( new TreeNode[] {} );
  }

  private TreeNode findNode( final Object treeDataToSelect )
  {
    TreeNode select = null;
    if( treeDataToSelect instanceof TreeNode )
      select = (TreeNode) treeDataToSelect;
    else
      select = new TreeNode( this, null, null, treeDataToSelect );

    TreeNode ptr = null;
    final TreeNode[] nodes = getRootElements();
    for( final TreeNode node : nodes )
    {
      ptr = findNode( node, select );
      if( ptr != null )
        return ptr;
    }

    return null;
  }

  private TreeNode findNode( final TreeNode node, final TreeNode nodeToSelect )
  {
    if( node.equals( nodeToSelect ) )
      return node;

    final TreeNode[] children = node.getChildren();
    for( final TreeNode child : children )
    {
      final TreeNode found = findNode( child, nodeToSelect );
      if( Objects.isNotNull( found ) )
        return found;
    }

    return null;
  }
}