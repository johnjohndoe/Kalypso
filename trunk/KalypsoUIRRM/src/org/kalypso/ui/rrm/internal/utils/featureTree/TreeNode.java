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

import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.resource.ImageDescriptor;
import org.kalypso.commons.java.lang.Objects;

/**
 * @author Gernot Belger
 */
public class TreeNode implements IAdaptable, Comparable<TreeNode>
{
  private final Collection<TreeNode> m_children = new ArrayList<>();

  private final TreeNode m_parent;

  private final ITreeNodeUiHandler m_uiHandler;

  private final Object m_treeData;

  private final ITreeNodeModel m_model;

  public TreeNode( final TreeNode parent, final ITreeNodeUiHandler uiHandler, final Object treeData )
  {
    this( parent.getModel(), parent, uiHandler, treeData );
  }

  public TreeNode( final ITreeNodeModel model, final TreeNode parent, final ITreeNodeUiHandler uiHandler, final Object treeData )
  {
    m_model = model;
    m_parent = parent;
    m_uiHandler = uiHandler;
    m_treeData = treeData;
  }

  public TreeNode getParent( )
  {
    return m_parent;
  }

  public TreeNode[] getChildren( )
  {
    return m_children.toArray( new TreeNode[m_children.size()] );
  }

  public boolean hasChildren( )
  {
    return m_children.size() > 0;
  }

  public String getLabel( )
  {
    if( Objects.isNull( m_uiHandler ) )
      return StringUtils.EMPTY;

    return m_uiHandler.getTreeLabel();
  }

  public ImageDescriptor getImage( )
  {
    return m_uiHandler.getTreeImage();
  }

  public void addChild( final TreeNode child )
  {
    Assert.isTrue( child.getParent() == this );

    m_children.add( child );
  }

  public ITreeNodeUiHandler getUiHandler( )
  {
    return m_uiHandler;
  }

  @Override
  public boolean equals( final Object obj )
  {
    if( !(obj instanceof TreeNode) )
      return false;

    final TreeNode other = (TreeNode) obj;

    return m_treeData.equals( other.m_treeData );
  }

  @Override
  public int hashCode( )
  {
    return m_treeData.hashCode();
  }

  public ITreeNodeModel getModel( )
  {
    return m_model;
  }

  public Object getData( )
  {
    return m_treeData;
  }

  @Override
  public Object getAdapter( final Class adapter )
  {
    if( !(m_treeData instanceof IAdaptable) )
      return null;

    return ((IAdaptable) m_treeData).getAdapter( adapter );
  }

  @Override
  public int compareTo( final TreeNode other )
  {
    final ITreeNodeUiHandler h1 = getUiHandler();
    final ITreeNodeUiHandler h2 = other.getUiHandler();

    return h1.compareTo( h2 );

  }
}