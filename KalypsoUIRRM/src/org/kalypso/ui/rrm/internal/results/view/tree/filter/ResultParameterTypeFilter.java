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
package org.kalypso.ui.rrm.internal.results.view.tree.filter;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.ui.rrm.internal.results.view.base.IHydrologyResultReference;
import org.kalypso.ui.rrm.internal.results.view.base.KalypsoHydrologyResults.CATCHMENT_RESULT_TYPE;
import org.kalypso.ui.rrm.internal.results.view.base.KalypsoHydrologyResults.NODE_RESULT_TYPE;
import org.kalypso.ui.rrm.internal.results.view.base.KalypsoHydrologyResults.STORAGE_RESULT_TYPE;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNode;

/**
 * @author Dirk Kuch
 */
public class ResultParameterTypeFilter extends ViewerFilter
{
  private Object m_type;

  private StructuredViewer m_viewer;

  public static final String PROPERTY_TYPE = "type"; //$NON-NLS-1$

  public ResultParameterTypeFilter( final String string )
  {
    setType( string );
  }

  @Override
  public boolean select( final Viewer viewer, final Object parentElement, final Object element )
  {
    final Object type = getType();
    if( StringUtils.EMPTY == type )
      return true;

    if( element instanceof TreeNode )
    {
      final TreeNode node = (TreeNode) element;

      if( doSelectNode( node, type ) )
        return true;

      return false;
    }

    return false;
  }

  private boolean doSelectNode( final TreeNode node, final Object type )
  {
    // TODO remove level

    if( type instanceof CATCHMENT_RESULT_TYPE )
      return doSelectCatchmentType( node, (CATCHMENT_RESULT_TYPE) type );
    else if( type instanceof NODE_RESULT_TYPE )
      return doSelectNodeType( node, (NODE_RESULT_TYPE) type );
    else if( type instanceof STORAGE_RESULT_TYPE )
      return doSelectStorageType( node, (STORAGE_RESULT_TYPE) type );

    return false;
  }

  private boolean doSelectStorageType( final TreeNode node, final STORAGE_RESULT_TYPE type )
  {
    final int level = getLevel( node );
    if( level == 3 )
    {
      final Object data = node.getData();
      if( Objects.notEqual( STORAGE_RESULT_TYPE.class, data ) )
        return false;

      final TreeNode[] children = node.getChildren();
      for( final TreeNode child : children )
      {
        if( doSelectStorageType( child, type ) )
          return true;
      }

      return false;
    }

    final Object adapter = node.getAdapter( IHydrologyResultReference.class );
    if( !(adapter instanceof IHydrologyResultReference) )
    {
      final TreeNode[] children = node.getChildren();
      for( final TreeNode child : children )
      {
        if( doSelectStorageType( child, type ) )
          return true;
      }

      return false;
    }

    final IHydrologyResultReference reference = (IHydrologyResultReference) adapter;
    if( Objects.notEqual( type, reference.getType() ) )
      return false;

    return reference.isValid();
  }

  private boolean doSelectNodeType( final TreeNode node, final NODE_RESULT_TYPE type )
  {
    final int level = getLevel( node );
    if( level == 3 )
    {
      final Object data = node.getData();
      if( Objects.notEqual( NODE_RESULT_TYPE.class, data ) )
        return false;

      final TreeNode[] children = node.getChildren();
      for( final TreeNode child : children )
      {
        if( doSelectNodeType( child, type ) )
          return true;
      }

      return false;
    }

    final Object adapter = node.getAdapter( IHydrologyResultReference.class );
    if( !(adapter instanceof IHydrologyResultReference) )
    {
      final TreeNode[] children = node.getChildren();
      for( final TreeNode child : children )
      {
        if( doSelectNodeType( child, type ) )
          return true;
      }

      return false;
    }

    final IHydrologyResultReference reference = (IHydrologyResultReference) adapter;
    if( Objects.notEqual( type, reference.getType() ) )
      return false;

    return reference.isValid();
  }

  private boolean doSelectCatchmentType( final TreeNode node, final CATCHMENT_RESULT_TYPE type )
  {
    final int level = getLevel( node );
    if( level == 3 )
    {
      final Object data = node.getData();
      if( Objects.notEqual( CATCHMENT_RESULT_TYPE.class, data ) )
        return false;

      final TreeNode[] children = node.getChildren();
      for( final TreeNode child : children )
      {
        if( doSelectCatchmentType( child, type ) )
          return true;
      }

      return false;
    }

    final Object adapter = node.getAdapter( IHydrologyResultReference.class );
    if( !(adapter instanceof IHydrologyResultReference) )
    {
      final TreeNode[] children = node.getChildren();
      for( final TreeNode child : children )
      {
        if( doSelectCatchmentType( child, type ) )
          return true;
      }

      return false;
    }

    final IHydrologyResultReference reference = (IHydrologyResultReference) adapter;
    if( Objects.notEqual( type, reference.getType() ) )
      return false;

    return reference.isValid();
  }

  public void setViewer( final StructuredViewer viewer )
  {
    m_viewer = viewer;
  }

  public Object getType( )
  {
    return m_type;
  }

  public void setType( final Object type )
  {
    m_type = type;

    if( m_viewer != null )
      m_viewer.refresh();
  }

  private int getLevel( final TreeNode node )
  {
    int count = 0;
    TreeNode parent = node.getParent();
    while( parent != null )
    {
      parent = parent.getParent();
      count += 1;
    }

    return count;
  }

}
