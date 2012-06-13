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
package org.kalypso.ui.rrm.internal.timeseries.view.filter;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeUiHandler;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNode;

/**
 * @author Dirk Kuch
 */
public class TimeseriesBrowserParameterTypeFilter extends ViewerFilter
{
  private String m_type;

  private StructuredViewer m_viewer;

  public static final String PROPERTY_TYPE = "type"; //$NON-NLS-1$

  public TimeseriesBrowserParameterTypeFilter( final String string )
  {
    setType( string );
  }

  @Override
  public boolean select( final Viewer viewer, final Object parentElement, final Object element )
  {
    final String type = getType();
    if( StringUtils.isEmpty( type ) )
      return true;

    if( element instanceof TreeNode )
    {
      final TreeNode node = (TreeNode) element;
      if( isEmptyNode( node ) )
        return true;

      if( hasChildWithType( node, type ) )
        return true;

      return false;
    }

    return false;
  }

  private boolean isEmptyNode( final TreeNode node )
  {
    final ITreeNodeUiHandler handler = node.getUiHandler();
    return StringUtils.containsIgnoreCase( handler.getClass().getName(), "EmptyNodeUiHandler" ); //$NON-NLS-1$
  }

  private boolean hasChildWithType( final TreeNode node, final String type )
  {

    final Object adapter = node.getAdapter( ITimeseries.class );
    if( adapter instanceof ITimeseries )
    {
      final ITimeseries timeseries = (ITimeseries) adapter;
      final String parameterType = timeseries.getParameterType();

      return StringUtils.equalsIgnoreCase( parameterType, type );
    }
    else
    {
      final TreeNode[] children = node.getChildren();
      for( final TreeNode child : children )
      {
        if( hasChildWithType( child, type ) )
          return true;
      }
    }

    return false;
  }

  public void setViewer( final StructuredViewer viewer )
  {
    m_viewer = viewer;
  }

  public String getType( )
  {
    return m_type;
  }

  public void setType( final String type )
  {
    m_type = StringUtils.isBlank( type ) ? null : type.toLowerCase();

    if( m_viewer != null )
      m_viewer.refresh();
  }

  public boolean doSelect( final String parameterType )
  {
    if( StringUtils.isEmpty( m_type ) || StringUtils.isBlank( m_type ) )
      return true;

    return StringUtils.equalsIgnoreCase( m_type, parameterType );
  }

}
