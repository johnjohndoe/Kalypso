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
package org.kalypso.ui.rrm.internal.timeseries.view.filter;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNode;

/**
 * @author Dirk Kuch
 */
public class TimeseriesBrowserTextSearchFilter extends ViewerFilter
{
  private String m_string;

  private StructuredViewer m_viewer;

  public static final String PROPERTY_STRING = "string"; //$NON-NLS-1$

  public TimeseriesBrowserTextSearchFilter( final String string )
  {
    setString( string );
  }

  @Override
  public boolean select( final Viewer viewer, final Object parentElement, final Object element )
  {
    final String searchString = getString();
    if( StringUtils.isEmpty( searchString ) )
      return true;

    if( element instanceof TreeNode )
    {
      final TreeNode node = (TreeNode) element;

      if( hasChildWithName( node, searchString ) )
        return true;

      final int hierarchy = getLevel( node );
      if( hierarchy > 2 )
        return true;

      return false;
    }

    return false;
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

  private boolean hasChildWithName( final TreeNode node, final String searchString )
  {
    if( StringUtils.containsIgnoreCase( node.getLabel(), searchString ) )
      return true;

    final TreeNode[] children = node.getChildren();
    for( final TreeNode child : children )
    {
      if( hasChildWithName( child, searchString ) )
        return true;
    }

    return false;
  }

  public void setViewer( final StructuredViewer viewer )
  {
    m_viewer = viewer;
  }

  public String getString( )
  {
    return m_string;
  }

  public void setString( final String string )
  {
    m_string = StringUtils.isBlank( string ) ? null : string.toLowerCase();

    if( m_viewer != null )
      m_viewer.refresh();
  }

}
