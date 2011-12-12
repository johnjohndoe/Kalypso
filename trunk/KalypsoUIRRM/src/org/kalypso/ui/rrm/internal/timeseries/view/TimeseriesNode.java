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
package org.kalypso.ui.rrm.internal.timeseries.view;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.jface.resource.ImageDescriptor;

/**
 * @author Gernot Belger
 */
public class TimeseriesNode
{
  private final Collection<TimeseriesNode> m_children = new ArrayList<>();

  private final TimeseriesNode m_parent;

  private final ITimeseriesNodeUiHandler m_uiHandler;

  private final Object m_treeData;

  private final ITimeseriesTreeModel m_model;

  public TimeseriesNode( final ITimeseriesTreeModel model, final TimeseriesNode parent, final ITimeseriesNodeUiHandler uiHandler, final Object treeData )
  {
    m_model = model;
    m_parent = parent;
    m_uiHandler = uiHandler;
    m_treeData = treeData;
  }

  public TimeseriesNode getParent( )
  {
    return m_parent;
  }

  public TimeseriesNode[] getChildren( )
  {
    return m_children.toArray( new TimeseriesNode[m_children.size()] );
  }

  public boolean hasChildren( )
  {
    return m_children.size() > 0;
  }

  public String getLabel( )
  {
    return m_uiHandler.getTreeLabel();
  }

  public ImageDescriptor getImage( )
  {
    return m_uiHandler.getTreeImage();
  }

  public String getIdentifier( )
  {
    return m_uiHandler.getIdentifier();
  }

  void addChild( final TimeseriesNode child )
  {
    m_children.add( child );
  }

  public ITimeseriesNodeUiHandler getUiHandler( )
  {
    return m_uiHandler;
  }

  @Override
  public boolean equals( final Object obj )
  {
    if( !(obj instanceof TimeseriesNode) )
      return false;

    final TimeseriesNode other = (TimeseriesNode) obj;

    return m_treeData.equals( other.m_treeData );
  }

  @Override
  public int hashCode( )
  {
    return m_treeData.hashCode();
  }

  public ITimeseriesTreeModel getModel( )
  {
    return m_model;
  }
}