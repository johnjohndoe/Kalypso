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
package org.kalypso.model.wspm.pdb.ui.internal.content.filter;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.IWaterBodyStructure;
import org.kalypso.model.wspm.pdb.ui.internal.content.IConnectionViewer;

/**
 * @author Gernot Belger
 */
public class WaterBodiesFilter extends ViewerFilter
{
  public static final String PROPERTY_GKN = "gkn"; //$NON-NLS-1$

  public static final String PROPERTY_NAME = "name"; //$NON-NLS-1$

  private String m_gkn;

  private String m_name;

  private StructuredViewer m_viewer = null;

  private final IConnectionViewer m_connectionViewer;

  public WaterBodiesFilter( final String gkn, final String name, final IConnectionViewer viewer )
  {
    m_connectionViewer = viewer;
    setGkn( gkn );
    setName( name );
  }

  public void setViewer( final StructuredViewer viewer )
  {
    m_viewer = viewer;
  }

  @Override
  public boolean select( final Viewer viewer, final Object parentElement, final Object element )
  {
    if( element instanceof WaterBody )
    {
      if( StringUtils.isBlank( m_gkn ) && StringUtils.isBlank( m_name ) )
        return true;

      return containsChildWithName( (WaterBody) element );
    }

    return true;
  }

  public String getGkn( )
  {
    return m_gkn;
  }

  public String getName( )
  {
    return m_name;
  }

  public void setGkn( final String gkn )
  {
    m_gkn = StringUtils.isBlank( gkn ) ? null : gkn.toLowerCase();

    if( m_viewer != null )
      m_viewer.refresh();
  }

  public void setName( final String name )
  {
    m_name = StringUtils.isBlank( name ) ? null : name.toLowerCase();

    if( m_viewer != null )
      m_viewer.refresh();
  }

  public boolean containsChildWithName( final WaterBody element )
  {
    final String myName = element.getLabel().toLowerCase();
    final String myGkn = element.getName();

    if( !StringUtils.isBlank( m_name ) && myName.contains( m_name ) )
      return true;
    if( !StringUtils.isBlank( m_gkn ) && myGkn.startsWith( m_gkn ) )
      return true;

    if( m_connectionViewer == null )
      return false;

    final IWaterBodyStructure structure = m_connectionViewer.getStructure();
    if( structure == null )
      return false;

    final Object[] children = structure.getChildren( element );
    for( final Object child : children )
    {
      if( child instanceof WaterBody && containsChildWithName( (WaterBody) child ) )
        return true;
    }

    return false;
  }
}