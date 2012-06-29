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
package org.kalypso.ui.rrm.internal.cm.view.filter;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.kalypso.ui.rrm.internal.cm.view.FactorizedTimeseriesBean;

/**
 * @author Holger Albert
 */
public class GroupViewerFilter extends ViewerFilter
{
  private String m_searchText;

  public GroupViewerFilter( )
  {
    m_searchText = null;
  }

  @Override
  public boolean select( final Viewer viewer, final Object parentElement, final Object element )
  {
    if( m_searchText == null || m_searchText.length() == 0 )
      return true;

    if( element instanceof FactorizedTimeseriesBean )
    {
      final FactorizedTimeseriesBean bean = (FactorizedTimeseriesBean) element;

      final String groupText = bean.getGroupText();
      if( groupText == null || !groupText.toLowerCase().contains( m_searchText.toLowerCase() ) )
        return false;

      return true;
    }

    return false;
  }

  public void updateSearchText( final String searchText )
  {
    m_searchText = searchText;
  }
}