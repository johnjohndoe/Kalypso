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
package org.kalypso.model.km.internal.ui.kmupdate;

import java.math.BigDecimal;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.kalypso.model.km.internal.binding.KMBindingUtils;
import org.kalypso.model.km.internal.binding.KMChannelElement;

import de.tu_harburg.wb.kalypso.rrm.kalininmiljukov.KalininMiljukovType.Profile;

/**
 * @author Gernot Belger
 */
public class KMProfileStationFilter extends ViewerFilter
{
  private KMChannelElement m_element;

  @Override
  public boolean select( final Viewer viewer, final Object parentElement, final Object element )
  {
    if( m_element == null )
      return true;

    if( element instanceof Profile )
    {
      final Profile profile = (Profile) element;
      final BigDecimal station = profile.getStation();
      return KMBindingUtils.isBetween( m_element.getKMType(), station );
    }

    return true;
  }

  public void setInput( final KMChannelElement element )
  {
    m_element = element;
  }
}
