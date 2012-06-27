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
package org.kalypso.ui.rrm.internal.scenarios;

import org.kalypso.model.hydrology.binding.timeseriesMappings.ITimeseriesMapping;

/**
 * @author Holger Albert
 */
public class MappingValue
{
  private String m_featureId;

  private ITimeseriesMapping m_mapping;

  private final boolean m_copy;

  public MappingValue( final ITimeseriesMapping mapping, final boolean copy )
  {
    m_featureId = mapping.getId();
    m_mapping = mapping;
    m_copy = copy;
  }

  public void update( final ITimeseriesMapping mapping )
  {
    m_featureId = mapping.getId();
    m_mapping = mapping;
  }

  public String getFeatureId( )
  {
    return m_featureId;
  }

  public ITimeseriesMapping getMapping( )
  {
    return m_mapping;
  }

  public boolean isCopy( )
  {
    return m_copy;
  }
}