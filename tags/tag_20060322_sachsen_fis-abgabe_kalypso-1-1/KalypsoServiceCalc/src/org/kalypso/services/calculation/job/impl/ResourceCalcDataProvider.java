/*--------------- Kalypso-Header ------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 --------------------------------------------------------------------------*/

package org.kalypso.services.calculation.job.impl;

import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import org.kalypso.services.calculation.job.ICalcDataProvider;
import org.kalypso.services.calculation.service.CalcJobServiceException;

/**
 * For testing purposes. Provides data from resources.
 * 
 * @author belger
 */
public class ResourceCalcDataProvider implements ICalcDataProvider
{
  private final String m_base;
  private Map m_resourceMap = new HashMap();

  public ResourceCalcDataProvider( final String base )
  {
    m_base = base;
  }

  public void addResource( final String id, final String resourcePath )
  {
    m_resourceMap.put( id, resourcePath );
  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcDataProvider#getURLForID(java.lang.String)
   */
  public URL getURLForID( final String id ) throws CalcJobServiceException
  {
    final String resourcePath = (String)m_resourceMap.get( id );
    
    return getClass().getResource( m_base + resourcePath );
  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcDataProvider#hasID(java.lang.String)
   */
  public boolean hasID( final String id )
  {
    return m_resourceMap.containsKey( id );
  }

}
