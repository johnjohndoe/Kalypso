package org.kalypso.convert.namodel.optimize;

import java.net.URL;
import java.util.HashMap;

import org.kalypso.services.calculation.job.ICalcDataProvider;
import org.kalypso.services.calculation.service.CalcJobServiceException;

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

public class OptimizeCalcDataProvider implements ICalcDataProvider
{

  private final ICalcDataProvider m_calcDataProvider;

  private final HashMap m_map = new HashMap();

  /*
   * 
   * @author huebsch
   */
  public OptimizeCalcDataProvider( ICalcDataProvider calcDataProvider )
  {
    m_calcDataProvider = calcDataProvider;
  }

  public void addURL( String id, URL url )
  {
    m_map.put( id, url );
  }

  public URL getURLForID( String id ) throws CalcJobServiceException
  {
    if( m_map.containsKey( id ) )
    {
      return (URL)m_map.get( id );
    }
    return m_calcDataProvider.getURLForID( id );
  }

  public boolean hasID( String id )
  {
    if( m_map.containsKey( id ) )
      return true;
    return m_calcDataProvider.hasID( id );
  }
}
