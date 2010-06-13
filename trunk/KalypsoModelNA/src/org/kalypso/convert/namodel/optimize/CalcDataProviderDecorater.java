package org.kalypso.convert.namodel.optimize;

import java.net.URL;
import java.util.HashMap;

import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.SimulationException;

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

/**
 * @author doemming TODO rename it, e.g. CalcDataProviderDecorator but nothing with Optimize....
 */
public class CalcDataProviderDecorater implements ISimulationDataProvider
{
  private final ISimulationDataProvider m_calcDataProvider;

  private final HashMap<String, URL> m_map = new HashMap<String, URL>();

  public CalcDataProviderDecorater( ISimulationDataProvider calcDataProvider )
  {
    m_calcDataProvider = calcDataProvider;
  }

  public void addURL( String id, URL url )
  {
    m_map.put( id, url );
  }

  @Override
  public Object getInputForID( String id ) throws SimulationException
  {
    if( m_map.containsKey( id ) )
    {
      return m_map.get( id );
    }
    return m_calcDataProvider.getInputForID( id );
  }

  @Override
  public boolean hasID( String id )
  {
    if( m_map.containsKey( id ) )
      return true;
    return m_calcDataProvider.hasID( id );
  }

  /**
   * @see org.kalypso.simulation.core.ISimulationDataProvider#dispose()
   */
  @Override
  public void dispose( )
  {
    m_calcDataProvider.dispose();
  }

  public ISimulationDataProvider getDecoratedDataProvider( )
  {
    return m_calcDataProvider;
  }
}
