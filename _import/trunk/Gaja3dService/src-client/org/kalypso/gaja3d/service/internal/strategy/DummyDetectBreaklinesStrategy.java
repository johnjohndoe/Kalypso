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
package org.kalypso.gaja3d.service.internal.strategy;

import java.net.URI;
import java.net.URISyntaxException;
import java.rmi.RemoteException;

import org.apache.axis.AxisFault;
import org.kalypso.gaja3d.service.client.Client;

/**
 * This strategy calls a (possibly local) WPS to start the simulation
 * 
 * @author kurzbach
 */
public class DummyDetectBreaklinesStrategy implements DetectBreaklinesStrategy
{

  public URI[] detectBreaklines( final URI[] boundaryLocation, final URI[] demGridLocation ) throws RemoteException
  {
    try
    {
      return new URI[] { Client.class.getResource( Client.BREAKLINES_FILENAME ).toURI() };
    }
    catch( final URISyntaxException e )
    {
      throw AxisFault.makeFault( e );
    }
  }

  /**
   * @param edgeMethod
   *          the edgeMethod to set
   */
  public void setEdgeMethod( String edgeMethod )
  {
  }

  /**
   * @param smoothMethod
   *          the smoothMethod to set
   */
  public void setSmoothMethod( String smoothMethod )
  {
  }

  /**
   * @param smooth
   *          the smooth to set
   */
  public void setSmooth( int smooth )
  {
  }

  /**
   * @param featureMethod
   *          the featureMethod to set
   */
  public void setFeatureMethod( String featureMethod )
  {
  }

  /**
   * @param lowThresh
   *          the lowThresh to set
   */
  public void setLowThresh( double lowThresh )
  {
  }

  /**
   * @param highThresh
   *          the highThresh to set
   */
  public void setHighThresh( double highThresh )
  {
  }

  /**
   * @param distanceTolerance
   *          the distanceTolerance to set
   */
  public void setDistanceTolerance( double distanceTolerance )
  {
  }

}
