/*--------------- Kalypso-Header --------------------------------------------------------------------

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
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.timeseries.wq.wechmann;

import org.kalypso.ogc.sensor.timeseries.wq.WQException;

/**
 * The Wechmann Function. Performs conversion from W to Q and from Q to W according to the Wechmann parameters.
 * <p>
 * The Wechmann Function is defined as follows:
 * 
 * <pre>
 * Q = exp( ln( K1 ) + ln( W - W1 ) * K2 )
 * 
 * having:
 * 
 * Q = computed runoff (in m³/s)
 * W = current waterlevel (in cm at the Gauge) oder auf Deutsch (cm am Pegel)
 * 
 * and
 * 
 * the Wechmann Parameters: ln(K1), W1,  K2
 * </pre>
 * 
 * @author schlienger
 */
public class WechmannFunction
{
  private WechmannFunction()
  {
  // not to be instanciated
  }

  /**
   * @param wp
   * @param W
   * @return Q
   * @see WechmannFunction#computeQ(double, double, double, double)
   *  
   */
  public static final double computeQ( final WechmannParams wp, final double W )
  {
    return computeQ( wp.getLNK1(), W, wp.getW1(), wp.getK2() );
  }

  /**
   * Computes the Q using the following function:
   * 
   * <pre>
   * Q = exp( ln( K1 ) + ln( W - W1 ) * K2 )
   * </pre>
   * 
   * @param LNK1
   * @param W
   * @param W1
   * @param K2
   * @return Q
   *  
   */
  public static final double computeQ( final double LNK1, final double W, final double W1, final double K2 )
  {
    if( W - W1 <= 0 )
    {
      // TODO: we thrown an exception before, but if W <= W1, then Q = 0. Please check this with an Hydrologue.
      // throw new WechmannException( "Log 0 not valid (W - W1 <= 0)" );
      return 0;
    }

    return Math.exp( LNK1 + Math.log( W - W1 ) * K2 );
  }

  /**
   * @param wp
   * @param Q
   * @return W
   * @see WechmannFunction#computeW(double, double, double, double)
   * 
   * @throws WQException
   */
  public static final double computeW( final WechmannParams wp, final double Q ) throws WQException
  {
    return computeW( wp.getW1(), Q, wp.getLNK1(), wp.getK2() );
  }

  /**
   * Computes the W using the following function:
   * 
   * <pre>
   *               ln( Q ) - ln( K1 )
   * W = W1 + exp( ------------------ )
   *                       K2
   * </pre>
   * 
   * @param W1
   * @param Q
   * @param LNK1
   * @param K2
   * @return W
   * 
   * @throws WQException
   *           when K2 = 0
   */
  public static final double computeW( final double W1, final double Q, final double LNK1, final double K2 )
      throws WQException
  {
    if( K2 == 0 )
      throw new WQException( "Divide by 0 (K2 = 0)" );

    return W1 + Math.exp( ( Math.log( Q ) - LNK1 ) / K2 );
  }
}