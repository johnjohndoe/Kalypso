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

/**
 * Wechmann Parameters
 * 
 * @author schlienger
 */
public final class WechmannParams
{
  /** Konstante W1 */
  private final double m_W1;

  /** Konstante LNK1 */
  private final double m_LNK1;

  /** Konstante K2 */
  private final double m_K2;

  /**
   * obere Wasserstandsgrenze in cm. When no WGR value is defined, this class supposes Double.MAX_VALUE is used
   */
  private final double m_WGR;

  /**
   * this is the Q computed from WGR usign the Wechmann function. This Q is also stored as a member of this class
   * because it is used when convert Q to W.
   * <p>
   * this is a computed value, it is not serialized.
   */
  private final double m_Q4WGR;

  /**
   * Creates the parameters with a WGR value of Double.MAX_VALUE. Use this constructor when the WGR value is not
   * defined, thus the parameters are valid for all possible W values.
   * 
   * @param W1
   * @param LNK1
   * @param K2
   */
  public WechmannParams( double W1, double LNK1, double K2 )
  {
    this( W1, LNK1, K2, Double.MAX_VALUE );
  }

  /**
   * Creates the parameters. They will be used until the Waterlevel reaches WGR.
   * 
   * @param W1
   *          Konstante
   * @param LNK1
   *          Konstante
   * @param K2
   *          Konstante
   * @param WGR
   *          obere Wasserstandsgrenze in cm
   *  
   */
  public WechmannParams( double W1, double LNK1, double K2, double WGR )
  {
    m_W1 = W1;
    m_LNK1 = LNK1;
    m_K2 = K2;
    m_WGR = WGR;

    m_Q4WGR = WechmannFunction.computeQ( LNK1, WGR, W1, K2 );
  }

  /**
   * @return K2
   */
  public double getK2()
  {
    return m_K2;
  }

  /**
   * @return LN(K1)
   */
  public double getLNK1()
  {
    return m_LNK1;
  }

  /**
   * @return W1
   */
  public double getW1()
  {
    return m_W1;
  }

  /**
   * @return WGR
   */
  public double getWGR()
  {
    return m_WGR;
  }

  /**
   * @return true if WGR was defined once object was constructed
   */
  public boolean hasWGR()
  {
    return Double.compare( m_WGR, Double.MAX_VALUE ) != 0;
  }

  /**
   * @return the corresponding Q-value to the WGR
   */
  public double getQ4WGR()
  {
    return m_Q4WGR;
  }
}