/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans;

import org.apache.commons.lang3.ArrayUtils;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.KnaufReach;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.base.KnaufProfileWrapper;

/**
 * @author Dirk Kuch
 */
public class KnaufSA15Bean extends AbstractKnaufProjectBean
{

  private final KnaufReach m_reach;

  private Double m_calculationDistance;

  private Double m_weirHeight = 0.0;

  private Double m_weirWidth = 0.0;

  private Double m_weirUeberfallBeiwert = 0.0;

  private Double m_epsh = 0.005;

  private Double m_epsv = 0.01;

  private Double m_rny = 1.31;

  private Double m_cwr = 1.5;

  public KnaufSA15Bean( final KnaufReach reach )
  {
    m_reach = reach;

    // FIXME for later processing values should be initialized from the outside
    doInit();
  }

  private void doInit( )
  {
    final KnaufProfileWrapper[] profiles = m_reach.getProfiles();

    if( ArrayUtils.getLength( profiles ) >= 2 )
    {
      final KnaufProfileWrapper p0 = profiles[0];
      final KnaufProfileWrapper pn = profiles[ArrayUtils.getLength( profiles ) - 1];

      m_calculationDistance = Math.abs( pn.getStation() - p0.getStation() ) * 1000;
    }

  }

  @Override
  public Integer getSatzart( )
  {
    return 15;
  }

  /**
   * @return Länge des Berechnungsabschnitts
   */
  public Double getCalculationDistance( )
  {
    return m_calculationDistance;
  }

  public void setCalculationDistance( final Double length )
  {
    m_calculationDistance = length;
  }

  public Double getWeirHeight( )
  {
    return m_weirHeight;
  }

  public void setWeirHeight( final Double height )
  {
    m_weirHeight = height;
  }

  public Double getWeirWidth( )
  {
    return m_weirWidth;
  }

  public void setWeirWidth( final Double width )
  {
    m_weirWidth = width;
  }

  public Double getWeirUeberfallBeiwert( )
  {
    return m_weirUeberfallBeiwert;
  }

  public void setWeirUeberfallBeiwert( final Double weirUeberfallBeiwert )
  {
    m_weirUeberfallBeiwert = weirUeberfallBeiwert;
  }

  public Double getEPSH( )
  {
    return m_epsh;
  }

  public void setEPSH( final Double epsh )
  {
    m_epsh = epsh;
  }

  public Double getEPSV( )
  {
    return m_epsv;
  }

  public void setEPSV( final Double epsv )
  {
    m_epsv = epsv;
  }

  /**
   * <pre>
   *  RNY in 1.0E-06 m2/s
   *  Wasser bei 10 C : RNY= 1.31
   *  Wasser bei 20 C : RNY= 1.00
   *  (programmintern wird mit 1.0E-06 multipliziert)
   *  Vorbesetzung : RNY = 1.31
   * </pre>
   */
  public Double getRNY( )
  {
    return m_rny;
  }

  public void setRNY( final Double rny )
  {
    m_rny = rny;
  }

  /**
   * <pre>
   *  71 - 80 F10.0 - Widerstandszahl für durchströmten Bewuchs
   *  Zahlenbereich 1.0 < CWR < 1.6
   *  Vorbesetzung : CWR = 1.5
   * </pre>
   */
  public Double getCWR( )
  {
    return m_cwr;
  }

  public void setCWR( final Double cwr )
  {
    m_cwr = cwr;
  }
}
