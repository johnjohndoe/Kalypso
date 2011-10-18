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
package org.kalypso.model.wspm.tuhh.core.profile.importer.wprof;

import java.math.BigDecimal;
import java.net.URI;

import org.kalypso.model.wspm.tuhh.core.wprof.IWProfPoint;
import org.kalypso.model.wspm.tuhh.core.wprof.WProfProfileType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author Gernot Belger
 */
public class BridgePoint implements IWProfPoint
{
  private BigDecimal m_distance;

  private final IWProfPoint m_wprofPoint;

  public BridgePoint( final IWProfPoint wprofPoint )
  {
    m_wprofPoint = wprofPoint;
  }

  @Override
  public BigDecimal getDistance( )
  {
    if( m_distance != null )
      return m_distance;

    return m_wprofPoint.getDistance();
  }

  public void setDistance( final BigDecimal distance )
  {
    m_distance = distance;
  }

  /**
   * @return
   * @see org.kalypso.model.wspm.tuhh.core.wprof.IWProfPoint#getComment()
   */
  @Override
  public String getComment( )
  {
    return m_wprofPoint.getComment();
  }

  /**
   * @return
   * @see org.kalypso.model.wspm.tuhh.core.wprof.IWProfPoint#getFeature()
   */
  @Override
  public Feature getFeature( )
  {
    return m_wprofPoint.getFeature();
  }

  /**
   * @return
   * @see org.kalypso.model.wspm.tuhh.core.wprof.IWProfPoint#getLocation()
   */
  @Override
  public GM_Point getLocation( )
  {
    return m_wprofPoint.getLocation();
  }

  /**
   * @return
   * @see org.kalypso.model.wspm.tuhh.core.wprof.IWProfPoint#getNumber()
   */
  @Override
  public Number getNumber( )
  {
    return m_wprofPoint.getNumber();
  }

  /**
   * @return
   * @see org.kalypso.model.wspm.tuhh.core.wprof.IWProfPoint#getObjectType()
   */
  @Override
  public String getObjectType( )
  {
    return m_wprofPoint.getObjectType();
  }

  /**
   * @return
   * @see org.kalypso.model.wspm.tuhh.core.wprof.IWProfPoint#getPartNumber()
   */
  @Override
  public int getPartNumber( )
  {
    return m_wprofPoint.getPartNumber();
  }

  /**
   * @return
   * @see org.kalypso.model.wspm.tuhh.core.wprof.IWProfPoint#getPhotos()
   */
  @Override
  public URI[] getPhotos( )
  {
    return m_wprofPoint.getPhotos();
  }

  /**
   * @return
   * @see org.kalypso.model.wspm.tuhh.core.wprof.IWProfPoint#getPNam()
   */
  @Override
  public String getPNam( )
  {
    return m_wprofPoint.getPNam();
  }

  /**
   * @return
   * @see org.kalypso.model.wspm.tuhh.core.wprof.IWProfPoint#getProfileComment()
   */
  @Override
  public String getProfileComment( )
  {
    return m_wprofPoint.getProfileComment();
  }

  /**
   * @return
   * @see org.kalypso.model.wspm.tuhh.core.wprof.IWProfPoint#getProfileName()
   */
  @Override
  public String getProfileName( )
  {
    return m_wprofPoint.getProfileName();
  }

  @Override
  public WProfProfileType getProfileType( )
  {
    return m_wprofPoint.getProfileType();
  }

  /**
   * @return
   * @see org.kalypso.model.wspm.tuhh.core.wprof.IWProfPoint#getPunktattribut()
   */
  @Override
  public int getPunktattribut( )
  {
    return m_wprofPoint.getPunktattribut();
  }

  /**
   * @return
   * @see org.kalypso.model.wspm.tuhh.core.wprof.IWProfPoint#getRiverId()
   */
  @Override
  public String getRiverId( )
  {
    return m_wprofPoint.getRiverId();
  }

  /**
   * @return
   * @see org.kalypso.model.wspm.tuhh.core.wprof.IWProfPoint#getRiverName()
   */
  @Override
  public String getRiverName( )
  {
    return m_wprofPoint.getRiverName();
  }

  /**
   * @return
   * @see org.kalypso.model.wspm.tuhh.core.wprof.IWProfPoint#getStation()
   */
  @Override
  public BigDecimal getStation( )
  {
    return m_wprofPoint.getStation();
  }

  /**
   * @return
   * @see org.kalypso.model.wspm.tuhh.core.wprof.IWProfPoint#getValue()
   */
  @Override
  public double getValue( )
  {
    return m_wprofPoint.getValue();
  }
}
