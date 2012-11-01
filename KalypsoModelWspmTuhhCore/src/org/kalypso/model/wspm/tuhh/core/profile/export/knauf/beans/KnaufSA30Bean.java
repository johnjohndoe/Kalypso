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
package org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans;

import org.apache.commons.lang3.StringUtils;
import org.kalypso.commons.java.lang.Doubles;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.base.KNAUF_FLIESSGESETZ;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.base.KnaufProfileWrapper;

/**
 * @author Dirk Kuch
 */
public class KnaufSA30Bean extends AbstractKnaufProjectBean
{
  private final IProfileRecord m_point;

  private final KnaufProfileWrapper m_profile;

  private Double m_breite;

  private Double m_hoehe;

  private Double m_station;

  private Double m_bewuchsAx;

  private Double m_bewuchsAy;

  private Double m_bewuchsDp;

  private Double m_ksValue;

  private Double m_kstValue;

  private Double m_rechtswert;

  private Double m_hochwert;

  public KnaufSA30Bean( final KnaufProfileWrapper profile, final IProfileRecord point )
  {
    m_profile = profile;
    m_point = point;

    setStation( profile.getStation() * 1000.0 );
    setBreite( point.getBreite() );
    setHoehe( point.getHoehe() );

    setBewuchsAx( Doubles.firstNonNull( point.getBewuchsAx(), 0.0 ) );
    setBewuchsAy( Doubles.firstNonNull( point.getBewuchsAx(), 0.0 ) );
    setBewuchsDp( Doubles.firstNonNull( point.getBewuchsAx(), 0.0 ) );
    setKsValue( Doubles.firstNonNull( point.getKsValue(), 0.0 ) );
    setKstValue( Doubles.firstNonNull( point.getKstValue(), 0.0 ) );
    setRechtswert( Doubles.firstNonNull( point.getRechtswert(), 0.0 ) );
    setHochwert( Doubles.firstNonNull( point.getHochwert(), 0.0 ) );
  }

  @Override
  public Integer getSatzart( )
  {
    return 30;
  }

  /**
   * @return profile station in m
   */
  public Double getStation( )
  {
    return m_station;
  }

  public Double getBreite( )
  {
    return m_breite;
  }

  public Double getHoehe( )
  {
    return m_hoehe;
  }

  public void setBreite( final Double breite )
  {
    m_breite = breite;
  }

  public void setHoehe( final Double hoehe )
  {
    m_hoehe = hoehe;
  }

  public void setStation( final Double station )
  {
    m_station = station;
  }

  public String getKennziffer( )
  {
    return m_profile.getKennziffer( m_point );

  }

  public Object getAusuferungsgrenze( )
  {
    return m_profile.getAusuferungsgrenze( m_point );
  }

  /** @return String of length 9 */
  public String getDescription( )
  {
    final int index = m_point.indexOfComponent( IWspmPointProperties.POINT_PROPERTY_COMMENT );
    if( index < 0 )
      return StringUtils.repeat( " ", 9 ); //$NON-NLS-1$

    final Object value = m_point.getValue( index );
    if( Objects.isNull( value ) )
      return StringUtils.repeat( " ", 9 ); //$NON-NLS-1$

    String description = value.toString();
    if( description.length() == 9 )
      return description;
    else if( description.length() > 9 )
      return description.substring( 0, 8 );

    while( description.length() < 9 )
      description += " "; //$NON-NLS-1$ 

    return description;
  }

  public Double getBewuchsDp( )
  {
    return m_bewuchsDp;
  }

  public Double getBewuchsAx( )
  {
    return m_bewuchsAx;
  }

  public Double getBewuchsAy( )
  {
    return m_bewuchsAy;
  }

  public void setBewuchsDp( final Double bewuchsDp )
  {
    m_bewuchsDp = bewuchsDp;
  }

  public void setBewuchsAy( final Double bewuchsAy )
  {
    m_bewuchsAy = bewuchsAy;
  }

  public void setBewuchsAx( final Double bewuchsAx )
  {
    m_bewuchsAx = bewuchsAx;
  }

  public Double getKstValue( )
  {
    return m_kstValue;
  }

  public Double getKsValue( )
  {
    return m_ksValue;
  }

  public void setKsValue( final Double ksValue )
  {
    m_ksValue = ksValue;
  }

  public void setKstValue( final Double kstValue )
  {
    m_kstValue = kstValue;
  }

  public Double getRechtswert( )
  {
    return m_rechtswert;
  }

  public void setRechtswert( final Double rechtswert )
  {
    m_rechtswert = rechtswert;
  }

  public Double getHochwert( )
  {
    return m_hochwert;
  }

  public void setHochwert( final Double hochwert )
  {
    m_hochwert = hochwert;
  }

  public Double getRoughness( )
  {
    final KNAUF_FLIESSGESETZ fliessgesetz = m_profile.getFliessgesetz();
    if( KNAUF_FLIESSGESETZ.eManningStrickler.equals( fliessgesetz ) )
      return getKstValue();

    return getKsValue();
  }
}
