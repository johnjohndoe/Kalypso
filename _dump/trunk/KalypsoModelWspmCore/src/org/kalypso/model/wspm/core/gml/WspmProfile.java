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
package org.kalypso.model.wspm.core.gml;

import java.math.BigDecimal;
import java.math.RoundingMode;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.javax.xml.namespace.QNameUtilities;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCorePlugin;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;

/**
 * @author Gernot Belger
 */
public class WspmProfile extends AbstractFeatureBinder implements IFeatureWrapper2
{
  public final static QName QNAME_PROFILE = new QName( IWspmConstants.NS_WSPMPROF, "Profile" );

  public static final QName QNAME_STATION = new QName( IWspmConstants.NS_WSPMPROF, "station" );

  public static final QName QNAME_TYPE = new QName( IWspmConstants.NS_WSPMPROF, "type" );

  public final static QName QNAME_LINE = new QName( IWspmConstants.NS_WSPMPROF, "profileLocation" );

  public static final QName QNAME_SRS = new QName( IWspmConstants.NS_WSPMPROF, "srsName" );

  /**
   * The scale (i.e. fraction digits) for station values.
   * 
   * @see BigDecimal
   */
  public static final int STATION_SCALE = 4;

  public WspmProfile( final Feature feature )
  {
    super( feature, QNAME_PROFILE );
  }

  public WspmWaterBody getWater( )
  {
    final Feature parent = getFeature().getParent();
    if( parent != null && QNameUtilities.equals( parent.getFeatureType().getQName(), IWspmConstants.NS_WSPM, "WaterBody" ) )
      return new WspmWaterBody( parent );

    return null;
  }

  // TODO: this should directly return the BigDecimal
  public double getStation( )
  {
    final BigDecimal profileStation = ProfileFeatureFactory.getProfileStation( getFeature() );

    return profileStation == null ? Double.NaN : profileStation.doubleValue();
  }

  public void setStation( final double station )
  {
    final BigDecimal bigStation = stationToBigDecimal( station );
    ProfileFeatureFactory.setProfileStation( getFeature(), bigStation );
  }

  public IProfil getProfil( )
  {
    try
    {
      return ProfileFeatureFactory.toProfile( getFeature() );
    }
    catch( final Exception e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoModelWspmCorePlugin.getDefault().getLog().log( status );

      return null;
    }
  }

  public GM_Curve getLine( )
  {
    return (GM_Curve) getFeature().getProperty( QNAME_LINE );
  }

  public String getSrsName( )
  {
    return (String) getFeature().getProperty( QNAME_SRS );
  }

  public void setSrsName( final String srsName )
  {
    getFeature().setProperty( QNAME_SRS, srsName );
  }

  /**
   * Converts a double valued station into a BigDecimal with a scale of {@value #STATION_SCALE}.
   * 
   * @see #STATION_SCALE
   */
  public static BigDecimal stationToBigDecimal( final double station )
  {
    return new BigDecimal( station ).setScale( STATION_SCALE, RoundingMode.HALF_UP );
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    return "" + getStation();
  }
}
