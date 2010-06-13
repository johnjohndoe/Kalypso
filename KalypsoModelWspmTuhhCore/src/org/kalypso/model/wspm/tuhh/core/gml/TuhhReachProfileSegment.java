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
package org.kalypso.model.wspm.tuhh.core.gml;

import java.math.BigDecimal;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * @author Gernot Belger
 */
public class TuhhReachProfileSegment extends Feature_Impl implements IWspmTuhhConstants
{
  public static final QName QNAME_PROFILEREACHSEGMENT = new QName( NS_WSPM_TUHH, "ProfileReachSegmentWspmTuhhSteadyState" ); //$NON-NLS-1$

  private static final QName PROPERTY_STATION = new QName( NS_WSPM_TUHH, "station" );//$NON-NLS-1$

  private static final QName PROPERTY_PROFILE_MEMBER = new QName( NS_WSPM_TUHH, "profileMember" ); //$NON-NLS-1$

  private static final QName PROPERTY_PROFILE_LOCATION = new QName( NS_WSPM_TUHH, "profileLocation" ); //$NON-NLS-1$

  public TuhhReachProfileSegment( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  public void setProfileMember( final IProfileFeature profileReference )
  {
    setProperty( new QName( NS_WSPM_TUHH, "profileMember" ), profileReference.getId() ); //$NON-NLS-1$

    invalidEnvelope();
  }

  // Commented out, because not used by the tuhh-model
  // public void setDistanceL( final double distanceL )
  // {
  // m_reachSegment.setProperty( new QName( NS_WSPM_TUHH, "distanceL" ), distanceL );
  // }
  //
  // public void setDistanceM( final double distanceM )
  // {
  // m_reachSegment.setProperty( new QName( NS_WSPM_TUHH, "distanceM" ), distanceM );
  // }
  //
  // public void setDistanceR( final double distanceR )
  // {
  // m_reachSegment.setProperty( new QName( NS_WSPM_TUHH, "distanceR" ), distanceR );
  // }

  public BigDecimal getStation( )
  {
    return getProperty( PROPERTY_STATION, BigDecimal.class );
  }

  public IProfileFeature getProfileMember( )
  {
    final String href = (String) getProperty( PROPERTY_PROFILE_MEMBER );
    final GMLWorkspace workspace = getWorkspace();
    final Feature feature = workspace == null ? null : workspace.getFeature( href );

    if( feature instanceof IProfileFeature )
      return (IProfileFeature) feature;

    return null;

  }

  public void setStation( final double station )
  {
    final BigDecimal bigStation = ProfilUtil.stationToBigDecimal( station );
    setProperty( PROPERTY_STATION, bigStation );
  }

  public GM_Curve getGeometry( )
  {
    return getProperty( PROPERTY_PROFILE_LOCATION, GM_Curve.class );
  }

  /**
   * @see org.kalypsodeegree_impl.model.feature.Feature_Impl#getAdapter(java.lang.Class)
   */
  @Override
  public Object getAdapter( @SuppressWarnings("rawtypes") final Class adapter )
  {
    if( IProfileFeature.class == adapter )
      return getProfileMember();

    return super.getAdapter( adapter );
  }
}
