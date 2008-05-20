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
package org.kalypso.model.wspm.tuhh.core.gml;

import java.math.BigDecimal;

import javax.xml.namespace.QName;

import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;

/**
 * @author Gernot Belger
 */
public class TuhhReachProfileSegment extends AbstractFeatureBinder implements IWspmTuhhConstants
{
  public static final QName QNAME_PROFILEREACHSEGMENT = new QName( NS_WSPM_TUHH, "ProfileReachSegmentWspmTuhhSteadyState" );

  public TuhhReachProfileSegment( final Feature reachSegment )
  {
    super( reachSegment, QNAME_PROFILEREACHSEGMENT );
  }

  public void setProfileMember( final WspmProfile profileReference )
  {
    getFeature().setProperty( new QName( NS_WSPM_TUHH, "profileMember" ), profileReference.getFeature().getId() );

    getFeature().invalidEnvelope();
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
    return (BigDecimal) getFeature().getProperty( new QName( NS_WSPM_TUHH, "station" ) );
  }

  public WspmProfile getProfileMember( )
  {
    final String href = (String) getFeature().getProperty( new QName( NS_WSPM_TUHH, "profileMember" ) );
    final GMLWorkspace workspace = getFeature().getWorkspace();
    final Feature feature = workspace == null ? null : workspace.getFeature( href );
    return feature == null ? null : new WspmProfile( feature );
  }

  public void setStation( final double station )
  {
    final BigDecimal bigStation = WspmProfile.stationToBigDecimal( station );
    getFeature().setProperty( new QName( NS_WSPM_TUHH, "station" ), bigStation );
  }

  public GM_Curve getGeometry( )
  {
    final Object geomProp = getFeature().getProperty( new QName( NS_WSPM_TUHH, "profileLocation" ) );
    if( geomProp instanceof GM_Curve )
    {
      return (GM_Curve) geomProp;
    }
    else
    {
      return null;
    }
  }

  public GM_Object getDefaultGeometry( )
  {
    return getFeature().getDefaultGeometryProperty();
  }
}
