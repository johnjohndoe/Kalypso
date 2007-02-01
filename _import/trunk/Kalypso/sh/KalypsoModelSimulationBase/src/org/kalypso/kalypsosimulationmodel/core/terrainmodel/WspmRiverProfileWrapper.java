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
package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author Gernot Belger
 */
public class WspmRiverProfileWrapper extends RiverProfile implements IWspmRiverProfileWrapper
{
  public WspmRiverProfileWrapper( final Feature feature )
  {
    super( feature, IWspmRiverProfileWrapper.QNAME );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IWspmRiverProfileWrapper#getWspmRiverProfile()
   */
  public WspmProfile getWspmRiverProfile( )
  {
    final Feature feature = getFeature();
    final Object value = feature.getProperty( IWspmRiverProfileWrapper.QNAME_PROP_WSPM_RIVER_PROFILE );
    final Feature profileFeature = FeatureHelper.getFeature( feature.getWorkspace(), value );

    return profileFeature == null ? null : new WspmProfile( feature );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IWspmRiverProfileWrapper#setWspmRiverProfile(org.kalypso.model.wspm.core.gml.WspmProfile)
   */
  public void setWspmRiverProfile( final WspmProfile wspmProfile )
  {
    if( wspmProfile.getFeature().getParent() != getFeature() )
      throw new IllegalArgumentException( "Given profile is not child of this wrapper." );

    getFeature().setProperty( IWspmRiverProfileWrapper.QNAME_PROP_WSPM_RIVER_PROFILE, wspmProfile.getFeature() );
  }

}
