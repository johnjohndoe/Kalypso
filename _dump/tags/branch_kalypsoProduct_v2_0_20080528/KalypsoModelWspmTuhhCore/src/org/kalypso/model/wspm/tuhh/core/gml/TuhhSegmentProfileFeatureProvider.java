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

import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.model.wspm.core.gml.IProfileFeatureProvider;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 *
 */
public class TuhhSegmentProfileFeatureProvider implements IProfileFeatureProvider
{
  /**
   * @see org.kalypso.model.wspm.core.gml.IProfileFeatureProvider#getProfile(org.kalypsodeegree.model.feature.Feature)
   */
  public WspmProfile getProfile( final Feature feature )
  {
    if( GMLSchemaUtilities.substitutes( feature.getFeatureType(), TuhhReach.QNAME_TUHH_REACH ) )
    {
      final TuhhReach reach = new TuhhReach( feature );
      final TuhhReachProfileSegment[] reachProfileSegments = reach.getReachProfileSegments();
      if( reachProfileSegments.length > 0 )
        return reachProfileSegments[0].getProfileMember();
    }
    
    if( GMLSchemaUtilities.substitutes( feature.getFeatureType(), TuhhReachProfileSegment.QNAME_PROFILEREACHSEGMENT ) )
    {
      final TuhhReachProfileSegment segment = new TuhhReachProfileSegment( feature );
      return segment.getProfileMember();
    }

    return null;
  }

}
