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
package org.kalypso.model.wspm.pdb.internal.wspm;

import java.util.HashMap;
import java.util.Map;

import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmReach;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * Index for profile references from reaches.
 *
 * @author Gernot Belger
 */
public class ReachProfileReferencer
{
  private final Map<IProfileFeature, TuhhReach> m_index = new HashMap<>();

  public ReachProfileReferencer( final WspmWaterBody waterBody )
  {
    final IFeatureBindingCollection<WspmReach> reaches = waterBody.getReaches();
    for( final WspmReach reach : reaches )
      buildIndex( reach );
  }

  private void buildIndex( final WspmReach reach )
  {
    if( !(reach instanceof TuhhReach) )
      return;

    final TuhhReach tuhhReach = (TuhhReach) reach;
    final TuhhReachProfileSegment[] segments = tuhhReach.getReachProfileSegments();
    for( final TuhhReachProfileSegment segment : segments )
    {
      final IProfileFeature profileMember = segment.getProfileMember();
      // REMARK: normally, we could index all reaches, that reference a profile,
      // but here, we only need to know that at least one reach exists.
      m_index.put( profileMember, tuhhReach );
    }
  }

  public boolean isReferenced( final IProfileFeature profile )
  {
    return m_index.containsKey( profile );
  }
}