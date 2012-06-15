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
package org.kalypso.model.wspm.tuhh.ui.featureview;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.viewers.ICheckStateProvider;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;

/**
 * @author Gernot Belger
 */
public class ReachSegmentCheckStateProvider implements ICheckStateProvider
{
  private final TuhhReach m_reach;

  final Set<IProfileFeature> m_profileHash = new HashSet<IProfileFeature>();

  public ReachSegmentCheckStateProvider( final TuhhReach reach )
  {
    Assert.isNotNull( reach );
    
    m_reach = reach;
    // TODO: we probably also need to listen to model events in order to clear the hash
    m_profileHash.clear();
    
    /* Performance: Hash the profile for quicker access later */
      final TuhhReachProfileSegment[] reachProfileSegments = m_reach.getReachProfileSegments();
      for( TuhhReachProfileSegment tuhhReachProfileSegment : reachProfileSegments )
      {
        IProfileFeature profileMember = tuhhReachProfileSegment.getProfileMember();
        m_profileHash.add( profileMember );
      }
  }
  
  /**
   * @see org.eclipse.jface.viewers.ICheckStateProvider#isChecked(java.lang.Object)
   */
  @Override
  public boolean isChecked( Object element )
  {
    final IProfileFeature profile = (IProfileFeature) element;
    return reachContainsProfile( profile );
  }

  private boolean reachContainsProfile( IProfileFeature profile )
  {
    return m_profileHash.contains( profile );
  }

  /**
   * @see org.eclipse.jface.viewers.ICheckStateProvider#isGrayed(java.lang.Object)
   */
  @Override
  public boolean isGrayed( Object element )
  {
    // TODO: maybe if profile with same station was already checked?
    return false;
  }
}
