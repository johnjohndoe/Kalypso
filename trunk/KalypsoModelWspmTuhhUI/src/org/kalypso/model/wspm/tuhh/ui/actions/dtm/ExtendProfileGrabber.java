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
package org.kalypso.model.wspm.tuhh.ui.actions.dtm;

import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * The currently grabbed profile and associated information.
 * 
 * @author Gernot Belger
 */
public class ExtendProfileGrabber
{
  private final IKalypsoFeatureTheme m_theme;

  private final double m_snapRadius;

  private IProfileFeature m_grabbedProfile;

  private TuhhReach m_reach;

  private TuhhReachProfileSegment m_reachSegment;

  public ExtendProfileGrabber( final IKalypsoFeatureTheme theme, final double snapRadius )
  {
    m_theme = theme;
    m_snapRadius = snapRadius;
  }

  private FeatureList getProfileFeatures( )
  {
    return m_theme.getFeatureList();
  }

  public boolean doGrab( final GM_Point pos )
  {
    m_grabbedProfile = findProfile( pos );
    return m_grabbedProfile != null;
  }

  private IProfileFeature findProfile( final GM_Point pos )
  {
    final FeatureList profileFeatures = getProfileFeatures();
    final IFeatureType targetFeatureType = profileFeatures.getPropertyType().getTargetFeatureType();
    if( GMLSchemaUtilities.substitutes( targetFeatureType, IProfileFeature.FEATURE_PROFILE ) )
      return (IProfileFeature) GeometryUtilities.findNearestFeature( pos, m_snapRadius, profileFeatures, IProfileFeature.PROPERTY_LINE );

    final TuhhReachProfileSegment nearest = (TuhhReachProfileSegment) GeometryUtilities.findNearestFeature( pos, m_snapRadius, profileFeatures, TuhhReachProfileSegment.PROPERTY_PROFILE_LOCATION );
    if( nearest == null )
      return null;

    m_reach = (TuhhReach) nearest.getOwner();
    m_reachSegment = nearest;

    return nearest.getProfileMember();
  }

  /**
   * The element that holds the profile (e.g. the ReachSegment)
   */
  public Feature getProfileOwner( )
  {
    return m_reachSegment;
  }

  public IProfileFeature getProfile( )
  {
    return m_grabbedProfile;
  }

  public CommandableWorkspace getWorkspace( )
  {
    return m_theme.getWorkspace();
  }

  public TuhhReach getReach( )
  {
    return m_reach;
  }
}