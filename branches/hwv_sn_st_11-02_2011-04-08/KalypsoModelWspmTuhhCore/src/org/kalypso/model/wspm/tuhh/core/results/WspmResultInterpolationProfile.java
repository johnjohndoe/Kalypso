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
package org.kalypso.model.wspm.tuhh.core.results;

import java.math.BigDecimal;

import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.util.ProfileInterpolation;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * @author Gernot Belger
 */
public class WspmResultInterpolationProfile
{
  private final BigDecimal m_previousStation;

  private final BigDecimal m_nextStation;

  private final BigDecimal m_interpolatedStation;

  public WspmResultInterpolationProfile( final BigDecimal previousStation, final BigDecimal nextStation, final BigDecimal interpolatedStation )
  {
    m_previousStation = previousStation;
    m_nextStation = nextStation;
    m_interpolatedStation = interpolatedStation;
  }

  public IProfileFeature createInterpolatedProfile( final TuhhReach reach, final boolean onlyRiverChannel )
  {
    final IProfileFeature nextProfile = reach.findProfile( m_nextStation );
    final IProfileFeature previousProfile = reach.findProfile( m_previousStation );

    final ProfileInterpolation interpolation = new ProfileInterpolation( previousProfile.getProfil(), nextProfile.getProfil(), onlyRiverChannel );
    final IProfil newProfile = interpolation.interpolate( m_interpolatedStation, IWspmTuhhConstants.PROFIL_TYPE_PASCHE );
    final String name = String.format( Messages.getString("WspmResultInterpolationProfile_0"), m_previousStation, m_nextStation ); //$NON-NLS-1$
    newProfile.setName( name );

    final IProfileFeature profileFeature = createProfileFeature();
    ProfileFeatureFactory.toFeature( newProfile, profileFeature );

    return profileFeature;
  }

  private IProfileFeature createProfileFeature( )
  {
    try
    {
      final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( IProfileFeature.QN_PROFILE, null, GmlSerializer.DEFAULT_FACTORY );
      return (IProfileFeature) workspace.getRootFeature();
    }
    catch( final GMLSchemaException e )
    {
      // will not happen
      e.printStackTrace();
      return null;
    }
  }

}
