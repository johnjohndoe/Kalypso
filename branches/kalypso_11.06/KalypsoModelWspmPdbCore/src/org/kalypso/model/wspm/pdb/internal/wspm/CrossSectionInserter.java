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

import java.util.Collection;
import java.util.IdentityHashMap;
import java.util.Map;

import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.pdb.db.constants.WaterBodyConstants.STATIONING_DIRECTION;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;

/**
 * Inserts {@link org.kalypso.model.wspm.pdb.db.mapping.CrossSection}s into a
 * {@link org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject}.
 * 
 * @author Gernot Belger
 */
public class CrossSectionInserter
{
  private final TuhhWspmProject m_project;

  private final Map<WspmWaterBody, TuhhReach> m_reaches = new IdentityHashMap<WspmWaterBody, TuhhReach>();

  public CrossSectionInserter( final TuhhWspmProject project )
  {
    m_project = project;
  }

  public void insert( final CrossSection section ) throws Exception
  {
    final WspmWaterBody waterBody = insertWaterBody( m_project, section.getWaterBody() );
    final TuhhReach reach = insertReach( section, waterBody );

    final IProfileFeature profile = insertProfile( section, waterBody );

    insertCrossSection( profile, reach );
  }

  private IProfileFeature insertProfile( final CrossSection section, final WspmWaterBody waterBody )
  {
    final IProfileFeature newProfile = waterBody.createNewProfile();
    newProfile.setProfileType( IWspmTuhhConstants.PROFIL_TYPE_PASCHE );

    final Geometry line = section.getLine();
    final String srs = line == null ? null : JTSAdapter.toSrs( line.getSRID() );
    newProfile.setSrsName( srs );

    final IProfil profile = newProfile.getProfil();
    final CrossSectionConverter converter = new CrossSectionConverter( section, profile );
    converter.execute();
    ProfileFeatureFactory.toFeature( profile, newProfile );

    return newProfile;
  }

  private void insertCrossSection( final IProfileFeature profile, final TuhhReach reach )
  {
    /* Just add, nothing else to do */
    reach.createProfileSegment( profile, profile.getStation() );
  }

  static WspmWaterBody insertWaterBody( final TuhhWspmProject project, final WaterBody waterBody ) throws Exception
  {
    // FIXME: always update local wb with properties from database

    final String gkn = waterBody.getName();

    /* If water body with same code exists, we will use it */
    final WspmWaterBody wspmWaterBody = project.findWaterByRefNr( gkn );
    if( wspmWaterBody != null )
      return wspmWaterBody;

    final String name = waterBody.getLabel();

    final WspmWaterBody newWspmWaterBody = project.createWaterBody( name, true );
    newWspmWaterBody.setRefNr( gkn );
    newWspmWaterBody.setDescription( waterBody.getDescription() );

    final STATIONING_DIRECTION directionOfStationing = waterBody.getDirectionOfStationing();
    final boolean isDirectionUpstreams = directionOfStationing == WaterBody.STATIONING_DIRECTION.upstream;
    newWspmWaterBody.setDirectionUpstreams( isDirectionUpstreams );

    final GM_Curve centerLine = (GM_Curve) JTSAdapter.wrapWithSrid( waterBody.getRiverlineAsLine() );

    final String kalypsoSRS = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    final GM_Curve transformedCenterline = (GM_Curve) centerLine.transform( kalypsoSRS );
    newWspmWaterBody.setCenterLine( transformedCenterline );

    return newWspmWaterBody;
  }

  private TuhhReach insertReach( final CrossSection section, final WspmWaterBody waterBody ) throws GMLSchemaException
  {
    // REMARK: we always insert exactly one new reach per checkout, regardless if
    // a reach with the same name already exists.
    if( m_reaches.containsKey( waterBody ) )
      return m_reaches.get( waterBody );

    final State state = section.getState();
    final String name = state.getName();

    final TuhhReach newReach = TuhhWspmProject.createNewReachForWaterBody( waterBody );
    // TODO: maybe make name unique, if already exists (i.e. reach_2)
    newReach.setName( name );
    newReach.setDescription( state.getDescription() );

    m_reaches.put( waterBody, newReach );

    return newReach;
  }

  public TuhhReach[] getInsertedReaches( )
  {
    final Collection<TuhhReach> values = m_reaches.values();
    return values.toArray( new TuhhReach[values.size()] );
  }
}