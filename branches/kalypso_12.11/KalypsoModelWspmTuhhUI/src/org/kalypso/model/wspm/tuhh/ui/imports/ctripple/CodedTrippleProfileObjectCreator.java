/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.tuhh.ui.imports.ctripple;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecord;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecords;
import org.kalypso.model.wspm.core.profil.IProfilePointPropertyProvider;
import org.kalypso.model.wspm.core.profil.ProfileObjectFactory;
import org.kalypso.model.wspm.core.util.WspmProfileHelper;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.ctripple.CodedTrippleHorizonMapper;
import org.kalypso.model.wspm.tuhh.core.ctripple.CodedTrippleProfile;
import org.kalypso.model.wspm.tuhh.core.ctripple.CodedTrippleProfileHorizon;
import org.kalypso.model.wspm.tuhh.core.ctripple.CodedTrippleProfilePoint;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingUtilities;
import org.kalypso.transformation.transformer.GeoTransformerException;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.PrecisionModel;

/**
 * @author Holger Albert
 */
public class CodedTrippleProfileObjectCreator
{
  private final CodedTrippleHorizonMapper m_mapper;

  private final CodedTrippleProfile m_profile;

  private final IProfileFeature m_profileFeature;

  public CodedTrippleProfileObjectCreator( final CodedTrippleHorizonMapper mapper, final CodedTrippleProfile profile, final IProfileFeature profileFeature )
  {
    m_mapper = mapper;
    m_profile = profile;
    m_profileFeature = profileFeature;
  }

  public void createProfileObjects( ) throws GM_Exception, GeoTransformerException
  {
    final List<IProfileObject> profileObjects = new ArrayList<>();

    final CodedTrippleProfileHorizon[] horizons = m_profile.getProfileHorizons();
    for( final CodedTrippleProfileHorizon horizon : horizons )
    {
      /* Ignore the base horizon. */
      final String horizonId = horizon.getHorizonId();
      if( CodedTrippleProfile.PROFILE_HORIZON_ID.equals( horizonId ) )
        continue;

      final IProfileObject profileObject = getProfileObject( horizon );
      final IProfileObjectRecords records = profileObject.getRecords();

      final CodedTrippleProfilePoint[] points = horizon.getProfilePoints();
      for( int i = 0; i < points.length; i++ )
      {
        final CodedTrippleProfilePoint point = points[i];
        addPoint( i, records, point );
      }

      final String description = m_mapper.getHorizonIdDescription( horizonId );
      profileObject.setDescription( description );

      profileObjects.add( profileObject );
    }

    if( profileObjects.size() > 0 )
    {
      /* Add the profile objects to the profile. */
      /* Add all in one step, to reduce events. */
      final IProfile profile = m_profileFeature.getProfile();
      profile.addProfileObjects( profileObjects.toArray( new IProfileObject[] {} ) );

      /* Search one bridge and one ok without id and repair. */
      final IProfileObject[] pos = profile.getProfileObjects();
      BuildingUtilities.repairBridges( pos );

      /* Update the components in the profile. */
      final IProfilePointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( IWspmTuhhConstants.PROFIL_TYPE_PASCHE );
      for( final IProfileObject po : pos )
        BuildingUtilities.updateComponents( po, profile, provider );
    }
  }

  private IProfileObject getProfileObject( final CodedTrippleProfileHorizon horizon )
  {
    final String horizonId = horizon.getHorizonId();
    final String partId = m_mapper.getPartId( horizonId );
    return ProfileObjectFactory.createProfileObject( m_profileFeature.getProfile(), partId );
  }

  private void addPoint( final int index, final IProfileObjectRecords records, final CodedTrippleProfilePoint point ) throws GM_Exception, GeoTransformerException
  {
    final IProfileObjectRecord record = records.addNewRecord();

    final String id = String.format( "%d", index ); //$NON-NLS-1$
    final String comment = m_mapper.getCodeDescription( point.getCode() );
    final double rechtswert = point.getEasting();
    final double hochwert = point.getNorthing();

    final GeometryFactory geometryFactory = new GeometryFactory( new PrecisionModel(), JTSAdapter.toSrid( m_profileFeature.getSrsName() ) );
    final Point jtsPoint = geometryFactory.createPoint( new Coordinate( rechtswert, hochwert ) );

    // FIXME Bad for points lying far away from the profile.
    final double breite = WspmProfileHelper.getWidthPosition( jtsPoint, m_profileFeature.getProfile() );
    final double hoehe = point.getHeight();
    final String code = point.getCode();

    record.setId( id );
    record.setComment( comment );
    record.setBreite( breite );
    record.setHoehe( hoehe );
    record.setRechtswert( rechtswert );
    record.setHochwert( hochwert );
    record.setCode( code );
  }
}