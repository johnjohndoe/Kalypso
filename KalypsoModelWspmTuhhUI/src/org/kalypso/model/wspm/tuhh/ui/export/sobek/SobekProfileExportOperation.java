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
package org.kalypso.model.wspm.tuhh.ui.export.sobek;

import java.math.BigDecimal;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.java.lang.Doubles;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.IProfilePointMarker;
import org.kalypso.model.wspm.core.profil.sobek.SobekModel;
import org.kalypso.model.wspm.core.profil.sobek.profiles.SobekFrictionDat;
import org.kalypso.model.wspm.core.profil.sobek.profiles.SobekNetworkD12Point;
import org.kalypso.model.wspm.core.profil.sobek.profiles.SobekProfile;
import org.kalypso.model.wspm.core.profil.sobek.profiles.SobekProfileDat;
import org.kalypso.model.wspm.core.profil.sobek.profiles.SobekProfileDef;
import org.kalypso.model.wspm.core.profil.sobek.profiles.SobekProfileDefYZTable;
import org.kalypso.model.wspm.core.profil.sobek.profiles.SobekYZPoint;
import org.kalypso.model.wspm.core.profil.sobek.struct.SobekStruct;
import org.kalypso.model.wspm.core.profil.sobek.struct.SobekStructDat;
import org.kalypso.model.wspm.core.profil.sobek.struct.SobekStructDef;
import org.kalypso.model.wspm.core.profil.visitors.FindMinMaxVisitor;
import org.kalypso.model.wspm.core.profil.visitors.ProfileVisitors;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.flowzones.IFlowZoneType;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Gernot Belger
 */
public class SobekProfileExportOperation implements ISobekProfileExportOperation
{
  enum NormalExportType
  {
    /** just create a normal profile, no buildings present */
    normal,
    /** create interpolated profiles at the entry and exit point of the bridge */
    interpolateAdjacent,
    /** do nothing */
    none
  }

  private final SobekExportInfo m_info;

  private final SobekModel m_sobekModel;

  public SobekProfileExportOperation( final SobekExportInfo info, final SobekModel sobekModel )
  {
    m_info = info;
    m_sobekModel = sobekModel;
  }

  @Override
  public String getLabel( )
  {
    return "Gathering export data";
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final IStatusCollector log = new StatusCollector( KalypsoModelWspmTuhhUIPlugin.getID() );

    final IProfileFeature[] profiles = m_info.getProfiles();
    for( final IProfileFeature profileFeature : profiles )
    {
      final IStatus status = exportProfile( profileFeature );
      log.add( status );
    }

    return log.asMultiStatus( getLabel() );
  }

  private IStatus exportProfile( final IProfileFeature profileFeature )
  {
    final IStatusCollector log = new StatusCollector( KalypsoModelWspmTuhhUIPlugin.getID() );

    final String id = m_info.getID( profileFeature );
    final String profileName = m_info.getName( profileFeature );

    /* profile dat */
    final IProfile profile = profileFeature.getProfile();

    final IProfileRecord[] points = getPointsToExport( profile, log );
    final IProfileObject[] objects = profile.getProfileObjects();

    final NormalExportType normalExport = exportProfileObjects( profile, id, profileName, points, objects );
    switch( normalExport )
    {
      case normal:
        exportNormalProfile( log, id, profileName, profile, points );
        break;

      case interpolateAdjacent:
        // FIXME: writeAdjacentInterpolatedProfiles( id, profileName, points, profileFeature, profile );
        break;

      case none:
        break;
    }

    return log.asMultiStatus( String.format( "%s", profileFeature.getBigStation() ) ); //$NON-NLS-1$
  }

  private void exportNormalProfile( final IStatusCollector log, final String id, final String profileName, final IProfile profile, final IProfileRecord[] points )
  {
    if( points.length < 2 )
    {
      log.add( IStatus.INFO, "skipped, profile has less than 2 points" );
    }
    else
    {
      final SobekProfileDat profileDat = createProfileDat( id, points );
      final SobekProfileDef profileDef = createProfileDef( id, profileName, points );

      final SobekFrictionDat frictionDat = createFrictionDat( id, profileName, profile );
      final SobekNetworkD12Point networkPoint = createNetworkPoint( id, profileName, profile, points );

      final SobekProfile sobekProfile = new SobekProfile( profileDat, profileDef, frictionDat, networkPoint );
      m_sobekModel.addProfile( sobekProfile );
    }
  }

  private NormalExportType exportProfileObjects( final IProfile profile, final String id, final String profileName, final IProfileRecord[] points, final IProfileObject[] objects )
  {
    if( !m_info.getExportBuildings() )
      return NormalExportType.normal;

    // switch over building type
    int reallyExportedObjectCount = 0;
    int buildingCount = 0;

    for( final IProfileObject profileObject : objects )
    {
      final String countSuffix = reallyExportedObjectCount == 0 ? StringUtils.EMPTY : "_" + reallyExportedObjectCount; //$NON-NLS-1$
      final String idSuffix = m_info.getIdSuffix();
      final String structId = id + idSuffix + countSuffix;

      final SobekStruct struct = createStruct( profile, points, profileObject, profileName, structId );
      if( struct != null )
      {
        reallyExportedObjectCount++;

        m_sobekModel.addStruct( struct );
        buildingCount++;
      }
    }

    /* determine what to do next */
    if( buildingCount == 0 )
      return NormalExportType.normal;

    /* if we have at least one building, we export interpolated profiles upstreams and downstreams */
    // FIXME: should be optional
    return NormalExportType.normal;
//    return NormalExportType.interpolateAdjacent;
  }

  private SobekStruct createStruct( final IProfile profile, final IProfileRecord[] points, final IProfileObject profileObject, final String profileName, final String structId )
  {
    if( profileObject instanceof BuildingBruecke )
    {
      final BuildingBruecke bridge = (BuildingBruecke)profileObject;

      final SobekStructDat structDat = new SobekStructDat( structId, profileName, structId );

      final Double widthValue = bridge.getBreite();
      final double width = widthValue == null ? 0.0 : widthValue.doubleValue();

      final FindMinMaxVisitor visitor = new FindMinMaxVisitor( IWspmConstants.POINT_PROPERTY_HOEHE );
      ProfileVisitors.visit( visitor, points );
      final IProfileRecord minPoint = visitor.getMinimum();
      if( minPoint == null )
        // TODO: log
        return null;

      final Double height = minPoint.getHoehe();

      final String srsName = profile.getSrsName();

      final Double easting = minPoint.getRechtswert();
      final Double northing = minPoint.getHochwert();

      final GM_Point location;
      if( Doubles.isNullOrInfinite( easting, northing ) )
        location = null;
      else
        location = GeometryFactory.createGM_Point( easting, northing, srsName );

      final SobekProfileDefBuildingExporter profileDefExporter = new SobekProfileDefBuildingExporter( m_info.getTargetDir() );
      final SobekProfileDef profileDef = profileDefExporter.export( structId, profileName, profile, points );

      final SobekStructDef structDef = SobekStructDef.createAbutmentBridge( structId, profileName, width, height, structId, profileDef );

      return new SobekStruct( structDat, structDef, location );
    }

    // TODO: implement other types
    return null;
  }

  private SobekProfileDef createProfileDef( final String id, final String profileName, final IProfileRecord[] points )
  {
    final int st = 0;
    final BigDecimal sw = new BigDecimal( 0 );

    final SobekProfileDefYZTable data = new SobekProfileDefYZTable( st, sw );

    for( final IProfileRecord point : points )
    {
      final Double width = point.getBreite();
      final Double height = point.getHoehe();

      final BigDecimal py = new BigDecimal( width.toString() ).setScale( 4, BigDecimal.ROUND_HALF_UP );
      final BigDecimal pz = new BigDecimal( height.toString() ).setScale( 4, BigDecimal.ROUND_HALF_UP );

      final SobekYZPoint yzPoint = new SobekYZPoint( py, pz );
      data.addPoint( yzPoint );
    }

    final String name = profileName;

    return new SobekProfileDef( id, name, data );
  }

  private SobekProfileDat createProfileDat( final String id, final IProfileRecord[] points )
  {
    final String di = id;
    final BigDecimal rl = new BigDecimal( 0 );
    final BigDecimal ll = null;

    final Double startHeight = points[0].getHoehe();
    final Double endHeight = points[points.length - 1].getHoehe();

    final BigDecimal rs = new BigDecimal( startHeight.toString() ).setScale( 2, BigDecimal.ROUND_HALF_UP );
    final BigDecimal ls = new BigDecimal( endHeight.toString() ).setScale( 2, BigDecimal.ROUND_HALF_UP );

    final SobekProfileDat profileDat = new SobekProfileDat( id, di, rl, ll, rs, ls );
    return profileDat;
  }

  private SobekFrictionDat createFrictionDat( final String id, final String name, final IProfile profile )
  {
    final IFlowZoneType[] zoneTypes = m_info.getRoughnessZoneTypes();
    final boolean preferRoughnessClasses = m_info.getPreferRoughnessClasses();
    final String roughnessId = m_info.getRoughnessID();

    final SobekFrictionDatExporter exporter = new SobekFrictionDatExporter( zoneTypes, roughnessId, preferRoughnessClasses );
    return exporter.export( id, name, profile );
  }

  private SobekNetworkD12Point createNetworkPoint( final String id, final String profileName, final IProfile profile, final IProfileRecord[] points )
  {
    final FindMinMaxVisitor visitor = new FindMinMaxVisitor( IWspmConstants.POINT_PROPERTY_HOEHE );
    ProfileVisitors.visit( visitor, points );
    final IProfileRecord minPoint = visitor.getMinimum();
    if( minPoint == null )
      return null;

    final String srsName = profile.getSrsName();

    final Double easting = minPoint.getRechtswert();
    final Double northing = minPoint.getHochwert();

    if( Doubles.isNullOrInfinite( easting, northing ) )
      return null;

    final BigDecimal px = new BigDecimal( easting ).setScale( 4, BigDecimal.ROUND_HALF_DOWN );
    final BigDecimal py = new BigDecimal( northing ).setScale( 4, BigDecimal.ROUND_HALF_DOWN );

    return new SobekNetworkD12Point( id, profileName, null, null, px, py, 0, 0, srsName );
  }

  private IProfileRecord[] getPointsToExport( final IProfile profil, final IStatusCollector log )
  {
    final String flowZone = m_info.getFlowZone();
    if( StringUtils.isBlank( flowZone ) )
      return profil.getPoints();

    final IComponent markerComponent = ComponentUtilities.getFeatureComponent( flowZone );
    final String unknownLabel = String.format( Messages.getString( "SobekDefExportOperation.1" ), flowZone ); //$NON-NLS-1$
    final String markerLabel = markerComponent == null ? unknownLabel : ComponentUtilities.getComponentLabel( markerComponent );

    final IProfilePointMarker[] markers = profil.getPointMarkerFor( flowZone );
    if( markers.length < 2 )
    {
      final String message = String.format( Messages.getString( "SobekDefExportOperation_2" ), markerLabel, profil.getStation(), profil.getName() ); //$NON-NLS-1$
      log.add( IStatus.WARNING, message );
      return profil.getPoints();
    }

    final IProfileRecord startPoint = markers[0].getPoint();
    final IProfileRecord endPoint = markers[1].getPoint();

    final int startIndex = startPoint.getIndex();
    final int endIndex = endPoint.getIndex();

    return profil.getPoints( startIndex, endIndex );
  }
}