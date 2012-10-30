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
package org.kalypso.model.wspm.pdb.internal.waterlevel2d;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

import org.apache.commons.lang3.Range;
import org.eclipse.core.runtime.IStatus;
import org.hibernatespatial.mgeom.MGeometryException;
import org.hibernatespatial.mgeom.MLineString;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecord;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecords;
import org.kalypso.model.wspm.core.profil.impl.GenericProfileHorizon;
import org.kalypso.model.wspm.core.util.JTSWaterlevelIntersector;
import org.kalypso.model.wspm.pdb.db.mapping.WaterlevelFixation;
import org.kalypso.model.wspm.pdb.gaf.GafPointCode;
import org.kalypso.model.wspm.pdb.gaf.IGafConstants;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.opengis.geometry.MismatchedDimensionException;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateList;
import com.vividsolutions.jts.geom.LineString;

/**
 * Helper class that holds all the {@link ProjectedWaterlevel}s.
 * 
 * @author Gernot Belger
 */
public class ProjectedWaterlevels
{
  private final IStatusCollector m_log = new StatusCollector( WspmPdbCorePlugin.PLUGIN_ID );

  private final ProjectedWaterlevel[] m_waterlevels;

  private final String m_eventName;

  private final BigDecimal m_station;

  private final MLineString m_profileLine;

  public ProjectedWaterlevels( final String eventName, final BigDecimal station, final MLineString profileLine, final WaterlevelFixation[] waterlevels )
  {
    m_eventName = eventName;
    m_station = station;
    m_profileLine = profileLine;

    m_waterlevels = new ProjectedWaterlevel[waterlevels.length];
    for( int i = 0; i < m_waterlevels.length; i++ )
      m_waterlevels[i] = new ProjectedWaterlevel( profileLine, waterlevels[i] );
  }

  public IStatus getStatus( )
  {
    return m_log.asMultiStatus( Messages.getString("ProjectedWaterlevels_0") ); //$NON-NLS-1$
  }

  public IProfileObject[] createParts( ) throws MismatchedDimensionException, FactoryException, TransformException
  {
    /* create 'original' waterlevel: just the fixation points projected to the profile line */
    final ProjectOriginalWaterlevelWorker originalWorker = new ProjectOriginalWaterlevelWorker( m_eventName, m_waterlevels );
    final IStatus originalStatus = originalWorker.execute();
    if( !originalStatus.isOK() )
      m_log.add( originalStatus );

    final IProfileObject originalWaterlevel = originalWorker.getResult();

    /* ignore empty waterlevels */
    if( originalWaterlevel.getRecords().size() == 0 )
    {
      m_log.add( IStatus.WARNING, Messages.getString("ProjectedWaterlevels_1"), null, m_station ); //$NON-NLS-1$
      return new IProfileObject[] {};
    }

    // FIXME
    final double maxDistance = 0.001; // 1 cm
    /* simplify original waterlevel, to avoid too many points */
    final SimplifyProjectedWaterlevelWorker simplifyWorker = new SimplifyProjectedWaterlevelWorker( originalWaterlevel, maxDistance );
    final IStatus simplifyStatus = simplifyWorker.execute();
    if( !simplifyStatus.isOK() )
      m_log.add( simplifyStatus );

    final IProfileObject simplifiedWaterlevel = simplifyWorker.getResult();

    /* create 2d waterlevels */
    final IProfileObject[] waterlevels2d = create2Dwaterlevels( simplifiedWaterlevel );

    /* build return set */
    final Collection<IProfileObject> allParts = new ArrayList<>();
    allParts.add( simplifiedWaterlevel );
    allParts.addAll( Arrays.asList( waterlevels2d ) );

    return allParts.toArray( new IProfileObject[allParts.size()] );
  }

  private IProfileObject[] create2Dwaterlevels( final IProfileObject simplifiedWaterlevel )
  {
    final Coordinate[] profileCoordinates = extractWidthHeightCoordinates();
    if( profileCoordinates.length < 2 )
      return new IProfileObject[] {};

    final Range<Double> profileWidthRange = calculateExtendedWidthRange( profileCoordinates );

    /* create waterlevel line */
    final LineString waterlevelLine = extractLine( simplifiedWaterlevel, profileWidthRange );
    if( waterlevelLine == null )
      return new IProfileObject[] {};

    /* intersect waterlevel with profile line */
    final JTSWaterlevelIntersector intersector = new JTSWaterlevelIntersector( profileCoordinates );
    final LineString[] waterlevelLines = intersector.createWaterlevels( waterlevelLine );

    /* build parts from intersections */
    return buildWaterlevel2dParts( waterlevelLines );
  }

  /**
   * Fetch coordinates in width/height coordinate system from profile line
   */
  private Coordinate[] extractWidthHeightCoordinates( )
  {
    final Coordinate[] profileCoordinates = m_profileLine.getCoordinates();

    final Coordinate[] widthHeights = new Coordinate[profileCoordinates.length];

    for( int i = 0; i < widthHeights.length; i++ )
    {
      final double width = m_profileLine.getMatN( i );
      final double height = profileCoordinates[i].z;

      widthHeights[i] = new Coordinate( width, height );
    }

    return widthHeights;
  }

  /**
   * Calculate min/max x value of coordinates.<br/>
   * The returned range is extended in both directions by the width of the real range.
   */
  private Range<Double> calculateExtendedWidthRange( final Coordinate[] profileCoordinates )
  {
    /* calculate min/max */
    double min = Double.MAX_VALUE;
    double max = -Double.MAX_VALUE;

    for( final Coordinate coordinate : profileCoordinates )
    {
      final double value = coordinate.x;

      min = Math.min( min, value );
      max = Math.max( max, value );
    }

    /* extend range */
    final double distance = max - min;

    return Range.between( min - distance, max + distance );
  }

  private LineString extractLine( final IProfileObject simplifiedWaterlevel, final Range<Double> profileWidthRange )
  {
    final CoordinateList waterlevelLocations = new CoordinateList();

    /* extract waterlevel locations */
    final IProfileObjectRecords records = simplifiedWaterlevel.getRecords();
    for( int i = 0; i < records.size(); i++ )
    {
      final IProfileObjectRecord record = records.getRecord( i );
      final Coordinate widthHeight = record.getWidthHeightLocation();
      waterlevelLocations.add( widthHeight, false );
    }

    /* sort locations by x */
    sortCoordinatesByX( waterlevelLocations );

    /* deny empty waterlevels */
    if( waterlevelLocations.isEmpty() )
      return null;

    /* extend waterlevel to left and right to make sure that it covers the whole profile */
    final Coordinate leftBorder = waterlevelLocations.getCoordinate( 0 );
    final double leftExtendX = Math.min( leftBorder.x, profileWidthRange.getMinimum() );
    final Coordinate leftExtend = new Coordinate( leftExtendX, leftBorder.y );

    final Coordinate rightBorder = waterlevelLocations.getCoordinate( waterlevelLocations.size() - 1 );
    final double rightExtendX = Math.min( rightBorder.x, profileWidthRange.getMaximum() );
    final Coordinate rightExtend = new Coordinate( rightExtendX, leftBorder.y );

    waterlevelLocations.add( 0, leftExtend, false );
    waterlevelLocations.add( rightExtend, false );

    /* create the waterlevel line */
    final Coordinate[] waterlevelcoordinates = waterlevelLocations.toCoordinateArray();
    return m_profileLine.getFactory().createLineString( waterlevelcoordinates );
  }

  /**
   * Extra method to suppress warning
   */
  @SuppressWarnings( "unchecked" )
  private void sortCoordinatesByX( final CoordinateList coordinates )
  {
    /* sort locations by x; fortunately the natural order is what we need */
    Collections.sort( coordinates );
  }

  private IProfileObject[] buildWaterlevel2dParts( final LineString[] waterlevelLines )
  {
    final Collection<IProfileObject> waterlevelParts = new ArrayList<>( waterlevelLines.length );
    for( final LineString waterlevelLine : waterlevelLines )
    {
      try
      {
        final IProfileObject waterlevelPart = buildWaterlevel2dPart( waterlevelLine );
        if( waterlevelPart != null )
        {
          /* set general data */
          final String waterlevelName = m_eventName + waterlevelParts.size() + 1;
          // TODO: important, that name is unique withing the cross section, how can we force this here?
          waterlevelPart.setValue( IGafConstants.PART_NAME, waterlevelName );
          waterlevelParts.add( waterlevelPart );
        }
      }
      catch( final MGeometryException | MismatchedDimensionException | FactoryException | TransformException e )
      {
        m_log.add( IStatus.WARNING, Messages.getString("ProjectedWaterlevels_2"), e ); //$NON-NLS-1$
      }
    }

    return waterlevelParts.toArray( new IProfileObject[waterlevelParts.size()] );
  }

  private IProfileObject buildWaterlevel2dPart( final LineString waterlevelLine ) throws MGeometryException, MismatchedDimensionException, FactoryException, TransformException
  {
    /* prepare to extract original points involved in this waterlevel part */
    final Range<Double> widthRange = Range.between( waterlevelLine.getStartPoint().getX(), waterlevelLine.getEndPoint().getX() );

    /* create generic part of 2d waterlevel */
    final GenericProfileHorizon waterlevel2D = new GenericProfileHorizon( IWspmTuhhConstants.OBJECT_TYPE_WATERLEVEL_SEGMENT );

    /* get description only from involved points */
    final AggregatedWaterlevel aggregator = new AggregatedWaterlevel( m_waterlevels, widthRange );
    final String description = aggregator.getDescription();
    waterlevel2D.setDescription( description );

    /* get discharge only from involved points */
    final BigDecimal discharge = aggregator.getDischarge();
    if( discharge != null )
      waterlevel2D.setValue( IGafConstants.METADATA_WATERLEVEL_DISCHARGE, discharge.toString() );

    /* convert to points */
    final IProfileObjectRecords records = waterlevel2D.getRecords();

    final Coordinate[] coordinates = waterlevelLine.getCoordinates();
    for( final Coordinate coordinate : coordinates )
    {
      final double width = coordinate.x;
      final double height = coordinate.y;

      /* extract location at width from profile */
      final Coordinate location = m_profileLine.getCoordinateAtM( width );

      /* create record and add values */
      final IProfileObjectRecord record = records.addNewRecord();

      record.setBreite( width );
      record.setHoehe( height );
      record.setComment( null );

      // FIXME: extrapolate location if outside profile
      if( location != null )
      {
        record.setRechtswert( location.x );
        record.setHochwert( location.y );
      }

      // FIXME: WS is of kind W, not W2
      record.setCode( GafPointCode.WS.getKey() );
    }

    return waterlevel2D;
  }
}