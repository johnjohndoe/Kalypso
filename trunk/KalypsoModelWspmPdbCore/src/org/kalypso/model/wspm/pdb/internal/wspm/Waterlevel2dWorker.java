/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Bj�rnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universit�t Hamburg-Harburg, Institut f�r Wasserbau, Hamburg, Germany
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
package org.kalypso.model.wspm.pdb.internal.wspm;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.hibernatespatial.mgeom.MCoordinate;
import org.hibernatespatial.mgeom.MGeomUtils;
import org.hibernatespatial.mgeom.MLineString;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecord;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecords;
import org.kalypso.model.wspm.pdb.db.mapping.Point;
import org.kalypso.model.wspm.pdb.db.mapping.WaterlevelFixation;
import org.kalypso.model.wspm.pdb.db.utils.PdbMappingUtils;
import org.kalypso.model.wspm.pdb.gaf.GafKind;
import org.kalypso.model.wspm.pdb.gaf.IGafConstants;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.wspm.ISectionProvider;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.GenericProfileHorizon;
import org.kalypso.transformation.transformer.JTSTransformer;
import org.opengis.geometry.MismatchedDimensionException;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.linearref.LengthIndexedLine;

/**
 * @author Gernot Belger
 */
public class Waterlevel2dWorker
{
  private final IStatusCollector m_log = new StatusCollector( WspmPdbCorePlugin.PLUGIN_ID );

  private final Map<IProfileObject, ISectionProvider> m_waterlevels2D = new HashMap<>();

  private final Map<BigDecimal, Collection<ISectionProvider>> m_sectionsByStation;

  private final Collection<WaterlevelFixation> m_waterlevels;

  private final String m_eventName;

  public Waterlevel2dWorker( final String eventName, final Collection<WaterlevelFixation> waterlevels, final Map<BigDecimal, Collection<ISectionProvider>> sectionsByStation )
  {
    m_eventName = eventName;
    m_waterlevels = waterlevels;
    m_sectionsByStation = sectionsByStation;
  }

  public Map<IProfileObject, ISectionProvider> getWaterlevels2D( )
  {
    return Collections.unmodifiableMap( m_waterlevels2D );
  }

  public IStatus execute( )
  {
    /* hash by station */
    final Map<BigDecimal, Collection<WaterlevelFixation>> waterlevelsByStation = hashWaterlevelsByStation();

    for( final Entry<BigDecimal, Collection<WaterlevelFixation>> entry : waterlevelsByStation.entrySet() )
    {
      final BigDecimal station = entry.getKey();
      final Collection<WaterlevelFixation> waterlevels = entry.getValue();

      /* find cross section(s) for station */
      final Collection<ISectionProvider> sections = m_sectionsByStation.get( station );
      if( sections == null || sections.isEmpty() )
      {
        m_log.add( IStatus.WARNING, "No cross section for watrlevel with station %s", null, station );
        continue;
      }

      // REMARK: assign waterlevel to all sections with same station, because if 2 sections have the same station, we do not know what to do...
      for( final ISectionProvider section : sections )
      {
        final IProfileObject part = createWaterlevel( section, waterlevels );
        if( part != null )
          m_waterlevels2D.put( part, section );
      }
    }

    return m_log.asMultiStatusOrOK( "Build 2D-Waterlevels" );
  }

  private Map<BigDecimal, Collection<WaterlevelFixation>> hashWaterlevelsByStation( )
  {
    final Map<BigDecimal, Collection<WaterlevelFixation>> hash = new TreeMap<>();

    for( final WaterlevelFixation waterlevel : m_waterlevels )
    {
      final BigDecimal station = waterlevel.getStation();

      if( !hash.containsKey( station ) )
        hash.put( station, new ArrayList<WaterlevelFixation>() );

      hash.get( station ).add( waterlevel );
    }

    return hash;
  }

  private IProfileObject createWaterlevel( final ISectionProvider section, final Collection<WaterlevelFixation> waterlevels )
  {
    /* gather some data */
    final MLineString profileLine = section.getProfileLine();
    final BigDecimal discharge = findDischarge( waterlevels );
    final String description = buildDescription( waterlevels );

    /* create part */
    final GenericProfileHorizon waterlevel2D = new GenericProfileHorizon();

    /* set general data */
    // TODO: important, that name is unique withing the cross section, how can we force this here?
    waterlevel2D.setValue( IGafConstants.PART_NAME, m_eventName );
    waterlevel2D.setValue( IGafConstants.PART_TYPE, GafKind.W.toString() );
    waterlevel2D.setDescription( description );
    if( discharge != null )
      waterlevel2D.setValue( IGafConstants.METADATA_WATERLEVEL_DISCHARGE, discharge.toString() );

    /* convert to points */
    final IProfileObjectRecords records = waterlevel2D.getRecords();

    for( final WaterlevelFixation waterlevel : waterlevels )
    {
      try
      {
        createRecord( records, profileLine, waterlevel );
      }
      catch( final MismatchedDimensionException e )
      {
        e.printStackTrace();
        m_log.add( IStatus.ERROR, e.toString() );
      }
      catch( final FactoryException e )
      {
        e.printStackTrace();
        m_log.add( IStatus.ERROR, e.toString() );
      }
      catch( final TransformException e )
      {
        e.printStackTrace();
        m_log.add( IStatus.ERROR, e.toString() );
      }
    }

    if( records.size() == 0 )
    {
      final BigDecimal station = section.getStation();
      m_log.add( IStatus.WARNING, "Skipping waterlevels at %s: no geometries available", null, station );
      return null;
    }

    return waterlevel2D;
  }

  /* build description from all waterlevels */
  private String buildDescription( final Collection<WaterlevelFixation> waterlevels )
  {
    final Set<String> descriptions = new LinkedHashSet<>();

    for( final WaterlevelFixation waterlevel : waterlevels )
    {
      /* collect description, ignore blanks/empty */
      final String description = waterlevel.getDescription();
      descriptions.add( StringUtils.trimToNull( description ) );
    }

    /* Build combined description without null elements */
    descriptions.remove( null );
    return StringUtils.join( descriptions, ", " ); //$NON-NLS-1$
  }

  private BigDecimal findDischarge( final Collection<WaterlevelFixation> waterlevels )
  {
    for( final WaterlevelFixation waterlevel : waterlevels )
    {
      final BigDecimal discharge = waterlevel.getDischarge();
      if( discharge != null )
        return discharge;
    }

    return null;
  }

  private IProfileObjectRecord createRecord( final IProfileObjectRecords records, final MLineString profileLine, final WaterlevelFixation waterlevel ) throws MismatchedDimensionException, FactoryException, TransformException
  {
    /* Fetch data from waterlevel */
    final com.vividsolutions.jts.geom.Point waterlevelPoint = getWaterlevelLocationInProfileSrs( profileLine, waterlevel );

    /* skip points without location, we cannot project them to the profile */
    if( waterlevelPoint == null )
      return null;

    final Coordinate waterlevelLocation = waterlevelPoint.getCoordinate();

    final String description = waterlevel.getDescription();

    /* create record and add values */
    final IProfileObjectRecord record = records.addNewRecord();

    record.setComment( description );

    /* keep original location of waterlevel, not the projection on the section, which can be computed by width */
    record.setRechtswert( waterlevelLocation.x );
    record.setHochwert( waterlevelLocation.y );

    record.setCode( IGafConstants.CODE_WS );

    record.setHoehe( waterlevel.getWaterlevel().doubleValue() );

    final double distance = profileLine.distance( waterlevelPoint );
    final double maxDistance = 1.0; // [m]
    if( distance > maxDistance )
    {
      final BigDecimal station = waterlevel.getStation();
      m_log.add( IStatus.WARNING, "Waterlevel with station %s: big distance to corresponding profile line: %d [m]", null, station, distance );
    }

    final BigDecimal width = calculateWidth( profileLine, waterlevelLocation );
    // FIXME: why doubles in record??
    record.setBreite( width.doubleValue() );

    return record;
  }

  private com.vividsolutions.jts.geom.Point getWaterlevelLocationInProfileSrs( final MLineString profileLine, final WaterlevelFixation waterlevel ) throws FactoryException, MismatchedDimensionException, TransformException
  {
    final com.vividsolutions.jts.geom.Point location = waterlevel.getLocation();
    if( location == null )
      return null;

    final Coordinate coordinate = location.getCoordinate();

    final int wSRID = location.getSRID();
    final int targetSRID = profileLine.getSRID();

    final JTSTransformer transformer = new JTSTransformer( wSRID, targetSRID );
    final Coordinate transformed = transformer.transform( coordinate );

    return profileLine.getFactory().createPoint( transformed );
  }

  private BigDecimal calculateWidth( final MLineString mProfileLine, final Coordinate waterlevelLocation )
  {
    /* calculate width and location on profile line */
    final LengthIndexedLine index = new LengthIndexedLine( mProfileLine );
    final double projectedIndex = index.project( waterlevelLocation );
    final MCoordinate projectedLocationWithM = MGeomUtils.extractPoint( mProfileLine, projectedIndex );

    if( Double.isNaN( projectedLocationWithM.m ) )
      return null;

    final BigDecimal mWidth = new BigDecimal( projectedLocationWithM.m );

    final int scale = PdbMappingUtils.findScale( Point.class, Point.PROPERTY_WIDTH );
    return mWidth.setScale( scale, BigDecimal.ROUND_HALF_UP );
  }
}