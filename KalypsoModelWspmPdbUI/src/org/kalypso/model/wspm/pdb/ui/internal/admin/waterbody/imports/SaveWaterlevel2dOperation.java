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
package org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.imports;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;

import org.hibernate.Session;
import org.hibernatespatial.mgeom.MCoordinate;
import org.hibernatespatial.mgeom.MGeomUtils;
import org.hibernatespatial.mgeom.MGeometryFactory;
import org.hibernatespatial.mgeom.MLineString;
import org.kalypso.model.wspm.pdb.connect.IPdbOperation;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPart;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.Point;
import org.kalypso.model.wspm.pdb.db.mapping.WaterlevelFixation;
import org.kalypso.model.wspm.pdb.gaf.GafKind;
import org.kalypso.model.wspm.pdb.gaf.IGafConstants;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.linearref.LengthIndexedLine;

/**
 * @author Gernot Belger
 */
public class SaveWaterlevel2dOperation implements IPdbOperation
{
  private final Event m_event;

  private final String m_username;

  private final MGeometryFactory m_factory;

  public SaveWaterlevel2dOperation( final Event event, final String username, final MGeometryFactory factory )
  {
    m_event = event;
    m_username = username;
    m_factory = factory;
  }

  @Override
  public String getLabel( )
  {
    return "Save waterlevel 2d";
  }

  @Override
  public void execute( final Session session )
  {
    /* convert single waterlevels to crosssection parts */
    final Set<WaterlevelFixation> waterlevelFixations = m_event.getWaterlevelFixations();
    final CrossSectionPart[] waterlevelParts = createParts( waterlevelFixations );

    /* not saved as fixation at all */
    waterlevelFixations.clear();

    /* Prepare event for save */
    final Date now = new Date();
    m_event.setCreationDate( now );
    m_event.setEditingDate( now );
    m_event.setEditingUser( m_username );

    session.save( m_event );

    /* create waterlevel crosssection parts */
    for( final CrossSectionPart part : waterlevelParts )
    {
      part.setEvent( m_event );
      /* TODO: hm.... */
      part.setName( m_event.getName() );

      session.save( part );
    }
  }

  private CrossSectionPart[] createParts( final Set<WaterlevelFixation> waterlevelFixations )
  {
    final Collection<CrossSectionPart> parts = new ArrayList<>();

    /* hash by station */
    final Map<BigDecimal, Collection<WaterlevelFixation>> waterlevelsByStation = hashWaterlevelsByStation( waterlevelFixations );
    final Map<BigDecimal, Collection<CrossSection>> sectionsByStation = hashSectionsByStation();

    for( final Entry<BigDecimal, Collection<WaterlevelFixation>> entry : waterlevelsByStation.entrySet() )
    {
      final BigDecimal station = entry.getKey();
      final Collection<WaterlevelFixation> waterlevels = entry.getValue();

      /* find cross section(s) for station */
      final Collection<CrossSection> sections = sectionsByStation.get( station );
      if( sections == null )
        continue;

      // REMARK: assign waterlevel to all sections with same station, because if 2 sections have the same station, we do not know what to do...
      for( final CrossSection section : sections )
      {
        final MLineString profileLineM = buildProfileLine( section );
        /* skip sectinos without geometry */
        if( profileLineM != null )
        {
          /* create part */
          final CrossSectionPart part = createPart( section, profileLineM, waterlevels );
          parts.add( part );
        }
      }
    }

    return parts.toArray( new CrossSectionPart[parts.size()] );
  }

  private Map<BigDecimal, Collection<CrossSection>> hashSectionsByStation( )
  {
    final Map<BigDecimal, Collection<CrossSection>> hash = new HashMap<>();

    final Set<CrossSection> sections = m_event.getState().getCrossSections();

    for( final CrossSection section : sections )
    {
      final BigDecimal station = section.getStation();

      if( !hash.containsKey( station ) )
        hash.put( station, new ArrayList<CrossSection>() );

      hash.get( station ).add( section );
    }

    return hash;
  }

  private Map<BigDecimal, Collection<WaterlevelFixation>> hashWaterlevelsByStation( final Set<WaterlevelFixation> waterlevels )
  {
    final Map<BigDecimal, Collection<WaterlevelFixation>> hash = new HashMap<>();

    for( final WaterlevelFixation waterlevel : waterlevels )
    {
      final BigDecimal station = waterlevel.getStation();

      if( !hash.containsKey( station ) )
        hash.put( station, new ArrayList<WaterlevelFixation>() );

      hash.get( station ).add( waterlevel );
    }

    return hash;
  }

  private CrossSectionPart createPart( final CrossSection section, final MLineString mProfileLine, final Collection<WaterlevelFixation> waterlevels )
  {
    final CrossSectionPart part = new CrossSectionPart( null, section, m_event.getName() );

    // TODO: discharge would be nice, if present;maybe as metadata?
    // part.setDescription( filename? );
    part.setEvent( m_event );

    final Collection<Coordinate> coordinates = new ArrayList<>( waterlevels.size() );
    /* convert to points */
    final Set<Point> points = part.getPoints();

    long pointNum = 0;
    for( final WaterlevelFixation waterlevel : waterlevels )
    {
      final Point point = createPoint( mProfileLine, part, pointNum++, waterlevel );
      points.add( point );

      final com.vividsolutions.jts.geom.Point location = point.getLocation();
      coordinates.add( location.getCoordinate() );
    }

    if( coordinates.size() > 1 )
    {
      final LineString line = m_factory.createLineString( coordinates.toArray( new Coordinate[coordinates.size()] ) );
      part.setLine( line );
    }

    return part;
  }

  private Point createPoint( final MLineString mProfileLine, final CrossSectionPart part, final long num, final WaterlevelFixation waterlevel )
  {
    final Coordinate waterlevelLocation = waterlevel.getLocation().getCoordinate();
    final String description = waterlevel.getDescription();

    final Point point = new Point( null, part, Long.toString( num ), num );

    point.setDescription( description );
    /* keep original location of waterlevel, not the projection on the section, which can be computed by width */
    point.setLocation( m_factory.createPoint( waterlevelLocation ) );

    point.setCode( IGafConstants.CODE_WS );
    // point.setHyk( );

    point.setHeight( waterlevel.getWaterlevel() );

    /* calculate width and location on profile line */
    final LengthIndexedLine index = new LengthIndexedLine( mProfileLine );
    final double projectedIndex = index.project( waterlevelLocation );
    final MCoordinate projectedLocationWithM = MGeomUtils.extractPoint( mProfileLine, projectedIndex );

    final BigDecimal mWidth = Double.isNaN( projectedLocationWithM.m ) ? null : new BigDecimal( projectedLocationWithM.m );

    point.setWidth( mWidth );

    return point;
  }

  private MLineString buildProfileLine( final CrossSection section )
  {
    final Set<CrossSectionPart> parts = section.getCrossSectionParts();
    for( final CrossSectionPart part : parts )
    {
      final String category = part.getCrossSectionPartType().getCategory();
      if( GafKind.P.toString().equals( category ) )
      {
        final MLineString line = buildProfileLine( part );
        if( line != null )
          return line;
      }
    }

    return null;
  }

  private MLineString buildProfileLine( final CrossSectionPart ppart )
  {
    /* sort points by consecutive number */
    final Map<Long, Point> sortedPoints = new TreeMap<>();

    if( sortedPoints.size() < 2 )
      return null;

    final Set<Point> points = ppart.getPoints();
    for( final Point point : points )
      sortedPoints.put( point.getConsecutiveNum(), point );

    /* rebuild line as line M */
    final Collection<MCoordinate> coords = new ArrayList<>( sortedPoints.size() );
    for( final Point point : sortedPoints.values() )
    {
      final com.vividsolutions.jts.geom.Point location = point.getLocation();

      final double xValue = location.getX();
      final double yValue = location.getY();

      final BigDecimal mValue = point.getWidth();
      final BigDecimal zValue = point.getHeight();

      final MCoordinate mCoord = new MCoordinate( xValue, yValue, zValue.doubleValue(), mValue.doubleValue() );
      coords.add( mCoord );
    }

    return m_factory.createMLineString( coords.toArray( new MCoordinate[coords.size()] ) );
  }
}