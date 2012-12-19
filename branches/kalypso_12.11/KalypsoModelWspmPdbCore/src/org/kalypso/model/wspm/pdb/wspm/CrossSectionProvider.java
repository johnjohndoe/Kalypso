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
package org.kalypso.model.wspm.pdb.wspm;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.hibernatespatial.mgeom.MCoordinate;
import org.hibernatespatial.mgeom.MGeometryFactory;
import org.hibernatespatial.mgeom.MLineString;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPart;
import org.kalypso.model.wspm.pdb.db.mapping.Point;
import org.kalypso.model.wspm.pdb.gaf.GafKind;

/**
 * Implementation of {@link ISectionProvider} based on {@link CrossSection}.
 * 
 * @author Gernot Belger
 */
public class CrossSectionProvider implements ISectionProvider
{
  private final CrossSection m_section;

  private MLineString m_profileLine;

  private final MGeometryFactory m_geometryFactory;

  public CrossSectionProvider( final CrossSection section, final MGeometryFactory geometryFactory )
  {
    m_section = section;
    m_geometryFactory = geometryFactory;
  }

  @Override
  public BigDecimal getStation( )
  {
    return m_section.getStation();
  }

  @Override
  public CrossSection getSection( )
  {
    return m_section;
  }

  @Override
  public synchronized MLineString getProfileLine( )
  {
    if( m_profileLine == null )
      m_profileLine = buildProfileLine();

    return m_profileLine;
  }

  private MLineString buildProfileLine( )
  {
    final Set<CrossSectionPart> parts = m_section.getCrossSectionParts();
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

    final Set<Point> points = ppart.getPoints();
    for( final Point point : points )
      sortedPoints.put( point.getConsecutiveNum(), point );

    if( sortedPoints.size() < 2 )
      return null;

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

    return m_geometryFactory.createMLineString( coords.toArray( new MCoordinate[coords.size()] ) );
  }
}