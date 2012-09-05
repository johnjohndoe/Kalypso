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
package org.kalypso.model.wspm.ewawi.utils.structures;

import java.util.ArrayList;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

import org.kalypso.model.wspm.ewawi.data.EwawiEplLine;
import org.kalypso.model.wspm.ewawi.data.enums.EwawiObjectart;
import org.kalypso.shape.geometry.ISHPMultiPoint;
import org.kalypso.shape.geometry.SHPEnvelope;
import org.kalypso.shape.geometry.SHPGeometryUtils;
import org.kalypso.shape.geometry.SHPMultiPointz;
import org.kalypso.shape.geometry.SHPPointz;
import org.kalypso.shape.geometry.SHPPolyLinez;
import org.kalypso.shape.geometry.SHPRange;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

/**
 * A EWAWI+ length structure.
 * 
 * @author Holger Albert
 */
public class EwawiLengthStructure
{
  private final Short m_objektNummer;

  private final SortedMap<Short, EwawiEplLine> m_eplLines;

  public EwawiLengthStructure( final Short objektNummer )
  {
    m_objektNummer = objektNummer;
    m_eplLines = new TreeMap<>();
  }

  public Short getObjektNummer( )
  {
    return m_objektNummer;
  }

  public EwawiEplLine[] getEplLines( )
  {
    return m_eplLines.values().toArray( new EwawiEplLine[] {} );
  }

  public void addEplLine( final EwawiEplLine eplLine )
  {
    // TODO Punktnummer muss eindeutig sein, vielleicht eine Exception werfen...
    final Short punktNummer = eplLine.getPunktNummer();
    if( m_eplLines.containsKey( punktNummer ) )
      System.out.println( String.format( "Punktnummer '%d' is bereits in Struktur '%d' vorhanden und wird überschrieben.", punktNummer, m_objektNummer ) );

    m_eplLines.put( punktNummer, eplLine );
  }

  public SHPPolyLinez getShape( )
  {
    /* The structure points. */
    final List<SHPPointz> shpPoints = new ArrayList<>();
    final List<Coordinate> coordinates = new ArrayList<>();

    /* Loop all pro lines. */
    for( final EwawiEplLine eplLine : m_eplLines.values() )
    {
      /* We use the length of the line to determine m. */
      final double x = eplLine.getRechtswert().doubleValue();
      final double y = eplLine.getHochwert().doubleValue();
      final double z = eplLine.getHoehe().doubleValue();
      double m = 0.0;

      coordinates.add( new Coordinate( x, y ) );
      if( coordinates.size() > 1 )
      {
        final GeometryFactory factory = new GeometryFactory();
        final LineString lineString = factory.createLineString( coordinates.toArray( new Coordinate[] {} ) );
        m = lineString.getLength();
      }

      final SHPPointz shapez = new SHPPointz( x, y, z, m );
      shpPoints.add( shapez );
    }

    /* Create the line geometry. */
    final SHPPointz[] points = shpPoints.toArray( new SHPPointz[] {} );
    final SHPEnvelope envelope = SHPGeometryUtils.createEnvelope( points );
    final SHPRange zrange = SHPGeometryUtils.createZRange( points );
    final SHPRange mrange = SHPGeometryUtils.createMRange( points );
    final ISHPMultiPoint multiPoint = new SHPMultiPointz( envelope, points, zrange, mrange );

    return new SHPPolyLinez( multiPoint, new int[] { 0 } );
  }

  // HINT: Every function downward uses the first epl point to retrieve the data.

  public EwawiObjectart getObjektArt( )
  {
    final EwawiEplLine[] eplLines = getEplLines();
    return eplLines[0].getObjectArt();
  }

  public Long getGewKennzahl( )
  {
    final EwawiEplLine[] eplLines = getEplLines();
    return eplLines[0].getGewKennzahl();
  }

  public String getComment( )
  {
    final EwawiEplLine[] eplLines = getEplLines();
    return eplLines[0].getComment();
  }
}