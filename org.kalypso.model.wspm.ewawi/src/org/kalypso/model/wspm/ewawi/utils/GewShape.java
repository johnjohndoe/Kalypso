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
package org.kalypso.model.wspm.ewawi.utils;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.SortedMap;
import java.util.TreeMap;

import org.apache.commons.io.FilenameUtils;
import org.kalypso.model.wspm.ewawi.shape.writer.log.XyzEwawiLogger;
import org.kalypso.shape.FileMode;
import org.kalypso.shape.ShapeFile;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypso.shape.dbf.IDBFField;
import org.kalypso.shape.geometry.ISHPGeometry;
import org.kalypso.shape.tools.SHP2JTS;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;

/**
 * This file holds the shape of the rivers, containing the river number and river name.
 * 
 * @author Holger Albert
 */
public class GewShape
{
  public static final String GKZ_FGN25 = "GKZ_FGN25";

  public static final String GN_ACHS_08 = "GN_Achs_08";

  public static final String ALIAS = "Alias";

  private final File m_gewShape;

  private final Map<Long, List<GewShapeRow>> m_gkzFgn25Hash;

  public GewShape( final File gewShape )
  {
    m_gewShape = gewShape;
    m_gkzFgn25Hash = new HashMap<>();
  }

  public void init( ) throws DBaseException, IOException
  {
    /* Read the shape file. */
    final ShapeFile shapeFile = new ShapeFile( FilenameUtils.removeExtension( m_gewShape.getAbsolutePath() ), Charset.defaultCharset(), FileMode.READ );

    /* Get the fields. */
    final IDBFField[] fields = shapeFile.getFields();

    /* Loop all rows. */
    final int numRecords = shapeFile.getNumRecords();
    for( int i = 0; i < numRecords; i++ )
    {
      /* Get all values of the row. */
      final Object[] values = shapeFile.getRow( i );
      final ISHPGeometry shape = shapeFile.getShape( i );

      /* Create the data object for the row. */
      final GewShapeRow row = new GewShapeRow( fields, shape, values );

      final String gkzFgn25 = (String)row.getValue( GKZ_FGN25 );
      final long parsedGkzFgn25 = Long.parseLong( gkzFgn25 );
      if( !m_gkzFgn25Hash.containsKey( parsedGkzFgn25 ) )
        m_gkzFgn25Hash.put( parsedGkzFgn25, new ArrayList<GewShapeRow>() );

      final List<GewShapeRow> gewList = m_gkzFgn25Hash.get( parsedGkzFgn25 );
      gewList.add( row );
    }

    shapeFile.close();
  }

  public ISHPGeometry getShape( final Long gkzFgn25, final Geometry geometry, final XyzEwawiLogger logger )
  {
    final List<GewShapeRow> rows = m_gkzFgn25Hash.get( gkzFgn25 );
    if( rows == null || rows.size() == 0 )
      return null;

    final GewShapeRow row = GewShape.findRow( geometry, rows, logger, "Gewaessershape" );
    if( row == null )
      return null;

    return row.getShape();
  }

  public Object getValue( final Long gkzFgn25, final String field, final Geometry geometry, final XyzEwawiLogger logger ) throws DBaseException
  {
    System.out.println( String.format( "Suche Gewässerabschnitte für GKZ '%d'...", gkzFgn25.longValue() ) );

    final List<GewShapeRow> rows = m_gkzFgn25Hash.get( gkzFgn25 );
    if( rows == null || rows.size() == 0 )
    {
      final String message = "Keine Gewässerabschnitte verfügbar.";
      System.out.println( message );

      if( logger != null )
      {
        final Point centroid = geometry.getCentroid();
        logger.logXyzLine( centroid.getX(), centroid.getY(), -9999.0, message, "Gewaessershape", -9999.0 );
      }

      return null;
    }

    final GewShapeRow row = GewShape.findRow( geometry, rows, logger, "Gewaessershape" );
    if( row == null )
      return null;

    return row.getValue( field );
  }

  public static GewShapeRow findRow( final Geometry geometry, final List<GewShapeRow> rows, final XyzEwawiLogger logger, final String category )
  {
    final SHP2JTS shp2jts = new SHP2JTS( geometry.getFactory() );

    /* Collect the distances, so if none intersects, we can use them to return the nearest. */
    final SortedMap<Double, GewShapeRow> distances = new TreeMap<>();

    /* Check, if intersecting. */
    for( final GewShapeRow row : rows )
    {
      final ISHPGeometry shape = row.getShape();

      final Geometry value = shp2jts.transform( shape );
      if( value == null )
        continue;

      if( value.intersects( geometry ) )
      {
        System.out.println( "Geometrie schneidet einen Gewässerabschnitt, dieser wird verwendet." );
        return row;
      }

      final double distance = value.distance( geometry );
      distances.put( new Double( distance ), row );
    }

    if( distances.size() == 0 )
    {
      final String message = "Keine Gewässerabschnitte verfügbar.";
      System.out.println( message );

      if( logger != null )
      {
        final Point centroid = geometry.getCentroid();
        logger.logXyzLine( centroid.getX(), centroid.getY(), -9999.0, message, category, -9999.0 );
      }

      return null;
    }

    final Entry<Double, GewShapeRow> entry = distances.entrySet().iterator().next();
    final Double key = entry.getKey();
    final GewShapeRow value = entry.getValue();

    final String message = String.format( "Geometrie schneidet keinen Gewässerabschnitt, nähester wird verwendet. Distanz: %f", key.doubleValue() );
    System.out.println( message );

    if( logger != null )
    {
      final Point centroid = geometry.getCentroid();
      logger.logXyzLine( centroid.getX(), centroid.getY(), -9999.0, message, category, key.doubleValue() );
    }

    return value;
  }
}