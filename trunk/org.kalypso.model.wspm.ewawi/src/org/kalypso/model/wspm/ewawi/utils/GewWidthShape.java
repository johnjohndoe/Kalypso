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

import org.apache.commons.io.FilenameUtils;
import org.kalypso.shape.FileMode;
import org.kalypso.shape.ShapeFile;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypso.shape.dbf.IDBFField;
import org.kalypso.shape.geometry.ISHPGeometry;

import com.vividsolutions.jts.geom.Geometry;

/**
 * This file holds the shape of the rivers, containing the river number and river width.
 * 
 * @author Holger Albert
 */
public class GewWidthShape
{
  public static final String GWZ = "GWZ";

  public static final String BREITE = "BREITE";

  private final File m_gewWidthShape;

  private final Map<Long, List<GewShapeRow>> m_gwzHash;

  public GewWidthShape( final File gewShape )
  {
    m_gewWidthShape = gewShape;
    m_gwzHash = new HashMap<>();
  }

  public void init( ) throws DBaseException, IOException
  {
    /* Read the shape file. */
    final ShapeFile shapeFile = new ShapeFile( FilenameUtils.removeExtension( m_gewWidthShape.getAbsolutePath() ), Charset.defaultCharset(), FileMode.READ );

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

      final String gwz = (String)row.getValue( GWZ );
      final long parsedGwz = Long.parseLong( gwz );
      if( !m_gwzHash.containsKey( parsedGwz ) )
        m_gwzHash.put( parsedGwz, new ArrayList<GewShapeRow>() );

      final List<GewShapeRow> gewList = m_gwzHash.get( parsedGwz );
      gewList.add( row );
    }

    shapeFile.close();
  }

  public Object getValue( final Long gwz, final String field, final Geometry geometry ) throws DBaseException
  {
    System.out.println( String.format( "Suche Gewässerabschnitte für GKZ '%d'...", gwz.longValue() ) );

    final List<GewShapeRow> rows = m_gwzHash.get( gwz );
    if( rows == null || rows.size() == 0 )
    {
      System.out.println( "Keine Gewässerabschnitte verfügbar." );
      return null;
    }

    final GewShapeRow row = GewShape.findRow( geometry, rows );
    if( row == null )
      return null;

    return row.getValue( field );
  }
}