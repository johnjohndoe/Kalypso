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
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.io.FilenameUtils;
import org.kalypso.shape.FileMode;
import org.kalypso.shape.ShapeFile;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypso.shape.dbf.IDBFField;

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

  private final Map<Long, GewShapeRow> m_gkzFgn25Hash;

  private final Map<String, GewShapeRow> m_aliasHash;

  public GewShape( final File gewShape )
  {
    m_gewShape = gewShape;
    m_gkzFgn25Hash = new HashMap<>();
    m_aliasHash = new HashMap<>();
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

      /* Create the data object for the row. */
      final GewShapeRow row = new GewShapeRow( fields, values );

      /* Get the fields of the record. */
      final String gkzFgn25 = (String)row.getValue( GKZ_FGN25 );
      final String gnAchs08 = (String)row.getValue( GN_ACHS_08 );
      final String alias = (String)row.getValue( ALIAS );

      // FIXME: The gkzFgn25 is not unique! Values will be overwritten...
      final long parsedGkzFgn25 = Long.parseLong( gkzFgn25 );
      if( !m_gkzFgn25Hash.containsKey( parsedGkzFgn25 ) )
        System.out.println( String.format( "Gewässerkennziffer '%d' -> Gewässername: %s", parsedGkzFgn25, gnAchs08 ) );
      else
        System.out.println( String.format( "Doppelt: Gewässerkennziffer '%d' -> Gewässername: %s", parsedGkzFgn25, gnAchs08 ) );

      m_gkzFgn25Hash.put( parsedGkzFgn25, row );

      // FIXME: The alias is not unique! Values will be overwritten...
      if( !m_aliasHash.containsKey( alias ) )
        System.out.println( String.format( "Alias '%s' -> Gewässername: %s", alias, gnAchs08 ) );
      else
        System.out.println( String.format( "Doppelt: Alias '%s' -> Gewässername: %s", alias, gnAchs08 ) );

      m_aliasHash.put( alias, row );
    }

    shapeFile.close();
  }

  public String getName( final Long gkzFgn25 ) throws DBaseException
  {
    final GewShapeRow row = m_gkzFgn25Hash.get( gkzFgn25 );
    if( row == null )
      return null;

    return (String)row.getValue( GN_ACHS_08 );
  }

  public String getName( final String alias ) throws DBaseException
  {
    final GewShapeRow row = m_aliasHash.get( alias );
    if( row == null )
      return null;

    return (String)row.getValue( GN_ACHS_08 );
  }
}