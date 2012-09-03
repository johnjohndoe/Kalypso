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

/**
 * This file holds the shape of the rivers, containing the river number and river name.
 * 
 * @author Holger Albert
 */
public class GewShape
{
  private static final String GKZ_FGN25 = "GKZ_FGN25";

  private static final String GN_ACHS_08 = "GN_Achs_08";

  private static final String ALIAS = "Alias";

  private final File m_gewShape;

  private final Map<Long, String> m_gkzGewNames;

  private final Map<String, String> m_aliasGewNames;

  public GewShape( final File gewShape )
  {
    m_gewShape = gewShape;
    m_gkzGewNames = new HashMap<>();
    m_aliasGewNames = new HashMap<>();
  }

  public void init( ) throws DBaseException, IOException
  {
    final ShapeFile shapeFile = new ShapeFile( FilenameUtils.removeExtension( m_gewShape.getAbsolutePath() ), Charset.defaultCharset(), FileMode.READ );
    final int numRecords = shapeFile.getNumRecords();
    for( int i = 0; i < numRecords; i++ )
    {
      /* Get the fields of the record. */
      final String gkzFgn25 = (String)shapeFile.getRowValue( i, GKZ_FGN25 );
      final String gnAchs08 = (String)shapeFile.getRowValue( i, GN_ACHS_08 );
      final String alias = (String)shapeFile.getRowValue( i, ALIAS );

      // FIXME: The gkzFgn25 is not unique! Values will be overwritten...
      final long parsedGkzFgn25 = Long.parseLong( gkzFgn25 );
      if( !m_gkzGewNames.containsKey( parsedGkzFgn25 ) )
        System.out.println( String.format( "Gewässerkennziffer '%d' -> Gewässername: %s", parsedGkzFgn25, gnAchs08 ) );
      else
        System.out.println( String.format( "Doppelt: Gewässerkennziffer '%d' -> Gewässername: %s", parsedGkzFgn25, gnAchs08 ) );

      m_gkzGewNames.put( parsedGkzFgn25, gnAchs08 );

      // FIXME: The alias is not unique! Values will be overwritten...
      if( !m_aliasGewNames.containsKey( alias ) )
        System.out.println( String.format( "Alias '%s' -> Gewässername: %s", alias, gnAchs08 ) );
      else
        System.out.println( String.format( "Doppelt: Alias '%s' -> Gewässername: %s", alias, gnAchs08 ) );

      m_aliasGewNames.put( alias, gnAchs08 );
    }

    shapeFile.close();
  }

  public String getName( final Long gkzFgn25 )
  {
    return m_gkzGewNames.get( gkzFgn25 );
  }

  public String getName( final String alias )
  {
    return m_aliasGewNames.get( alias );
  }
}