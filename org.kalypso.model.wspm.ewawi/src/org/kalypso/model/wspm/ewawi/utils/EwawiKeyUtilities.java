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

import org.apache.commons.io.FilenameUtils;

/**
 * This class contains functions for handling keys of a ewawi plus object. E.g. it can generate a key for a file.
 * 
 * @author Holger Albert
 */
public class EwawiKeyUtilities
{
  /**
   * The constructor.
   */
  private EwawiKeyUtilities( )
  {
  }

  /**
   * This function generates the key.
   * 
   * @param file
   *          The file.
   * @return The generated key.
   */
  public static EwawiKey generateKey( final File file )
  {
    final String name = file.getName();
    final String baseName = FilenameUtils.getBaseName( name );
    final String[] parts = baseName.split( "_" );
    if( parts.length == 3 )
      return new EwawiKey( parts[0], parts[1], parts[2], null );

    if( parts.length > 3 )
    {
      final String freeText = parts[parts.length - 1];
      if( "ESTAMM".equals( freeText ) || "LAENGS".equals( freeText ) || "PKT".equals( freeText ) )
        return new EwawiKey( parts[0], parts[1], parts[2], null );

      return new EwawiKey( parts[0], parts[1], parts[2], freeText );
    }

    throw new IllegalStateException( String.format( "File '%s' must contain at least 3 parts (pe, alias, model id)...", file.getName() ) );
  }
}