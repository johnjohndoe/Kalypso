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
package org.kalypso.model.wspm.tuhh.core.ctripple;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;

/**
 * The coded tripple reader.
 * 
 * @author Holger Albert
 */
public class CodedTrippleReader
{
  /**
   * Contains the profiles and horizons.
   */
  private CodedTripple m_codedTripple;

  public CodedTrippleReader( )
  {
    m_codedTripple = null;
  }

  /**
   * This function reads the source file and creates the coded tripple.
   * 
   * @param sourceFile
   *          The source file.
   * @param mapper
   *          The coded tripple horizon mapper.
   */
  public void read( File sourceFile, CodedTrippleHorizonMapper mapper ) throws IOException
  {
    /* The reader. */
    BufferedReader bf = null;

    try
    {
      /* Reset the old coded triple. */
      m_codedTripple = null;

      /* Create the reader. */
      bf = new BufferedReader( new FileReader( sourceFile ) );

      /* Create the coded tripple. */
      CodedTripple codedTripple = new CodedTripple( mapper );

      String line = null;
      while( (line = bf.readLine()) != null )
      {
        String trimmedLine = line.trim();
        if( StringUtils.isEmpty( trimmedLine ) )
          continue;

        CodedTrippleProfilePoint point = CodedTrippleProfilePoint.createProfilePoint( trimmedLine );
        codedTripple.addProfilePoint( point );
      }

      /* Store the new coded triple. */
      m_codedTripple = codedTripple;
    }
    finally
    {
      /* Close the reader. */
      IOUtils.closeQuietly( bf );
    }
  }

  /**
   * This function returns the coded tripple. It may return null, if no file was read or an error has occured during reading of a file.
   * 
   * @return The coded tripple or null.
   */
  public CodedTripple getCodedTripple( )
  {
    return m_codedTripple;
  }
}