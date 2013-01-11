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

import java.io.File;
import java.io.IOException;
import java.util.Properties;

import org.kalypso.commons.java.util.PropertiesUtilities;

/**
 * @author Holger Albert
 */
public class CodedTrippleHorizonMapper
{
  /**
   * The code to horizon id mappings.
   */
  private Properties m_codeToHorizonId;

  /**
   * Horizon id to part id mappings.
   */
  private Properties m_horizonIdToPartId;

  private Properties m_codeToCodeDescription;

  private Properties m_horizonIdToHorizonIdDescription;

  public CodedTrippleHorizonMapper( )
  {
    m_codeToHorizonId = new Properties();
    m_horizonIdToPartId = new Properties();
    m_codeToCodeDescription = new Properties();
    m_horizonIdToHorizonIdDescription = new Properties();
  }

  /**
   * This function loads the id mappings. If one file is null, nothing is loaded.
   * 
   * @param codeToHorizonIdFile
   *          The file that contains the code to horizon id mappings.
   * @param horizonIdToPartIdFile
   *          The file that contains the horizon id to part id mappings.
   */
  public void loadIdMappings( File codeToHorizonIdFile, File horizonIdToPartIdFile ) throws IOException
  {
    if( codeToHorizonIdFile == null || horizonIdToPartIdFile == null )
      return;

    Properties codeToHorizonId = PropertiesUtilities.load( codeToHorizonIdFile );
    Properties horizonIdToPartId = PropertiesUtilities.load( horizonIdToPartIdFile );

    m_codeToHorizonId = codeToHorizonId;
    m_horizonIdToPartId = horizonIdToPartId;
  }

  /**
   * This function returns the codes of code to horizon id mappings.
   * 
   * @return The codes of code to horizon id mappings.
   */
  public String[] getCodes( )
  {
    return m_codeToHorizonId.stringPropertyNames().toArray( new String[] {} );
  }

  /**
   * This function returns the horizon ids of the horizon id to part id mappings.
   * 
   * @return The horizon ids of the horizon id to part id mappings.
   */
  public String[] getHorizonIds( )
  {
    return m_horizonIdToPartId.stringPropertyNames().toArray( new String[] {} );
  }

  /**
   * This function returns the horizon id associated with the code of the point.
   * 
   * @param code
   *          The code of the point.
   * @return The horizon id.
   */
  public String getHorizonId( String code )
  {
    return m_codeToHorizonId.getProperty( code );
  }

  /**
   * This function returns the part id of a profile part of Kalypso for the horizon id.
   * 
   * @param horizonId
   *          The horizon id.
   * @return The part id of a profile part of Kalypso.
   */
  public String getPartId( String horizonId )
  {
    return m_horizonIdToPartId.getProperty( horizonId );
  }

  public void loadDescriptionMappings( File codeToCodeDescriptionFile, File horizonIdToHorizonIdDescriptionFile ) throws IOException
  {
    if( codeToCodeDescriptionFile == null || horizonIdToHorizonIdDescriptionFile == null )
      return;

    Properties codeToCodeDescription = PropertiesUtilities.load( codeToCodeDescriptionFile );
    Properties horizonIdToHorizonIdDescription = PropertiesUtilities.load( horizonIdToHorizonIdDescriptionFile );

    m_codeToCodeDescription = codeToCodeDescription;
    m_horizonIdToHorizonIdDescription = horizonIdToHorizonIdDescription;
  }

  public String getCodeDescription( String code )
  {
    return m_codeToCodeDescription.getProperty( code, code );
  }

  public String getHorizonIdDescription( String horizonId )
  {
    return m_horizonIdToHorizonIdDescription.getProperty( horizonId, horizonId );
  }
}