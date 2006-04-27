/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.gmlschema;

import java.io.File;
import java.net.URL;

import junit.framework.TestCase;

/**
 * @author doemming
 */
public class GMLSchemaSerializeTest extends TestCase
{
  /**
   * test succeedes when <br>
   * 1. GML(version 3.1) schemas get parsed from OGC-site and stored in local jar-archive<br>
   * AND<br>
   * 2. the local jar-archive gets parsed to another jar-archive to see if first one is valid
   */
  public void testCreateSchemaJarArchive( ) throws Exception
  {
    try
    {
      final File tmpArchive1 = File.createTempFile( "kalypsoSchema", "jar" );
      
      // TODO: better load schema from resources because not everybody can
      // acces the internet (proxy!)
      final URL schemaURL = new URL( "http://schemas.opengis.net/gml/3.1.1/base/gml.xsd" );
      GMLSchemaUtilities.createSchemaArchive( schemaURL, tmpArchive1 );

      final URL schemaURL2 = GMLSchemaUtilities.getSchemaURLForArchive( schemaURL );

      final File tmpArchive2 = File.createTempFile( "kalypsoSchema", "jar" );
      GMLSchemaUtilities.createSchemaArchive( schemaURL2, tmpArchive2 );
      tmpArchive1.delete();
      tmpArchive2.delete();
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw e;
    }
  }

}
