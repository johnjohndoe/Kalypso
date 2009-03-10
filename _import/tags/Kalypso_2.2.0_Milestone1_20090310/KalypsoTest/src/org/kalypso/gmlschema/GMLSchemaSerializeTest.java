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

import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypsodeegree_impl.gml.schema.schemata.UrlCatalogOGC;

/**
 * test succeedes when <br>
 * 1. GML(version 3.1) schemas get parsed from OGC-site and stored in local jar-archive<br>
 * AND<br>
 * 2. the local jar-archive gets parsed to another jar-archive to see if first one is valid
 * 
 * @author doemming
 */
public class GMLSchemaSerializeTest extends TestCase
{
  public void testCreateSchemaJarArchive( ) throws Exception
  {
    File tmpArchive1 = null;
    File tmpArchive2 = null;
    File zipDir = null;
    try
    {
      tmpArchive1 = File.createTempFile( "kalypsoSchema", ".jar" );

      final URL schemaURL = UrlCatalogOGC.class.getResource( "gml/3.1.1/base/gml.xsd" );
      GMLSchemaUtilities.createSchemaArchive( schemaURL, tmpArchive1 );

      zipDir = FileUtilities.createNewTempDir( "zipDir" );
      zipDir.mkdirs();
      ZipUtilities.unzip( tmpArchive1, zipDir );

      final URL schemaURL2 = new URL( zipDir.toURL(), GMLSchemaUtilities.BASE_SCHEMA_IN_JAR );

      tmpArchive2 = File.createTempFile( "kalypsoSchema", ".jar" );
      GMLSchemaUtilities.createSchemaArchive( schemaURL2, tmpArchive2 );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw e;
    }
    finally
    {
      if( tmpArchive1 != null )
        tmpArchive1.delete();

      if( zipDir != null )
        FileUtilities.deleteRecursive( zipDir );

      if( tmpArchive2 != null )
        tmpArchive2.delete();
    }
  }

}
