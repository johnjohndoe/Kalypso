/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.kalypsomodel1d2d.conv.test;

import java.io.File;
import java.io.IOException;
import java.net.URL;

import junit.framework.TestCase;

import org.apache.commons.io.FileUtils;
import org.junit.Test;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.net.UrlUtilities;
import org.kalypso.kalypsomodel1d2d.conv.GmlMesh2HmoConverter;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author felipe maximino
 *
 */
public class GmlMesh2HmoConverterTest extends TestCase
{  
  @Test
  public void testConvert() throws Exception
  {
    URL gmlLocation = getClass().getResource( "tinyDiscretization.gml" );
    assertNotNull( gmlLocation );

    URL hmoLocation = getClass().getResource( "tinyDis.hmo" );
    assertNotNull( hmoLocation );

    GMLWorkspace disWorkspace = GmlSerializer.createGMLWorkspace( gmlLocation, null );
    Feature rootFeature = disWorkspace.getRootFeature();
    IFEDiscretisationModel1d2d dis = (IFEDiscretisationModel1d2d) rootFeature.getAdapter( IFEDiscretisationModel1d2d.class );    
    
    File result = FileUtilities.createNewUniqueFile( "hmoTest", FileUtilities.TMP_DIR );
    
    GmlMesh2HmoConverter conv = new GmlMesh2HmoConverter( dis );
    conv.writeMesh( result );
    
    assertContentEquals( result, hmoLocation );

    result.delete();
  }
  
  private void assertContentEquals( File file, URL location ) throws IOException
  {
    String fileContent = FileUtils.readFileToString( new File(file.getAbsolutePath()), System.getProperty( "file.encoding" ) );
    String urlContent = UrlUtilities.toString( location, System.getProperty( "file.encoding" ) );
    assertEquals( fileContent, urlContent );
  }  
}
