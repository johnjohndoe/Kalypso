/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
  
---------------------------------------------------------------------------------------------------*/
package org.kalypso.convert.dwd.test;

import java.io.File;
import java.io.FileWriter;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;

import javax.xml.bind.Marshaller;

import junit.framework.TestCase;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree_impl.extension.ITypeRegistry;
import org.deegree_impl.extension.TypeRegistrySingleton;
import org.kalypso.convert.dwd.KrigingReader;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;
import org.kalypso.zml.repository.virtual.ObjectFactory;
import org.kalypso.zml.repository.virtual.VirtualRepository;

public class RasterTest extends TestCase
{
  public void testRaster()
  {
    KrigingReader reader = null;
    InputStream resourceAsStream = getClass().getResourceAsStream(
        "../resources/Kriging_GewichteWeisseElster.txt" );
    try
    {
      final ITypeRegistry registry = TypeRegistrySingleton.getTypeRegistry();
      registry.registerTypeHandler( new ObservationLinkHandler() );
      final InputStreamReader inputStreamReader = new InputStreamReader( resourceAsStream );
      reader = new KrigingReader( inputStreamReader );
      URL gmlURL = getClass().getResource( "../../namodel/modell/modell.gml" );
      URL schemaURL = getClass().getResource( "../../namodel/schema/namodell.xsd" );
      GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( gmlURL, schemaURL );
      Feature[] features = workspace.getFeatures( workspace.getFeatureType( "Catchment" ) );
      final String geoPropName = "Ort";
      VirtualRepository repository = reader.createRepositoryConf( features, geoPropName );
      ObjectFactory o = new ObjectFactory();
      Marshaller marshaller = o.createMarshaller();
      final File resultFile = new File( "C:\\TMP/repository.conf" );
      FileWriter confWriter = new FileWriter( resultFile );

      marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
      marshaller.marshal( repository, confWriter );
      confWriter.close();
      System.out.println( "wrote repositoryConf to " + resultFile.getAbsolutePath() );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      fail( e.getMessage() );
    }
  }
}