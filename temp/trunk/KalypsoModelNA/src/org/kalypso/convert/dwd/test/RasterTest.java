package org.kalypso.convert.dwd.test;

import java.io.File;
import java.io.FileWriter;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;

import javax.xml.bind.Marshaller;
import javax.xml.transform.OutputKeys;

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