package org.kalypso.ogc.gml.test;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileWriter;
import java.io.InputStreamReader;
import java.io.Writer;

import javax.xml.bind.JAXBException;

import junit.framework.TestCase;

import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree_impl.extension.TypeRegistryException;
import org.deegree_impl.extension.TypeRegistrySingleton;
import org.deegree_impl.gml.schema.EnumerationFeatureTypeProperty;
import org.deegree_impl.gml.schema.GMLSchema;
import org.deegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.deegree_impl.model.feature.XLinkFeatureTypeProperty;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;
import org.kalypso.util.xml.XMLTools;
import org.opengis.cs.CS_CoordinateSystem;
import org.xml.sax.InputSource;

/**
 * @author doemming
 */
public class JMSchemaTest extends TestCase
{
  static
  {
    try
    {
      TypeRegistrySingleton.getTypeRegistry().registerTypeHandler( new ObservationLinkHandler() );
    }
    catch( TypeRegistryException e )
    {
      e.printStackTrace();
    }
    catch( JAXBException e )
    {
      e.printStackTrace();
    }
  }

  public void loadSchemaAndCompareIt( final String xsdName, final String compareFile )
      throws Exception
  {
    final GMLSchema jmSchema = new GMLSchema( XMLTools.getAsDOM( JMSchemaTest.class.getResourceAsStream(
        xsdName ) ) );

    final FeatureType[] ftps = jmSchema.getFeatureTypes();
    final String result = toString( 0, ftps );

    System.out.println( result );

    // TODO Method may fail to close stream
    //
    //    The method creates an IO stream object, does not assign it to any fields,
    // pass it to other methods, or return it,
    //     and does not appear to close the stream on all paths out of the method.
    // This may result in a file descriptor leak.
    //     It is generally a good idea to use a finally block to ensure that streams
    // are closed.
    final BufferedReader reader = new BufferedReader( new InputStreamReader( getClass()
        .getResourceAsStream( compareFile ) ) );
    final StringBuffer goal = new StringBuffer();
    String buffer;
    while( ( buffer = reader.readLine() ) != null )
      goal.append( buffer );

    final String compareResult = goal.toString();
    assertEquals( compareResult.replaceAll( "\\s", "" ), result.replaceAll( "\\s", "" ) );
  }

  public void testObservationLink() throws Exception
  {
    loadSchemaAndCompareIt( "obslink_schema.xsd", "obslink_schema.txt" );
  }

  public void testObservationLinkGML() throws Exception
  {
    loadAndWriteGML( "obslink_schema.xsd", "obslink_schema.gml" );
  }

  public void testXLink() throws Exception
  {
    loadSchemaAndCompareIt( "xlink_schema.xsd", "xlink_schema.txt" );
  }

  public void testXlinkGML() throws Exception
  {
    loadAndWriteGML( "xlink_schema.xsd", "xlink_schema.gml" );
  }

  private void loadAndWriteGML( final String xsdFile, final String gmlFile ) throws Exception
  {
    InputSource schemaSource = new InputSource( getClass().getResourceAsStream( xsdFile ) );
    InputSource gmlSource = new InputSource( getClass().getResourceAsStream( gmlFile ) );
    ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();
    CS_CoordinateSystem crs = org.deegree_impl.model.cs.Adapters.getDefault().export(
        csFac.getCSByName( "EPSG:4326" ) );

    KalypsoFeatureLayer[] layers = GmlSerializer.deserialize( schemaSource, gmlSource, crs, null );

    System.out.println( "GML loaded" );
    File outFile = File.createTempFile( "test_parse", "gml" );
    final Writer writer = new FileWriter( outFile );

    GmlSerializer.serialize( writer, layers, null );
    System.out.println( "GML saved in " + outFile.getPath() );
  }

  private String toString( int indent, FeatureType[] fts )
  {
    String result = "";

    for( int i = 0; i < fts.length; i++ )
    {
      result = result + toString( indent, fts[i] );
    }

    return result;
  }

  private String toString( int indent, FeatureType ft )
  {
    String result = getIndent( indent ) + "FeatureType: " + ft.getName()
        + toString( indent + 1, ft.getProperties() );
    FeatureType[] childs = ft.getChildren();

    if( childs.length > 0 )
      result = result + getIndent( indent + 1 ) + "childs:" + toString( indent + 1, childs );

    return result;
  }

  private String toString( int indent, FeatureTypeProperty[] ftps )
  {
    String result = "";

    for( int i = 0; i < ftps.length; i++ )
    {
      result = result + toString( indent + 1, ftps[i] );
    }

    return result;
  }

  private String toString( int indent, FeatureTypeProperty ftp )
  {
    String result = ( getIndent( indent ) + ftp.getName() + "                                                    " )
        .substring( 0, indent + 40 )
        + ftp.getType();

    if( ftp instanceof XLinkFeatureTypeProperty )
      result = result + toString( (XLinkFeatureTypeProperty)ftp );

    if( ftp instanceof EnumerationFeatureTypeProperty )
      result = result + toString( (EnumerationFeatureTypeProperty)ftp );

    return result;
  }

  private String toString( final XLinkFeatureTypeProperty xlinkftp )
  {
    return " [" + xlinkftp.toString() + "]";

  }

  private String toString( final EnumerationFeatureTypeProperty eftp )
  {
    String result = "  Enumeration [";
    Object[] enum = eftp.getEnumeration();

    for( int i = 0; i < enum.length; i++ )
      result = result + " " + enum[i].toString();

    return result + "]";
  }

  private String getIndent( int indent )
  {
    String result = "";
    String space = "  ";
    while( indent-- > 0 )
      result = result + space;
    return "\n" + result;

  }
}