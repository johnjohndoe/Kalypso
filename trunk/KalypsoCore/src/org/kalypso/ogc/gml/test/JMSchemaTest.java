package org.kalypso.ogc.gml.test;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileWriter;
import java.io.InputStreamReader;
import java.io.Writer;
import java.net.URL;

import javax.xml.bind.JAXBException;

import junit.framework.TestCase;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureAssociationTypeProperty;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree_impl.extension.TypeRegistryException;
import org.deegree_impl.extension.TypeRegistrySingleton;
import org.deegree_impl.gml.schema.EnumerationFeatureTypeProperty;
import org.deegree_impl.gml.schema.GMLSchema;
import org.deegree_impl.model.cs.Adapters;
import org.deegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.deegree_impl.model.feature.XLinkFeatureTypeProperty;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;
import org.opengis.cs.CS_CoordinateSystem;

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

  public void loadSchemaAndCompareIt( String xsdName, final String compareFile ) throws Exception
  {
  
    final GMLSchema jmSchema = new GMLSchema(  getClass().getResource( xsdName ) ) ;

    final FeatureType[] ftps = jmSchema.getFeatureTypes();
    final String result = toString( 0, ftps );

    System.out.println( result );

    final BufferedReader reader = new BufferedReader( new InputStreamReader( getClass()
        .getResourceAsStream( compareFile ) ) );
    final StringBuffer goal = new StringBuffer();
    String buffer;
    while( ( buffer = reader.readLine() ) != null )
      goal.append( buffer );

    final String compareResult = goal.toString();
    assertEquals( compareResult.replaceAll( "\\s", "" ), result.replaceAll( "\\s", "" ) );
  }

  //  public void testObservationLink() throws Exception
  //  {
  //    loadSchemaAndCompareIt( "obslink_schema.xsd", "obslink_schema.txt" );
  //  }

  //  public void testObservationLinkGML() throws Exception
  //  {
  //    loadAndWriteGML( "obslink_schema.xsd", "obslink_schema.gml" );
  //  }

  //  public void testNaModelSchema() throws Exception
  //  {
  //    loadSchemaAndCompareIt( "namodell_schema.xsd", "namodell_schema.txt" );
  //  }
  public void testReadingSchema() throws Exception
  {
    try
    {
    	loadSchemaAndCompareIt( "spreemodell/modell.xsd", "spreemodell/modell.txt" );
//      loadSchemaAndCompareIt( "namodellV3.xsd", "namodellV3.txt" );
//      loadSchemaAndCompareIt( "namodellV2.xsd", "namodellV2.txt" );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw e;
    }
  }

  public void testSpreeModellGML() throws Exception
  {
  	URL modellURL=getClass().getResource("spreemodell/modell.gml");
  	URL schemaURL=getClass().getResource("spreemodell/modell.xsd");
  		//loadAndWriteGML( , "spreemodell/modell.gml" );
 	Feature fe=GmlSerializer.deserializeFeature(modellURL,schemaURL);
 	FileWriter writer=new FileWriter("/tmp/modell_out.gml");
 	GmlSerializer.serializeFeature(writer,fe,null);
  }

  //  
  //  public void testXLink() throws Exception
  //  {
  //    loadSchemaAndCompareIt( "xlink_schema.xsd", "xlink_schema.txt" );
  //  }
  //  
  //  public void testXlinkGML() throws Exception
  //  {
  //    loadAndWriteGML( "xlink_schema.xsd", "xlink_schema.gml" );
  //  }
  //  

  private void loadAndWriteGML( final String xsdFile, final String gmlFile ) throws Exception
  {
//    InputSource schemaSource = new InputSource( getClass().getResourceAsStream( xsdFile ) );
//    InputSource gmlSource = new InputSource( getClass().getResourceAsStream( gmlFile ) );
    ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();
    CS_CoordinateSystem crs = Adapters.getDefault().export( csFac.getCSByName( "EPSG:4326" ) );

    KalypsoFeatureLayer[] layers = GmlSerializer.deserialize(  getClass().getResource( xsdFile ),getClass().getResource( gmlFile ), crs, null );

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
    
    String result = getIndent( indent ) + "--+"+ft.getNamespace()+":"+ft.getName()+" FeatureType ";
    FeatureType[] childs = ft.getChildren();

    result=result+getIndent( indent +1) + "-+props:"+toString( indent ,ft, ft.getProperties() );
    
    if(childs!=null && childs.length > 0 )
    result = result + toString( indent + 1, childs );

    return result;
  }

  private String toString( int indent,FeatureType parent, FeatureTypeProperty[] ftps )
  {
    String result = "";

    for( int i = 0; i < ftps.length; i++ )
    {
      result = result + toString( indent + 1, parent,ftps[i] );
    }

    return result;
  }

  private String toString( int indent,FeatureType parent, FeatureTypeProperty ftp )
  {
    int ftpPos=parent.getPropertyPosition(ftp.getNamespace()+":"+ftp.getName());
    int maxOccurs=parent.getMaxOccurs(ftpPos);
    int minOccurs=parent.getMinOccurs(ftpPos);
    String result =  getIndent( indent," +"+ ftp.getNamespace()+":"+ftp.getName(),55) + ftp.getType();
    //    String result = ( getIndent( indent ) +" +"+ ftp.getNamespace()+":"+ftp.getName() + "                                                                     " )
    //        .substring( 0, indent + 55 )
    //        + ftp.getType();

    if( ftp instanceof FeatureAssociationTypeProperty )
    {
        
        FeatureAssociationTypeProperty fatp = (FeatureAssociationTypeProperty)ftp;
        FeatureType linkedFT=fatp.getAssociationFeatureType();
        result = result+ getIndent( indent,"",55) + " -->"+linkedFT.getNamespace()+":"+linkedFT.getName();
    }
    if( ftp instanceof XLinkFeatureTypeProperty )
      result = result + toString( (XLinkFeatureTypeProperty)ftp );

    if( ftp instanceof EnumerationFeatureTypeProperty )
      result = result + toString( (EnumerationFeatureTypeProperty)ftp );
    String max= maxOccurs==FeatureType.UNBOUND_OCCURENCY ? "unbounded":""+maxOccurs;
    result = result + " ["+minOccurs+".."+max+"]";

    return result;
  }
  
  private String getIndent( int indent, String text, int distance )
  {
  return ( getIndent( indent ) +text+ "                                                                     " )
        .substring( 0, indent + distance );
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
    String space = "  |";
    while( indent-- > 0 )
      result = result + space;
    return "\n" + result;

  }
}