package org.kalypso.convert.namodel;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.Writer;
import java.net.URL;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.util.factory.FactoryException;
import org.kalypso.zml.ObservationType;

/**
 * @author doemming
 */
public class NAZMLGenerator
{
  public static void main( String[] args )
  {
    try
    {
      convert();
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

  }

  public NAZMLGenerator() throws IOException
  {
    URL defaultZmlUrl=getClass().getResource("");
    InputStream stream = defaultZmlUrl.openStream();
    
  }
  public static void convert() throws SensorException, JAXBException, IOException, FactoryException
  {
//    while( true )
//    {
//      String line = "01 09 1995 00 00 00  0.3";
//      String pattern = "([0-9 ]+?)  ([0-9\\.]+)";
//      Pattern p = Pattern.compile( pattern );
//      Matcher matcher = p.matcher( line );
//      if( matcher.matches() )
//        System.out.println( " :-)" );
//      else
//        System.out.println( " :-(" );
//    }
    // generate ZMLFile
    
//      File zmlFile=createTmpZmlL();
//    
//        File zmlFile=new File("/home/doemming/weisseElster/klima.dat/test.zml");
//        IObservation observation = ZmlFactory.parseXML(zmlFile.toURL(),"ID");
//        ObservationType type = ZmlFactory.createXML(observation, null);
//        Marshaller marshaller = ZmlFactory.getMarshaller();
//        Writer writer=new
//     FileWriter("/home/doemming/weisseElster/klima.dat/test.zml_out");
//        marshaller.marshal(type, writer);
//        writer.close();
  }

  
  private static File createTmpZmlL()
  {
    
    return null;
  }

  

}