package org.kalypso.ogc.sensor.tableview;

import java.io.IOException;
import java.io.InputStream;

import javax.xml.bind.JAXBException;

import org.apache.commons.io.IOUtils;
import org.kalypso.template.obstableview.ObjectFactory;
import org.kalypso.template.obstableview.ObstableviewType;

/**
 * ObservationTableTemplateFactory
 * 
 * @author schlienger
 */
public class ObservationTableTemplateFactory
{
  private final static ObjectFactory OF = new ObjectFactory();
  
  
  private ObservationTableTemplateFactory( )
  {
    // not instanciation
  }

  /**
   * Loads the xml template from the given stream. Closes the stream.
   * 
   * @param ins
   * @return table view template
   * @throws JAXBException
   * @throws IOException
   */
  public static ObstableviewType loadTableTemplateXML( final InputStream ins ) throws JAXBException, IOException
  {
    try
    {
      final ObstableviewType baseTemplate = (ObstableviewType) OF
          .createUnmarshaller().unmarshal( ins );
      
      return baseTemplate;
    }
    finally
    {
      IOUtils.closeQuietly( ins );
    }
  }
}