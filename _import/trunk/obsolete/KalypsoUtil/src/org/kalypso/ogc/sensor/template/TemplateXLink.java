package org.kalypso.ogc.sensor.template;

import java.net.MalformedURLException;
import java.net.URL;

import org.kalypso.java.properties.PropertiesHelper;
import org.kalypso.util.xml.xlink.JaxbXlink;
import org.w3._1999.xlink.XlinkBase;

/**
 * A specific XLink that knows about parsing the HRef in order to create a valid URL.
 * 
 * @author schlienger
 */
public class TemplateXLink extends JaxbXlink
{
  public TemplateXLink( XlinkBase xlink )
  {
    super( xlink );
  }

  /**
   * @see org.kalypso.util.xml.xlink.JaxbXlink#toUrl()
   */
  public URL toUrl() throws MalformedURLException
  {
    String source = PropertiesHelper.parseFromString( getHRef(), '#' ).getProperty("SOURCE");
    
    return new URL( source );
  }
}
