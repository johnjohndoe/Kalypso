package org.kalypso.util.xml.xlink;

import java.net.MalformedURLException;
import java.net.URL;

/**
 * An interface for the XLink-Base specification
 * 
 * @author schlienger
 */
public interface IXlink
{
  public String getType();
  public String getActuate();
  public String getHRef();
  
  public URL toUrl() throws MalformedURLException;
}
