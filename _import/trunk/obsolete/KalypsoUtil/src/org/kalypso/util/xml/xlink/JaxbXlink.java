package org.kalypso.util.xml.xlink;

import java.net.MalformedURLException;
import java.net.URL;

import org.w3._1999.xlink.XlinkBase;

/**
 * A simple implementation of the IXLink interface based on the XlinkBase generated
 * by JAXB.
 * 
 * @author schlienger
 */
public class JaxbXlink implements IXlink
{
  private final XlinkBase m_xlink;
  
  public JaxbXlink( final XlinkBase xlink )
  {
    m_xlink = xlink;
  }

  /**
   * @see org.kalypso.util.xml.xlink.IXlink#getType()
   */
  public String getType()
  {
    return m_xlink.getType();
  }

  /**
   * @see org.kalypso.util.xml.xlink.IXlink#getActuate()
   */
  public String getActuate()
  {
    return m_xlink.getActuate();
  }

  /**
   * @see org.kalypso.util.xml.xlink.IXlink#getHRef()
   */
  public String getHRef()
  {
    return m_xlink.getHref();
  }

  /**
   * @throws MalformedURLException
   * @see org.kalypso.util.xml.xlink.IXlink#toUrl()
   */
  public URL toUrl() throws MalformedURLException
  {
    return new URL( getHRef() );
  }
}
