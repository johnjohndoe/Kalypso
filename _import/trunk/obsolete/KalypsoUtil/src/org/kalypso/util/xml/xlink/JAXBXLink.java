package org.kalypso.util.xml.xlink;

import org.w3._1999.xlink.XlinkBase;

/**
 * XLink wrapper over Jaxb XlinkBase.
 * 
 * @author schlienger
 */
public class JAXBXLink implements IXlink
{
  private final XlinkBase m_xlink;

  public JAXBXLink( final XlinkBase xlink )
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
}