package org.kalypso.util.xml.xlink.resolver;

import org.kalypso.util.xml.xlink.IXlink;
import org.kalypso.util.xml.xlink.XLinkException;

/**
 * A resolver for the XLink. Given an XLink it can resolve the linked object.
 * 
 * @author schlienger
 */
public interface IResolver
{
  public Object resolve( IXlink link ) throws XLinkException;
}
