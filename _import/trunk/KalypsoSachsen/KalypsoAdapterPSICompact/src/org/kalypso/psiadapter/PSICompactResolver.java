package org.kalypso.psiadapter;

import org.kalypso.util.xml.xlink.IXlink;
import org.kalypso.util.xml.xlink.XLinkException;
import org.kalypso.util.xml.xlink.resolver.IResolver;

/**
 * @author schlienger
 *
 */
public class PSICompactResolver implements IResolver
{

  /**
   *
   */

  public PSICompactResolver()
  {
    super();

  }

  /**
   * @see org.kalypso.util.xml.xlink.resolver.IResolver#resolve(org.kalypso.util.xml.xlink.IXlink)
   */
  public Object resolve( IXlink link ) throws XLinkException
  {
    return null;
  }

}
