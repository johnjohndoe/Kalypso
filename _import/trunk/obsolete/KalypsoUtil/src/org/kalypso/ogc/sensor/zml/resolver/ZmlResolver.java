package org.kalypso.ogc.sensor.zml.resolver;

import java.util.Properties;

import org.kalypso.java.properties.PropertiesHelper;
import org.kalypso.ogc.sensor.DefaultObservationProvider;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.zml.ZmlObservation;
import org.kalypso.util.xml.xlink.IXlink;
import org.kalypso.util.xml.xlink.XLinkException;
import org.kalypso.util.xml.xlink.resolver.IResolver;

/**
 * Resolves to a IObsevationProvider for ZmlObservations.
 * 
 * @author schlienger
 */
public class ZmlResolver implements IResolver
{
  public ZmlResolver()
  {
    // nix
  }

  /**
   * @see org.kalypso.util.xml.xlink.resolver.IResolver#resolve(org.kalypso.util.xml.xlink.IXlink)
   */
  public Object resolve( IXlink link ) throws XLinkException
  {
    // parse the href in order to know which axis is desired
    Properties props = PropertiesHelper.parseFromString( link.getHRef(), '#' );
    
    ZmlObservation obs = null;
    
    try
    {
      obs = new ZmlObservation( link.getHRef(), link.toUrl().openStream() );
    }
    catch( Exception e )
    {
      throw new XLinkException( e );
    }

    return new DefaultObservationProvider( obs, ObservationUtilities.findAxis( obs, (String)props.get("AXIS") ) );
  }
}
