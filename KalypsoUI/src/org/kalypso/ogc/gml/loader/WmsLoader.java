package org.kalypso.ogc.gml.loader;

import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;
import java.net.URLConnection;
import java.util.Properties;

import org.deegree.services.wms.capabilities.WMSCapabilities;
import org.deegree_impl.services.wms.RemoteWMService;
import org.deegree_impl.services.wms.capabilities.OGCWMSCapabilitiesFactory;
import org.deegree_impl.tools.NetWorker;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.loader.AbstractLoader;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.gml.KalypsoWMSLayer;
import org.kalypso.ui.KalypsoGisPlugin;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Loads layers from a wms
 * 
 * @author lupp
 */
public class WmsLoader extends AbstractLoader
{
  public void dispose()
  {
    super.dispose();
  }

  /**
   * @see org.kalypso.loader.AbstractLoader#loadIntern(java.util.Properties, java.net.URL, org.eclipse.core.runtime.IProgressMonitor)
   */
  protected Object loadIntern( Properties source, URL context, IProgressMonitor monitor ) throws LoaderException
  {
    try
    {
      final String name = source.getProperty( "NAME", "WMS" );
      final String service = source.getProperty( "SERVICE", "" );
      final String layers = source.getProperty( "LAYERS", "" );

      final OGCWMSCapabilitiesFactory wmsCapFac = new OGCWMSCapabilitiesFactory();

      final URL url = new URL( service + "SERVICE=WMS&VERSION=1.1.1&REQUEST=GetCapabilities" );

      final URLConnection c = url.openConnection();
      NetWorker.configureProxy( c );
      
      c.addRequestProperty( "SERVICE", "WMS" );
      c.addRequestProperty( "VERSION", "1.1.1" );
      c.addRequestProperty( "REQUEST", "GetCapabilities" );
      final Reader reader = new InputStreamReader( c.getInputStream() );

      final WMSCapabilities wmsCaps = wmsCapFac.createCapabilities( reader );
      final RemoteWMService remoteWMS = new RemoteWMService( wmsCaps );
      final CS_CoordinateSystem coordinatesSystem = KalypsoGisPlugin.getDefault().getCoordinatesSystem();
      return new KalypsoWMSLayer( name, layers, coordinatesSystem, remoteWMS );
    }
    catch( Exception e )
    {
      throw new LoaderException( "wmslayer not found: " + source, e );
    }
  }

  /**
   * @see org.kalypso.loader.ILoader#getDescription()
   */
  public String getDescription()
  {
    return "WMS Layer";
  }
}