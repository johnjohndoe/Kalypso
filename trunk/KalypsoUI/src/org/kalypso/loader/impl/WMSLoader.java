package org.kalypso.loader.impl;

import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;
import java.net.URLConnection;
import java.util.Properties;

import org.deegree.services.wms.capabilities.WMSCapabilities;
import org.deegree_impl.services.wms.RemoteWMService;
import org.deegree_impl.services.wms.capabilities.OGCWMSCapabilitiesFactory;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.loader.AbstractLoader;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.gml.KalypsoWMSLayer;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Loads layers from a wms
 * 
 * @author lupp
 */
public class WMSLoader extends AbstractLoader
{
  public void dispose()
  {
    super.dispose();
  }

  /**
   * @see org.kalypso.loader.AbstractLoader#loadIntern(java.util.Properties, org.eclipse.core.resources.IProject, org.eclipse.core.runtime.IProgressMonitor)
   */
  public Object loadIntern( final Properties source, final IProject project,
      final IProgressMonitor monitor ) throws LoaderException
  {
    try
    {
      final String name = source.getProperty( "NAME", "WMS" );
      final String service = source.getProperty( "SERVICE", "" );
      final String layers = source.getProperty( "LAYERS", "" );
     
      OGCWMSCapabilitiesFactory wmsCapFac=new OGCWMSCapabilitiesFactory();
      
   
      URL url=new URL(service+"SERVICE=WMS&VERSION=1.1.1&REQUEST=GetCapabilities");
        
    
      URLConnection c = KalypsoGisPlugin.getDefault().getURLConnectionFactory().createURLConnection(url);
      // TODO put this somewhere else
      
      c.addRequestProperty("SERVICE","WMS");
      c.addRequestProperty("VERSION","1.1.1");
      c.addRequestProperty("REQUEST","GetCapabilities");
      Reader reader = new InputStreamReader( c.getInputStream() );
            
      WMSCapabilities wmsCaps=wmsCapFac.createCapabilities(reader);
      RemoteWMService remoteWMS=new RemoteWMService(wmsCaps);
      CS_CoordinateSystem coordinatesSystem = KalypsoGisPlugin.getDefault().getCoordinatesSystem();
     return new KalypsoWMSLayer(name,layers,coordinatesSystem,remoteWMS);
    }
   catch( Exception e )
    {
        throw new LoaderException( "wmslayer not found: " + source ,e);
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