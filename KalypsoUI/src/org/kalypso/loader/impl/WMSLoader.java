package org.kalypso.loader.impl;

import java.net.URL;
import java.util.Properties;

import org.deegree.services.wms.capabilities.WMSCapabilities;
import org.deegree_impl.services.wms.RemoteWMService;
import org.deegree_impl.services.wms.capabilities.OGCWMSCapabilitiesFactory;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.loader.AbstractLoader;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.gml.IKalypsoLayer;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.util.pool.ResourcePool;

/**
 * Loads layers from a wms
 * 
 * @author lupp
 */
public class WMSLoader extends AbstractLoader //implements IPoolListener
{
    private final ResourcePool m_Pool = KalypsoGisPlugin.getDefault().getPool(
      IKalypsoLayer[].class );

  public WMSLoader()
  {
   //
  }

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
      final String service = source.getProperty( "SERVICE", "" );
      final String layers = source.getProperty( "LAYERS", "" );
      OGCWMSCapabilitiesFactory wmsCapFac=new OGCWMSCapabilitiesFactory();
      WMSCapabilities wmsCaps=wmsCapFac.createCapabilities(new URL(service)); 
      RemoteWMService remoteWMS=new RemoteWMService(wmsCaps);
     }
   catch( Exception e )
    {
        throw new LoaderException( "Layer not found: " + source ,e);
    }
    return null;
  }

  

  /**
   * @see org.kalypso.loader.ILoader#getDescription()
   */
  public String getDescription()
  {
    return "WMS Layer";
  }

  /**
   * @see org.kalypso.loader.AbstractLoader#save(java.util.Properties, org.eclipse.core.resources.IProject, org.eclipse.core.runtime.IProgressMonitor, java.lang.Object)
   */
  public void save( final Properties source, final IProject project, final IProgressMonitor monitor, final Object data )
      throws LoaderException
  {
  // TODO benutzer informieren, dass dies keinen Sinn macht ??
  
  }
  
  
  
}