package org.kalypso.ui.wizard.wms.utils;

import java.net.URL;

import org.deegree.ogcwebservices.wms.capabilities.WMSCapabilities;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.ogc.gml.wms.deegree.DeegreeWMSUtilities;
import org.kalypso.ogc.gml.wms.loader.ICapabilitiesLoader;
import org.kalypso.ogc.gml.wms.provider.images.IKalypsoImageProvider;
import org.kalypso.ogc.gml.wms.utils.KalypsoWMSUtilities;

/**
 * Small runnable, which loads the capabilities from a WMS.
 * 
 * @author Holger Albert
 */
public class CapabilitiesGetter implements ICoreRunnableWithProgress
{
  /**
   * The service URL.
   */
  private final URL m_service;

  /**
   * The capabilities.
   */
  private WMSCapabilities m_capabilities = null;

  /**
   * The ID of the provider.
   */
  private final String m_providerID;

  public CapabilitiesGetter( final URL service, final String providerID )
  {
    m_service = service;
    m_providerID = providerID;
  }

  /**
   * This function starts the loading of the capabilities.
   * 
   * @param monitor
   *            A progress monitor.
   * @return A status containing the result.
   */
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    /*
     * This image provider should do nothing, but create the necessary image loader, so we need not to provide anything,
     * except the providerID.
     */
    final IKalypsoImageProvider imageProvider = KalypsoWMSUtilities.getImageProvider( "", null, null, "", m_providerID );

    /* Get the loader. */
    final ICapabilitiesLoader loader = imageProvider.getLoader();

    /* Init the loader. */
    loader.init( m_service );

    /* Load the capabilities. */
    final WMSCapabilities capabilities = DeegreeWMSUtilities.loadCapabilities( loader, monitor );

    m_capabilities = capabilities;

    return Status.OK_STATUS;
  }

  /**
   * This function returns the last retrieved capabilities.
   * 
   * @return The last retrieved capabilities.
   */
  public WMSCapabilities getCapabilities( )
  {
    return m_capabilities;
  }
}