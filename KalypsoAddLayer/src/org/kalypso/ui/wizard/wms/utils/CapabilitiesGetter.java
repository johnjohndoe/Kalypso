package org.kalypso.ui.wizard.wms.utils;

import java.net.URL;

import org.deegree.ogcwebservices.wms.capabilities.WMSCapabilities;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.ogc.gml.wms.deegree.DeegreeWMSUtilities;
import org.kalypso.ogc.gml.wms.loader.WMSCapabilitiesLoader;

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
  private URL m_service;

  /**
   * The capabilities.
   */
  private WMSCapabilities m_capabilities = null;

  public CapabilitiesGetter( URL service )
  {
    super();

    m_service = service;
  }

  /**
   * This function starts the loading of the capabilities.
   * 
   * @param monitor
   *            A progress monitor.
   * @return A status containing the result.
   */
  public IStatus execute( IProgressMonitor monitor ) throws CoreException
  {
    // TODO Decide in the wizard, which capabilities loader should be used (use the combo box there for).
    WMSCapabilities capabilities = DeegreeWMSUtilities.loadCapabilities( new WMSCapabilitiesLoader( m_service, 10000 ), monitor );

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