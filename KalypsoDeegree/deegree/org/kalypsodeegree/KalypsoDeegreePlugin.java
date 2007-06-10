package org.kalypsodeegree;

import org.eclipse.core.runtime.Plugin;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.PlatformUI;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypsodeegree.model.TypeHandlerUtilities;
import org.osgi.framework.BundleContext;

public class KalypsoDeegreePlugin extends Plugin
{
  // The shared instance.
  private static KalypsoDeegreePlugin m_plugin;

  public KalypsoDeegreePlugin( )
  {
    super();
    m_plugin = this;
  }

  /**
   * @see org.eclipse.core.runtime.Plugin#start(org.osgi.framework.BundleContext)
   */
  @Override
  public void start( final BundleContext context ) throws Exception
  {
    super.start( context );
    
    registerTypeHandler();
  }

  /**
   * @see org.eclipse.core.runtime.Plugin#stop(org.osgi.framework.BundleContext)
   */
  @Override
  public void stop( final BundleContext context ) throws Exception
  {
    super.stop( context );
    m_plugin = null;
  }

  /**
   * Returns the shared instance.
   */
  public static KalypsoDeegreePlugin getDefault( )
  {
    return m_plugin;
  }

  /**
   * TODO: move this to the gml-schema plugin TODO: make extension point for registering type handlers!
   */
  private void registerTypeHandler( )
  {
    final ITypeRegistry<IMarshallingTypeHandler> marshallingRegistry = MarshallingTypeRegistrySingleton.getTypeRegistry();

    try
    {
      if( marshallingRegistry != null )
      {
        TypeHandlerUtilities.registerXSDSimpleTypeHandler( marshallingRegistry );
        TypeHandlerUtilities.registerTypeHandlers( marshallingRegistry );
      }
    }
    catch( final Throwable e ) // generic exception caught for simplicity
    {
      getLog().log( StatusUtilities.statusFromThrowable( e, "Failed to register type handlers" ) );
      // this method is also used in headless mode
      if( PlatformUI.isWorkbenchRunning() )
        MessageDialog.openError( PlatformUI.getWorkbench().getDisplay().getActiveShell(), "Interne Applikationsfehler", e.getLocalizedMessage() );
    }
  }

}
