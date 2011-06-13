package org.kalypso.model.product;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.model.product.utils.ConfigPropertyUtilities;
import org.osgi.framework.BundleContext;

/**
 * The main plugin class to be used in the desktop.
 */
public class KalypsoModelProductPlugin extends AbstractUIPlugin
{
  public static final String ALWAYS_SHOW_INTRO_ON_START = "alwaysShowIntroOnStart"; //$NON-NLS-1$

  // The shared instance.
  private static KalypsoModelProductPlugin plugin;

  private FormToolkit m_formToolkit;

  /**
   * The constructor.
   */
  public KalypsoModelProductPlugin( )
  {
    plugin = this;
  }

  /**
   * This method is called upon plug-in activation
   */
  @Override
  public void start( final BundleContext context ) throws Exception
  {
    super.start( context );
  }

  /**
   * This function returns the form toolkit for the Planer-Client.
   * 
   * @return The form toolkit.
   */
  public FormToolkit getToolkit( )
  {
    if( m_formToolkit == null )
      m_formToolkit = new FormToolkit( PlatformUI.getWorkbench().getDisplay() );

    return m_formToolkit;
  }

  /**
   * This function returns the form toolkit for the Planer-Client.
   * 
   * @return The form toolkit.
   */
  public static FormToolkit getFormToolkit( )
  {
    return getDefault().getToolkit();
  }

  /**
   * This method is called when the plug-in is stopped
   */
  @Override
  public void stop( final BundleContext context ) throws Exception
  {
    super.stop( context );
    plugin = null;
  }

  /**
   * Returns the shared instance.
   */
  public static KalypsoModelProductPlugin getDefault( )
  {
    return plugin;
  }

  /**
   * Returns an image descriptor for the image file at the given plug-in relative path.
   * 
   * @param path
   *          the path
   * @return the image descriptor
   */
  public static ImageDescriptor getImageDescriptor( final String path )
  {
    return AbstractUIPlugin.imageDescriptorFromPlugin( "org.kalypso.model.product", path ); //$NON-NLS-1$
  }

  /**
   * This function returns true, if the logged on user has expert privileges.
   * 
   * @return True, if the user is an expert.
   */
  public boolean isExpert( )
  {
    return ConfigPropertyUtilities.isExpert();
  }

}
