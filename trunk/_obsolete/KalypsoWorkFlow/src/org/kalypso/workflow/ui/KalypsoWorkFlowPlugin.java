package org.kalypso.workflow.ui;

import org.eclipse.ui.plugin.*;
import org.eclipse.jface.resource.ImageDescriptor;
import org.kalypso.workflow.WorkflowContext;
import org.osgi.framework.BundleContext;

/**
 * The main plugin class to be used in the desktop.
 */
public class KalypsoWorkFlowPlugin extends AbstractUIPlugin
{

  // The shared instance.
  private static KalypsoWorkFlowPlugin plugin;

  private WorkflowContext m_workflowContext;

  /**
   * The constructor.
   */
  public KalypsoWorkFlowPlugin( )
  {
    plugin = this;
    m_workflowContext = new WorkflowContext();
  }

  /**
   * This method is called upon plug-in activation
   */
  @Override
  public void start( BundleContext context ) throws Exception
  {
    super.start( context );
  }

  /**
   * This method is called when the plug-in is stopped
   */
  @Override
  public void stop( BundleContext context ) throws Exception
  {
    super.stop( context );
    plugin = null;
  }

  /**
   * Returns the shared instance.
   */
  public static KalypsoWorkFlowPlugin getDefault( )
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
  public static ImageDescriptor getImageDescriptor( String path )
  {
    return AbstractUIPlugin.imageDescriptorFromPlugin( "KalypsoWorkFlow", path );
  }

  public WorkflowContext getDefaultWorkflowContext( )
  {
    return m_workflowContext;
  }
}
