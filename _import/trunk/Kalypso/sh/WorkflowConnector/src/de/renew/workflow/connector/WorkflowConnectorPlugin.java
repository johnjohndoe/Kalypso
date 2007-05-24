package de.renew.workflow.connector;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.Properties;

import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.BundleContext;

import de.renew.workflow.cases.Case;
import de.renew.workflow.connector.context.ActiveWorkContext;
import de.renew.workflow.connector.context.SimpleCaseHandlingProjectNature;

/**
 * The activator class controls the plug-in life cycle
 */
public class WorkflowConnectorPlugin extends Plugin
{

  // The plug-in ID
  public static final String PLUGIN_ID = "de.renew.workflow.connector";

  private static final String ACTIVE_WORKCONTEXT_MEMENTO = "activeWorkContext";

  private static WorkflowConnectorPlugin plugin;

  private ActiveWorkContext<Case> m_activeWorkContext;

  /**
   * The constructor
   */
  public WorkflowConnectorPlugin( )
  {
    plugin = this;
  }

  /**
   * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
   */
  @Override
  public void start( BundleContext context ) throws Exception
  {
    super.start( context );
    Properties properties = new Properties();
    final String fileName = getStateLocation().append( ACTIVE_WORKCONTEXT_MEMENTO ).toOSString();
    final File file = new File( fileName );
    if( file.exists() )
    {
      properties.loadFromXML( new FileInputStream( file ) );
    }
    m_activeWorkContext = new ActiveWorkContext<Case>( properties, SimpleCaseHandlingProjectNature.ID );
  }

  /**
   * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
   */
  @Override
  public void stop( BundleContext context ) throws Exception
  {
    plugin = null;
    final Properties properties = m_activeWorkContext.createProperties();
    final String fileName = getStateLocation().append( ACTIVE_WORKCONTEXT_MEMENTO ).toOSString();
    final File file = new File( fileName );
    if( file.exists() )
    {
      file.delete();
    }
    properties.storeToXML( new FileOutputStream( file ), "" );
    m_activeWorkContext = null;
    super.stop( context );
  }

  /**
   * Returns the shared instance
   * 
   * @return the shared instance
   */
  public static WorkflowConnectorPlugin getDefault( )
  {
    return plugin;
  }

  public ActiveWorkContext<Case> getActiveWorkContext( )
  {
    return m_activeWorkContext;
  }

}
