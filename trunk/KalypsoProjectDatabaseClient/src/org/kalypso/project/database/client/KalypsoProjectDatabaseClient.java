package org.kalypso.project.database.client;

import java.net.URL;

import javax.xml.namespace.QName;
import javax.xml.ws.Service;
import javax.xml.ws.WebServiceException;

import org.eclipse.core.runtime.Plugin;
import org.kalypso.project.database.client.core.model.ProjectDatabaseModel;
import org.kalypso.project.database.sei.IProjectDatabase;
import org.kalypso.project.database.server.ProjectDatabase;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class KalypsoProjectDatabaseClient extends Plugin
{
  private ProjectDatabaseModel PROJECT_DATABASE_MODEL = null;

  private static IProjectDatabase m_service = null;

  public static synchronized IProjectDatabase getService( ) throws WebServiceException
  {
    if( m_service == null )
    {
      try
      {
        final String namespaceURI = "http://server.database.project.kalypso.org/";
        final String serviceImplName = ProjectDatabase.class.getSimpleName();

        final String wsdlLocationProperty = System.getProperty( IProjectDataBaseClientConstant.SERVER_WSDL_LOCATION );
        final URL wsdlLocation = new URL( wsdlLocationProperty );
        final QName serviceName = new QName( namespaceURI, serviceImplName + "Service" );

        final Service service = Service.create( wsdlLocation, serviceName );
        m_service = service.getPort( new QName( namespaceURI, serviceImplName + "Port" ), IProjectDatabase.class );

      }
      catch( final Throwable e )
      {
        // TODO trace option
      }
    }

    return m_service;
  }

  // The plug-in ID
  public static final String PLUGIN_ID = "org.kalypso.project.database.client";

  // The shared instance
  private static KalypsoProjectDatabaseClient plugin;

  /**
   * The constructor
   */
  public KalypsoProjectDatabaseClient( )
  {
  }

  /*
   * (non-Javadoc)
   * @see org.eclipse.core.runtime.Plugins#start(org.osgi.framework.BundleContext)
   */
  @Override
  public void start( final BundleContext context ) throws Exception
  {
    super.start( context );
    plugin = this;
  }

  /*
   * (non-Javadoc)
   * @see org.eclipse.core.runtime.Plugin#stop(org.osgi.framework.BundleContext)
   */
  @Override
  public void stop( final BundleContext context ) throws Exception
  {
    plugin = null;
    super.stop( context );
  }

  /**
   * Returns the shared instance
   * 
   * @return the shared instance
   */
  public static KalypsoProjectDatabaseClient getDefault( )
  {
    return plugin;
  }

  public ProjectDatabaseModel getProjectDatabaseModel( )
  {
    /* don't implement ProjectdatabaseModel() as Singleton, perhaps we have to flexibilise the model in future */
    if( PROJECT_DATABASE_MODEL == null )
      PROJECT_DATABASE_MODEL = new ProjectDatabaseModel();

    return PROJECT_DATABASE_MODEL;
  }
}
