package org.kalypso.project.database.client;

import java.net.MalformedURLException;
import java.net.URL;

import javax.xml.namespace.QName;
import javax.xml.ws.Service;

import org.eclipse.core.runtime.Plugin;
import org.kalypso.project.database.common.interfaces.IProjectDatabaseAccess;
import org.kalypso.project.database.common.interfaces.implementation.ProjectDatabaseAccess;
import org.kalypso.project.database.sei.IProjectDatabase;
import org.kalypso.project.database.server.ProjectDatabase;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class KalypsoProjectDatabaseClient extends Plugin
{

  private static IProjectDatabase m_service;

  static
  {
    try
    {
      final String namespaceURI = "http://server.database.project.kalypso.org/";
      final String serviceImplName = ProjectDatabase.class.getSimpleName();

// final String wsdlLocationProperty = System.getProperty( "kalypso.hwv.observation.service.client.wsdl.location" );
      // TODO: get from outside
      final String wsdlLocationProperty = "http://localhost/projectdb?wsdl";
      final URL wsdlLocation = new URL( wsdlLocationProperty );
      final QName serviceName = new QName( namespaceURI, serviceImplName + "Service" );
      final Service service = Service.create( wsdlLocation, serviceName );

      m_service = service.getPort( new QName( namespaceURI, serviceImplName + "Port" ), IProjectDatabase.class );
    }
    catch( MalformedURLException e )
    {
      e.printStackTrace();
    }

  }

  public static IProjectDatabase getService( )
  {
    return m_service;
  }

  // The plug-in ID
  public static final String PLUGIN_ID = "org.kalypso.project.database.client";

  // The shared instance
  private static KalypsoProjectDatabaseClient plugin;

  private static ProjectDatabaseAccess INCOMING_ACCESS_DATA = null;

  private ProjectDatabaseAccess PROJECT_ACCESS_DATA;

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
  public void start( BundleContext context ) throws Exception
  {
    super.start( context );
    plugin = this;
  }

  /*
   * (non-Javadoc)
   * @see org.eclipse.core.runtime.Plugin#stop(org.osgi.framework.BundleContext)
   */
  @Override
  public void stop( BundleContext context ) throws Exception
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

  public IProjectDatabaseAccess getIncomingAccessData( )
  {
    if( INCOMING_ACCESS_DATA == null )
    {
      INCOMING_ACCESS_DATA = new ProjectDatabaseAccess();

      String protocol = System.getProperty( IProjectDatabaseAccess.INCOMING_PROTOCOL );
      String username = System.getProperty( IProjectDatabaseAccess.INCOMING_USER_NAME );
      String password = System.getProperty( IProjectDatabaseAccess.INCOMING_PASSWORD );
      String url = System.getProperty( IProjectDatabaseAccess.INCOMING_URL );

      INCOMING_ACCESS_DATA.setProtocol( protocol );
      INCOMING_ACCESS_DATA.setUsername( username );
      INCOMING_ACCESS_DATA.setPassword( password );
      INCOMING_ACCESS_DATA.setUrl( url );
    }

    return INCOMING_ACCESS_DATA;
  }

  public IProjectDatabaseAccess getProjectAccessData( )
  {
    if( PROJECT_ACCESS_DATA == null )
    {
      PROJECT_ACCESS_DATA = new ProjectDatabaseAccess();

      String protocol = System.getProperty( IProjectDatabaseAccess.PROJECT_BASE_PROTOCOL );
      String username = System.getProperty( IProjectDatabaseAccess.PROJECT_BASE_USER_NAME );
      String password = System.getProperty( IProjectDatabaseAccess.PROJECT_BASE_PASSWORD );
      String url = System.getProperty( IProjectDatabaseAccess.PROJECT_BASE_URL );

      PROJECT_ACCESS_DATA.setProtocol( protocol );
      PROJECT_ACCESS_DATA.setUsername( username );
      PROJECT_ACCESS_DATA.setPassword( password );
      PROJECT_ACCESS_DATA.setUrl( url );
    }

    return PROJECT_ACCESS_DATA;
  }
}
