package org.kalypso.project.database;

import java.net.MalformedURLException;
import java.net.URL;

import javax.xml.namespace.QName;
import javax.xml.ws.Service;

import org.eclipse.core.runtime.Plugin;
import org.hibernate.SessionFactory;
import org.hibernate.cfg.AnnotationConfiguration;
import org.kalypso.project.database.sei.IProjectDatabase;
import org.kalypso.project.database.sei.beans.KalypsoProjectBean;
import org.kalypso.project.database.server.ProjectDatabase;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class KalypsoProjectDatabase extends Plugin
{

  private static SessionFactory FACTORY = null;
  static
  {
    if( FACTORY == null )
    {
      try
      {
        URL url = KalypsoProjectDatabase.class.getResource( "server/conf/hibernate.cfg.xml" ).toURI().toURL();
        AnnotationConfiguration configure = new AnnotationConfiguration().configure( url );

        ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
        Thread.currentThread().setContextClassLoader( KalypsoProjectDatabase.class.getClassLoader() );

        configure.addAnnotatedClass( KalypsoProjectBean.class );

        Thread.currentThread().setContextClassLoader( classLoader );

        FACTORY = configure.buildSessionFactory();

      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }
  }

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

  public static SessionFactory getSessionFactory( )
  {
    return FACTORY;
  }

  // The plug-in ID
  public static final String PLUGIN_ID = "org.kalypso.project.database";

  // The shared instance
  private static KalypsoProjectDatabase plugin;

  /**
   * The constructor
   */
  public KalypsoProjectDatabase( )
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
  public static KalypsoProjectDatabase getDefault( )
  {
    return plugin;
  }

}
