package org.kalypso.services.observation;

import javax.servlet.ServletConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletException;

import com.sun.xml.ws.transport.http.servlet.WSServlet;
import com.sun.xml.ws.transport.http.servlet.WSServletContextListener;

/**
 * A wrapper around the {@link WSServlet} class<br>
 * This is needed, as the OSGI stuff has no concept of{@link javax.servlet.ServletContextListener}s, which are needed in
 * order to correctly initialise the servlet.
 * 
 * @author Gernot Belger
 * @author Holger Albert
 */
public class WSProxyServlet extends WSServlet
{
  private final WSServletContextListener m_servletContextListener = new WSServletContextListener();

  /**
   * @see com.sun.xml.ws.transport.http.servlet.WSServlet#init(javax.servlet.ServletConfig)
   */
  @Override
  public void init( final ServletConfig servletConfig ) throws ServletException
  {
    /* Get the servlet context. */
    final ServletContext context = servletConfig.getServletContext();

    /* Debug. */
    System.out.println( "[NNNN] WS_Proxy_Servlet: Setting JAX-WS Runtime..." );

    /* We just simulate the initialisation of the servlet. */
    m_servletContextListener.contextInitialized( new ServletContextEvent( context ) );

    super.init( servletConfig );
  }

  /**
   * @see javax.servlet.GenericServlet#destroy()
   */
  @Override
  public void destroy( )
  {
    final ServletContext servletContext = getServletContext();

    super.destroy();

    m_servletContextListener.contextDestroyed( new ServletContextEvent( servletContext ) );
  }
}