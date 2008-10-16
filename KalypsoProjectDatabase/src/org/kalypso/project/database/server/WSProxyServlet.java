package org.kalypso.project.database.server;

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

// private WSServletDelegate m_delegate;

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

    /* We just simulate the initialization of the servlet. */
    m_servletContextListener.contextInitialized( new ServletContextEvent( context ) );

    // REMARK: in order to get rid of this context listener; and hence in order to move this code to a common place
    // we must register the servlet adapter like shown in the following lines (copied from
    // DeploymentDescriptorParser)
// ServletAdapterList factory = new ServletAdapterList();
//
// Class implementorClass;
// QName serviceName = new QName("http://server.database.project.kalypso.org/","ProjectDatabaseService");
// QName portName = new QName("http://server.database.project.kalypso.org/","ProjectDatabasePort");
// Container container;
// WSBinding binding;
// WSEndpoint<?> endpoint = WSEndpoint.create(
// implementorClass, true,
// null,
// serviceName, portName, container, binding,
// null, new ArrayList<Object>(), createEntityResolver(),false
// );
//
// ServletAdapter servletAdapter = factory.createAdapter( "projectDatabase", "/projectdb", endpoint );
//
//
// List<ServletAdapter> adapters = new ArrayList<ServletAdapter>(1);
// m_delegate = new WSServletDelegate(adapters,context);
// context.setAttribute(WSServlet.JAXWS_RI_RUNTIME_INFO,m_delegate);

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
// m_delegate.destroy();
  }
}