package org.deegree_impl.clients.gazetteer.control;

import java.io.IOException;
import java.net.URL;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.deegree.enterprise.control.FormEvent;
import org.deegree_impl.clients.gazetteer.configuration.GazetteerClientConfiguration;

public class GazetteerClientRequestDispatcher extends
    org.deegree_impl.enterprise.control.RequestDispatcher
{

  public static String ERROR_PAGE = "error.jsp";

  /**
   * This method initializes the servlet.
   * 
   * @param cfg
   *          the servlet configuration
   * 
   * @throws ServletException
   *           an exception
   */
  public void init( ServletConfig cfg ) throws ServletException
  {
    super.init( cfg );

    try
    {
      // initialize configuration of client and data servers
      GazetteerClientConfiguration.getInstance( new URL( getInitParameter( "Client.configFile" ) ) );
    }
    catch( Exception e )
    {
      System.out.println( e );
      throw new ServletException( "" + e );
    }

    if( getInitParameter( "Client.errorPage" ) != null )
    {
      ERROR_PAGE = getInitParameter( "Client.errorPage" );
    }
  }

  protected void service( HttpServletRequest request, HttpServletResponse response )
      throws ServletException, IOException
  {

    // create event out of request
    FormEvent _event = createEvent( request );

    // deliver event to application handler
    deliverEvent( _event );

    // get next page from request attribute
    String nextPage = (String)request.getAttribute( "next" );

    // show error page if next page is null
    if( nextPage == null )
      nextPage = ERROR_PAGE;
    nextPage = "/" + nextPage;

    if( request.getAttribute( "javax.servlet.jsp.jspException" ) != null )
    {
      nextPage = "/" + ERROR_PAGE;
    }

    request.setAttribute( "CONFIGURATION", GazetteerClientConfiguration.getInstance() );

    // call request dispatcher
    getServletConfig().getServletContext().getRequestDispatcher( nextPage ).forward( request,
        response );
    _event = null;
  }
}