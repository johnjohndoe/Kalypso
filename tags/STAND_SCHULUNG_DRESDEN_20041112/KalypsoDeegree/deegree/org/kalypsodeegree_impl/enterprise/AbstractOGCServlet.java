/*----------------    FILE HEADER  ------------------------------------------
 
 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de
 
 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.
 
 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.
 
 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 
 Contact:
 
 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Jens Fitzke 
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn 
 Germany
 E-Mail: jens.fitzke@uni-bonn.de

 ----------------------------------------------------------------------------*/
package org.deegree_impl.enterprise;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.URL;
import java.util.Enumeration;
import java.util.HashMap;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.deegree.services.OGCWebServiceException;
import org.deegree.xml.Marshallable;
import org.deegree_impl.services.OGCWebServiceException_Impl;
import org.deegree_impl.tools.Cleaner;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.StringExtend;

/**
 * Abstract servlet that serves as an OCC-compliant HTTP-frontend to any
 * OGC-WebService (WFS, WMS, ...).
 * <p>
 * 
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
public abstract class AbstractOGCServlet extends HttpServlet
{

  // Reference to file alteration monitor Thread. */
  public Reloader reloader = null;

  // The capabilities file of the underlying OGC-Service.
  protected URL capabilitiesURL = null;

  /**
   * Called by the servlet container to indicate that the servlet is being
   * placed into service. Sets the debug level according to the debug parameter
   * defined in the ServletEngine's environment.
   * <p>
   * NOTE: Everything that gets called by this method should not use Debug.xxx ()
   * to produce messages, but 'getServletContext.log ()'.
   * <p>
   * 
   * @param servletConfig
   *          servlet configuration
   * @throws ServletException
   *           exception if something occurred that interferes with the
   *           servlet's normal operation
   */
  public void init( ServletConfig servletConfig ) throws ServletException
  {
    super.init( servletConfig );
    Debug.setLevel( getInitParameter( "debug" ) );
    initService();
    new Cleaner( 120 * 1000 );
  }

  /**
   * Called by the servlet container to indicate to a servlet that the servlet
   * is being taken out of service. Stops the Reloader Thread, if running.
   */
  public void destroy()
  {
    if( reloader != null )
    {
      reloader.decease();
    }
  }

  /**
   * Returns HTTP-parameters of a HttpServletRequest as a HashMap object.
   * 
   * @param request
   *          source of the data
   * @return keys are parameter-names, values are parameter values
   */
  protected HashMap getParamMap( HttpServletRequest request )
  {
    HashMap param = new HashMap();

    Enumeration enum = request.getParameterNames();
    while( enum.hasMoreElements() )
    {
      String name = (String)enum.nextElement();
      String value = request.getParameter( name );
      param.put( name.toUpperCase(), value );
    }

    return param;
  }

  /**
   * Enables reloading mechanism on configuration file changes. Runs as a
   * separate Thread. The file that is monitored is specified by the init
   * parameter "capabilities" in the servlet container. Monitor frequency can be
   * adjusted by the parameter "updateFrequency".
   */
  protected void enableReloader()
  {
    if( reloader != null )
    {
      reloader.decease();
    }
    reloader = new Reloader( getInitParameter( "capabilities" ),
        getInitParameter( "updateFrequency" ) );
  }

  /**
   * Called when a reinitialization of the service is necessary, e.g. the
   * configuration file has been altered.
   */
  protected abstract void initService();

  /**
   * handles fatal errors by creating a OGC exception XML and sending it back to
   * the client
   */
  protected void handleError( Exception ex, HttpServletResponse response )
  {
    String tmp = StringExtend.stackTraceToString( ex.getStackTrace() );
    getServletContext().log( tmp );
    OGCWebServiceException wex = new OGCWebServiceException_Impl( this.getClass().getName(), tmp );
    try
    {
      PrintWriter pw = response.getWriter();
      pw.write( ( (Marshallable)wex ).exportAsXML() );
      pw.close();
    }
    catch( Exception e )
    {
      getServletContext().log( e.toString() );
    }
  }

  //////////////////////////////////////////////////////////////////////////
  //                           inner classes //
  //////////////////////////////////////////////////////////////////////////

  /**
   * Inner class that is used to monitor the configuration file
   * (capabilities-file). If the file is altered, the service is signalled to
   * reinitialize itself by a call to initService ().
   */
  protected class Reloader extends Thread
  {

    private long lastTS = 0;

    private int freq = 60 * 1000;

    private boolean doExit = false;

    /**
     * Constructs a new Reloader-Object. It will be started as a separate
     * thread.
     * 
     * @param capabilities
     *          file that should be monitored
     * @param freqStr
     *          check interval in seconds (as String)
     */
    protected Reloader( String capabilities, String freqStr )
    {

      // evaluate interval String
      if( freqStr == null )
      {
        getServletContext().log(
            "No frequency (interval) specified, defaulting to 60 " + "seconds." );
      }
      else
      {
        try
        {
          freq = Integer.parseInt( freqStr ) * 1000;
        }
        catch( NumberFormatException e )
        {
          getServletContext().log(
              "Invalid frequency (interval) specification: " + freqStr
                  + ", defaulting to 60 seconds.", e );
        }
      }
      // check for existence of file to monitor
      if( capabilities == null )
      {
        getServletContext().log( "No capabilities file to monitor specified. Exiting." );
      }
      try
      {
        capabilitiesURL = new URL( capabilities );
        File file = new File( capabilitiesURL.getFile() );
        if( !file.exists() )
        {
          getServletContext().log(
              "File to monitor: ('" + capabilities + "') " + "does not exist. Exiting." );
        }
        lastTS = file.lastModified();
        this.start();
      }
      catch( IOException e )
      {
        getServletContext().log( "Error setting up file monitor!", e );
      }
      catch( IllegalThreadStateException e )
      {
        getServletContext().log( "Reloader already started!?", e );
      }
      getServletContext().log(
          "Reloader-Thread started. Monitoring file: '" + capabilities + "', Check interval: "
              + freq + " (milliseconds)" );
      reloader = this;
    }

    /**
     * Kills the Reloader-Thread in a safe fashion.
     */
    public void decease()
    {
      doExit = true;
    }

    /**
     * Called internally when this Thread is started.
     */
    public void run()
    {

      while( !doExit )
      {
        try
        {
          sleep( freq );
          File file = new File( capabilitiesURL.getFile() );
          long currTS = file.lastModified();
          if( lastTS != currTS )
          {
            getServletContext().log(
                "Configuration alteration detected: reloading " + "service... " );
            System.out.println( "Configuration alteration detected: reloading " + "service... " );
            lastTS = currTS;
            initService();
          }
        }
        catch( InterruptedException e )
        {
          getServletContext().log( "Reloader interrupted.", e );
        }
      }
    }
  }
}