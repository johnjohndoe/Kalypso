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

 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.services.wfs;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.deegree.services.Handler;
import org.deegree.services.OGCWebServiceEvent;
import org.deegree.services.OGCWebServiceException;
import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.OGCWebServiceResponse;
import org.deegree.services.RequestController;
import org.deegree.services.wfs.DataStore;
import org.deegree.services.wfs.Dispatcher;
import org.deegree.xml.Marshallable;
import org.deegree.xml.XMLTools;
import org.deegree_impl.services.AbstractHandler;
import org.deegree_impl.services.OGCWebServiceEvent_Impl;
import org.deegree_impl.services.OGCWebServiceException_Impl;
import org.deegree_impl.services.OGCWebServiceResponse_Impl;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.StringExtend;
import org.w3c.dom.Document;

/**
 * 
 * The dispatcher acts as source of events to inform registered
 * WFSResponseListener as well as a reciever of requests that have to be
 * performed by one of the registered DataStores (also definied as
 * WFSRequestListener).
 * <p>
 * The class works as follows: When the first request is pushed to the
 * dispatcher to loops are started that ckecks continously in defined time
 * intervals (frequency) if new request and/or responses are present for
 * processing. If so the request/response is take from the container it has been
 * pushed to and will be deligated to the class that's responsible for its
 * processing.
 * <p>
 * If for a defined time interval (timeout) no request/response has been pushed
 * to the dispatcher the processing loops will be stopped. This also happens if
 * a thread based exception will be throwen during the request/response
 * processing.
 * 
 * <p>
 * -----------------------------------------------------
 * </p>
 * 
 * @author Andreas Poth
 * @version $Revision$ $Date$
 *          <p>
 */
final public class Dispatcher_Impl extends AbstractHandler implements Dispatcher
{
  protected Map reqControl = Collections.synchronizedMap( new HashMap() );

  /** container for requests to be performed */
  protected List requests = Collections.synchronizedList( new ArrayList() );

  /** container for responses to request that has be performed */
  protected List responses = Collections.synchronizedList( new ArrayList() );

  protected Loop reqThread = null;

  protected Loop resThread = null;

  /**
   * recieves a request that should be performed by the Dispatcher. The
   * Dispatcher will analyse the request and notify the responsible
   * methods/classes.
   */
  public synchronized void handleRequest( OGCWebServiceEvent event )
  {
    Debug.debugMethodBegin( this, "handleRequest" );

    // add request to the request container
    requests.add( event.getRequest() );

    RequestController reqCon = new WFSRequestController( event.getRequest() );
    reqControl.put( event.getRequest().getId(), reqCon );

    // start request processing (if not already started)
    startProcessingRequests();

    Debug.debugMethodEnd();
  }

  /**
   * recieves the response to a request that been performed by the
   * Dispatcher.The Dispatcher will analyse the response and notify the
   * responsible methods/classes.
   */
  public synchronized void handleResponse( OGCWebServiceEvent event )
  {
    Debug.debugMethodBegin( this, "handleResponse" );

    if( ( event == null ) || ( event.getResponse() == null ) )
    {
      performOGCWebServiceException( "no Response to request", " handleResponse" );
    }
    else
    {
      try
      {
        OGCWebServiceResponse response = event.getResponse();
        // remove request that's the basis of the response from
        // the 'in progress list'
        synchronized( inProgressList )
        {
          inProgressList.remove( response.getRequest() );
        }

        // get the request controller the request belongs to
        RequestController reqCon = (RequestController)reqControl.get( event.getId() );
        synchronized( reqCon )
        {
          // add the response to the request controller
          reqCon.addResponse( response );

          // check if the request performance has been finished. if so
          // get the complete response from the request controller, add it
          // to the response container of the dispatcher and start the
          // processing (if not already started)
          if( reqCon.requestFinished() )
          {
            synchronized( reqControl )
            {
              synchronized( responses )
              {
                reqControl.remove( event.getId() );
                responses.add( reqCon.getResponse() );
              }
            }

            // start response processing (if not already started)
            startProcessingResponses();
          }
        }
      }
      catch( Exception e )
      {
        Debug.debugException( e, " - " );
        StackTraceElement[] se = e.getStackTrace();
        StringBuffer sb = new StringBuffer();
        for( int i = 0; i < se.length; i++ )
        {
          sb.append( se[i].getClassName() + " " );
          sb.append( se[i].getFileName() + " " );
          sb.append( se[i].getMethodName() + "(" );
          sb.append( se[i].getLineNumber() + ")\n" );
        }
        performOGCWebServiceException( StringExtend.stackTraceToString( e.getStackTrace() ),
            " pushResponse" );
      }
    }

    Debug.debugMethodEnd();
  }

  /**
   * returns true if the handler is interested in a event
   */
  public boolean isInterested( OGCWebServiceEvent event )
  {
    boolean interested = false;
    synchronized( handlerList )
    {
      if( event.getType() == OGCWebServiceEvent.REQUEST )
      {
        for( int i = 0; i < handlerList.size(); i++ )
        {
          Handler handler = (Handler)handlerList.get( i );

          if( handler instanceof DataStore )
          {
            if( handler.isInterested( event ) )
            {
              interested = true;
              break;
            }
          }
        }
      }
      else
      {
        String id = event.getId();

        if( reqControl.get( id ) != null )
        {
          interested = true;
        }
      }
    }

    return interested;
  }

  /**
   * starts the loop for processing requests and responses
   */
  protected synchronized void startProcessingRequests()
  {
    Debug.debugMethodBegin( this, "startProcessingRequests" );

    if( reqThread == null )
    {
      reqThread = new RequestLoop();
      reqThread.start();
    }

    this.timestamp = System.currentTimeMillis();

    Debug.debugMethodEnd();
  }

  /**
   * starts the loop for processing requests and responses
   */
  protected synchronized void startProcessingResponses()
  {
    Debug.debugMethodBegin( this, "startProcessingResponses" );

    if( resThread == null )
    {
      resThread = new ResponseLoop();
      resThread.start();
    }

    this.timestamp = System.currentTimeMillis();

    Debug.debugMethodEnd();
  }

  /**
   * stops the processing loop(s)
   */
  public synchronized void stopLoops()
  {
    if( reqThread != null )
    {
      reqThread.stopLoop();
      reqThread = null;
    }

    if( resThread != null )
    {
      resThread.stopLoop();
      resThread = null;
    }
  }

  /**
   * returns the next request to be processed. This is the request that has been
   * waiting the longest time.
   */
  protected synchronized OGCWebServiceRequest getNextRequest()
  {
    //Debug.debugMethodBegin( this, "getNextRequest" );
    OGCWebServiceRequest req = null;
    synchronized( requests )
    {
      if( requests.size() > 0 )
      {
        req = (OGCWebServiceRequest)requests.remove( 0 );
      }
    }

    //Debug.debugMethodEnd();
    return req;
  }

  /**
   * returns the next response to be processed. This is the response that has
   * been waiting the longest time.
   */
  protected synchronized OGCWebServiceResponse getNextResponse()
  {
    //Debug.debugMethodBegin( this, "getNextResponse" );
    OGCWebServiceResponse resp = null;

    if( responses.size() > 0 )
    {
      resp = (OGCWebServiceResponse)responses.remove( 0 );
    }

    //Debug.debugMethodEnd();
    return resp;
  }

  /**
   * creates an exception that should be used instead of a response if the
   * performing of a request failed.
   */
  private void performOGCWebServiceException( String message, String location )
  {
    Debug.debugMethodBegin( this, "createOGCWebServiceException" );

    OGCWebServiceException exce = new OGCWebServiceException_Impl( location, message );

    Document doc = null;
    StringReader reader = new StringReader( ( (Marshallable)exce ).exportAsXML() );

    try
    {
      doc = XMLTools.parse( reader );
    }
    catch( Exception e )
    {
      System.out.println( e );
    }

    OGCWebServiceResponse response = new OGCWebServiceResponse_Impl( null, doc );

    // add exception to the response container
    fireResponse( new OGCWebServiceEvent_Impl( this, response, "A fatal exception "
        + "no request can be associated." ) );

    Debug.debugMethodEnd();
  }

  /**
   * this method is called within the loop. An extending class have to implement
   * this method.
   *  
   */
  protected void process()
  {}

  /////////////////////////////////////////////////////////////////////////
  //                          inner classes //
  /////////////////////////////////////////////////////////////////////////

  /**
   * inner class for processing requests
   */
  private class RequestLoop extends Loop
  {
    /**
     *  
     */
    protected void process()
    {
      OGCWebServiceRequest request = null;

      boolean fired = false;
      request = getNextRequest();

      if( request != null )
      {
        fired = fireRequest( new OGCWebServiceEvent_Impl( this, request, null ) );
        synchronized( inProgressList )
        {
          inProgressList.put( request, new Long( System.currentTimeMillis() ) );
        }

        // if fired equals false no registered handler is responsible
        // for this request. the calling client will be informed of this
        // by an event that contains a exception
        if( !fired )
        {
          OGCWebServiceException exce = new OGCWebServiceException_Impl( "RequestLoop: process",
              "no handler found for request " + request.toString() );

          Document doc = null;
          StringReader reader = new StringReader( ( (Marshallable)exce ).exportAsXML() );

          try
          {
            doc = XMLTools.parse( reader );
          }
          catch( Exception e )
          {
            System.out.println( e );
          }

          OGCWebServiceResponse response = new OGCWebServiceResponse_Impl( request, doc );

          reqControl.remove( request.getId() );

          OGCWebServiceEvent event = new OGCWebServiceEvent_Impl( this, response, null );

          fireResponse( event );
        }
      }
    }

    /**
     *  
     */
    protected void onStopped()
    {
      reqThread = null;
    }
  }

  /**
   * inner class for processing WFS responses to requests
   */
  private class ResponseLoop extends Loop
  {
    /**
     *  
     */
    protected void process()
    {
      OGCWebServiceResponse response = getNextResponse();

      if( response != null )
      {
        fireResponse( new OGCWebServiceEvent_Impl( this, response, null ) );
      }
    }

    /**
     *  
     */
    protected void onStopped()
    {
      resThread = null;
    }
  }

}