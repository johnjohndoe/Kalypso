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
package org.deegree_impl.clients.wcasclient.control;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.deegree.enterprise.control.FormEvent;
import org.deegree.enterprise.control.RPCException;
import org.deegree.enterprise.control.RPCMember;
import org.deegree.enterprise.control.RPCMethodCall;
import org.deegree.enterprise.control.RPCMethodResponse;
import org.deegree.enterprise.control.RPCParameter;
import org.deegree.enterprise.control.RPCStruct;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.services.wms.protocol.WMSGetMapRequest;
import org.deegree_impl.clients.wcasclient.CatalogClientException;
import org.deegree_impl.clients.wcasclient.Constants;
import org.deegree_impl.enterprise.control.AbstractListener;
import org.deegree_impl.enterprise.control.RPCFactory;
import org.deegree_impl.enterprise.control.RPCMember_Impl;
import org.deegree_impl.enterprise.control.RPCStruct_Impl;
import org.deegree_impl.enterprise.control.RPCWebEvent;
import org.deegree_impl.graphics.transformation.WorldToScreenTransform;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.services.wms.protocol.WMSProtocolFactory;
import org.deegree_impl.tools.Debug;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class TakeSearchBoxListener extends AbstractListener
{

  /**
   * 
   * 
   * @param e
   */
  public void actionPerformed( FormEvent e )
  {
    Debug.debugMethodBegin();

    GM_Envelope searchBox = null;
    try
    {
      searchBox = validateRequest( (RPCWebEvent)e );
    }
    catch( Exception ex )
    {
      ex.printStackTrace();
      gotoErrorPage( "Could not create search rectangle: \n" + ex.toString() );
      Debug.debugMethodEnd();
      return;
    }

    handle( searchBox );

    Debug.debugMethodEnd();
  }

  /**
   * validates the request to be performed.
   * 
   * @param event
   *          event object containing the request to be performed
   */
  protected GM_Envelope validateRequest( RPCWebEvent event ) throws CatalogClientException
  {
    Debug.debugMethodBegin();

    double minx = 0;
    double miny = 0;
    double maxx = 0;
    double maxy = 0;

    RPCMethodCall call = event.getRPCMethodCall();
    RPCStruct struct = (RPCStruct)call.getParameters()[0].getValue();
    int type = ( (Integer)struct.getMember( "type" ).getValue() ).intValue();

    String mreq = (String)struct.getMember( Constants.RPC_REQUEST ).getValue();
    WMSGetMapRequest gmr = null;
    try
    {
      gmr = WMSProtocolFactory.createGetMapRequest( "id", mreq );
    }
    catch( Exception e )
    {
      throw new CatalogClientException( "counldn't create GetMap request", e );
    }

    GM_Envelope searchBox = null;
    maxx = ( (Double)struct.getMember( Constants.RPC_BBOXMAXX ).getValue() ).doubleValue();
    maxy = ( (Double)struct.getMember( Constants.RPC_BBOXMAXY ).getValue() ).doubleValue();
    minx = ( (Double)struct.getMember( Constants.RPC_BBOXMINX ).getValue() ).doubleValue();
    miny = ( (Double)struct.getMember( Constants.RPC_BBOXMINY ).getValue() ).doubleValue();
    if( type == 0 )
    {
      searchBox = calcualteSearchBox( gmr, minx, miny, maxx, maxy );
    }
    else
    {
      searchBox = GeometryFactory.createGM_Envelope( minx, miny, maxx, maxy );
    }

    Debug.debugMethodEnd();
    return searchBox;
  }

  /**
   * 
   * @param gmr
   *          GetMap request
   * @param minx
   *          pixel minx ccordinate
   * @param miny
   *          pixel miny ccordinate
   * @param maxx
   *          pixel maxx ccordinate
   * @param maxy
   *          pixel maxy ccordinate
   * 
   * @return CRS coordinates corresponding to the pixel coordinates
   */
  private GM_Envelope calcualteSearchBox( WMSGetMapRequest gmr, double minx, double miny,
      double maxx, double maxy )
  {
    Debug.debugMethodBegin();

    GM_Envelope bbox = gmr.getBoundingBox();
    GeoTransform gti = new WorldToScreenTransform( bbox.getMin().getX(), bbox.getMin().getY(), bbox
        .getMax().getX(), bbox.getMax().getY(), 0, 0, gmr.getWidth() - 1, gmr.getHeight() - 1 );
    minx = gti.getSourceX( minx );
    miny = gti.getSourceY( miny );
    maxx = gti.getSourceX( maxx );
    maxy = gti.getSourceY( maxy );

    bbox = GeometryFactory.createGM_Envelope( minx, miny, maxx, maxy );

    Debug.debugMethodEnd();
    return bbox;
  }

  /**
   * handloes the creation of the response
   * 
   * @param searchBox
   *          search area defined by the user
   */
  private void handle( GM_Envelope searchBox )
  {
    Debug.debugMethodBegin();

    // get detailed sarch paramters
    HttpSession session = ( (HttpServletRequest)this.getRequest() ).getSession( true );
    RPCParameter[] detailedsearchParams = (RPCParameter[])session
        .getAttribute( Constants.SESSION_DETAILEDSEARCHPARAM );

    RPCStruct bbox = new RPCStruct_Impl();
    RPCMember mem = new RPCMember_Impl( Double.class, "" + searchBox.getMin().getX(),
        Constants.RPC_BBOXMINX );
    bbox.addMember( mem );
    mem = new RPCMember_Impl( Double.class, "" + searchBox.getMin().getY(), Constants.RPC_BBOXMINY );
    bbox.addMember( mem );
    mem = new RPCMember_Impl( Double.class, "" + searchBox.getMax().getX(), Constants.RPC_BBOXMAXX );
    bbox.addMember( mem );
    mem = new RPCMember_Impl( Double.class, "" + searchBox.getMax().getY(), Constants.RPC_BBOXMAXY );
    bbox.addMember( mem );

    // creates a new member containing the search box defined by the user
    mem = new RPCMember_Impl( RPCStruct.class, bbox, Constants.RPC_BBOX );

    // take existing struct or create new one and add a member containing
    // the searcbox
    if( detailedsearchParams != null )
    {
      for( int i = 0; i < detailedsearchParams.length; i++ )
      {
        if( detailedsearchParams[i].getValue() instanceof RPCStruct )
        {
          RPCStruct strct = (RPCStruct)detailedsearchParams[i].getValue();
          if( strct.getMember( Constants.RPC_BBOX ) != null )
          {
            strct.removeMember( Constants.RPC_BBOX );
          }
          strct.addMember( mem );
        }
      }
    }

    // write it as paramters of a RPCResponse into the requests attribute
    // and the users session
    try
    {
      RPCMethodResponse resp = RPCFactory.createRPCMethodResponse( detailedsearchParams );
      this.getRequest().setAttribute( Constants.SESSION_DETAILEDSEARCHPARAM, resp );
      session.setAttribute( Constants.SESSION_DETAILEDSEARCHPARAM, detailedsearchParams );
    }
    catch( RPCException ex )
    {
      ex.printStackTrace();
      gotoErrorPage( "Could not create response: \n" + ex.toString() );
    }

    Debug.debugMethodEnd();
  }
}