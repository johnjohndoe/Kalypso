/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
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
 lat/lon GmbH
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Klaus Greve
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: klaus.greve@uni-bonn.de

 
 ---------------------------------------------------------------------------*/

package org.deegree_impl.clients.wmsclient.control;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.HashMap;

import javax.servlet.http.HttpServletRequest;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;

import org.deegree.enterprise.control.FormEvent;
import org.deegree.enterprise.control.RPCMember;
import org.deegree.enterprise.control.RPCMethodCall;
import org.deegree.enterprise.control.RPCParameter;
import org.deegree.enterprise.control.RPCStruct;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Point;
import org.deegree.xml.XMLParsingException;
import org.deegree_impl.clients.ClientException;
import org.deegree_impl.clients.context.ContextException;
import org.deegree_impl.clients.context.General;
import org.deegree_impl.clients.context.Layer;
import org.deegree_impl.clients.context.LayerList;
import org.deegree_impl.clients.context.ViewContext;
import org.deegree_impl.clients.context.WebMapContextFactory;
import org.deegree_impl.clients.context.XMLFactory;
import org.deegree_impl.clients.wmsclient.model.Constants;
import org.deegree_impl.enterprise.control.AbstractListener;
import org.deegree_impl.enterprise.control.RPCWebEvent;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.tools.StringExtend;
import org.opengis.cs.CS_CoordinateSystem;
import org.w3c.dom.Document;

/**
 * TODO: sort out user issues (where, how, what) currently .xml files are being
 * saved accepted as contexts under WEB-INF/xml/users/ user directories contexts
 * are saved without metainfo (e.g. map title) ...TODO write doc
 * 
 * @author <a href="mailto:taddei@lat-lon.de">Ugo Taddei </a>
 *  
 */
public class ContextSaveListener extends AbstractListener
{

  /**
   * @see org.deegree.enterprise.control.WebListener#actionPerformed(org.deegree.enterprise.control.FormEvent)
   */
  public void actionPerformed( FormEvent event )
  {

    RPCWebEvent rpc = (RPCWebEvent)event;
    try
    {
      validate( rpc );
    }
    catch( ClientException e )
    {
      gotoErrorPage( "Not a valid RPC for ContextSave\n" + e.getMessage() );
    }

    RPCMethodCall mc = ( (RPCWebEvent)event ).getRPCMethodCall();
    RPCParameter[] pars = mc.getParameters();
    RPCStruct struct = (RPCStruct)pars[0].getValue();

    String username = getRpcProperty( struct, "username" );
    String newContext = getRpcProperty( struct, "newContext" );
    String baseContext = getRpcProperty( struct, "baseContext" );
    String csLayerList = getRpcProperty( struct, "layerList" );

    RPCStruct bboxStruct = (RPCStruct)struct.getMember(
        org.deegree_impl.clients.wcasclient.Constants.RPC_BBOX ).getValue();
    GM_Envelope bbox = ContextSwitchListener.extractBBox( bboxStruct );

    // read base context
    String home = ( (HttpServletRequest)this.getRequest() ).getSession( true ).getServletContext()
        .getRealPath( "/" )
        + "WEB-INF/xml/";
    StringBuffer path2Dir = new StringBuffer( home );

    ViewContext vc = null;
    try
    {
      vc = openVCFile( path2Dir.append( baseContext ).toString() );
    }
    catch( ClientException e )
    {
      gotoErrorPage( "Error opening base context<br/>" + e.getMessage() );
    }

    // change values: BBOX and Layer List
    try
    {
      changeBBox( vc, bbox );
      changeLayerList( vc, csLayerList );
    }
    catch( ClientException e )
    {
      gotoErrorPage( "Error changing context<br/>" + e.getMessage() );
    }

    // save new context
    try
    {
      path2Dir = new StringBuffer( home );
      path2Dir.append( "users/" );
      path2Dir.append( username + "/" );
      path2Dir.append( newContext );

      saveDocument( vc, path2Dir.toString() );
    }
    catch( ClientException e )
    {
      gotoErrorPage( "Error saving new context<br/>" + e.getMessage() );
    }

    // forward to new page
    this.getRequest().setAttribute( Constants.MESSAGE, "Saved context: " + newContext );

  }

  private ViewContext openVCFile( String file ) throws ClientException
  {
    try
    {
      return WebMapContextFactory.createViewContext( file );
    }
    catch( IOException ue )
    {
      throw new ClientException( "could not open file '" + file + "'\n"
          + StringExtend.stackTraceToString( ue.getStackTrace() ) );
    }
    catch( XMLParsingException xe )
    {
      throw new ClientException( "could not parse base context " + file + " \n"
          + StringExtend.stackTraceToString( xe.getStackTrace() ) );
    }
    catch( ContextException ce )
    {
      throw new ClientException( "could not create the context\n"
          + StringExtend.stackTraceToString( ce.getStackTrace() ) );
    }

  }

  private void saveDocument( ViewContext vc, String filename ) throws ClientException
  {
    try
    {
      Document doc = XMLFactory.export( vc );
      FileOutputStream fos = new FileOutputStream( filename );

      internalSave( new javax.xml.transform.stream.StreamResult( fos ), doc.getFirstChild()
          .getOwnerDocument() );
      fos.close();
    }
    catch( FileNotFoundException e )
    {
      throw new ClientException( "could not save file '" + filename + "'\n"
          + StringExtend.stackTraceToString( e.getStackTrace() ) );
    }
    catch( IOException e )
    {
      throw new ClientException( "could not save file '" + filename + "'\n"
          + StringExtend.stackTraceToString( e.getStackTrace() ) );
    }
    catch( ParserConfigurationException e )
    {
      throw new ClientException( "could not save file '" + filename + "'\n"
          + StringExtend.stackTraceToString( e.getStackTrace() ) );
    }
  }

  private void validate( RPCWebEvent rpc ) throws ClientException
  {
    RPCMethodCall mc = rpc.getRPCMethodCall();
    RPCParameter param = mc.getParameters()[0];
    RPCStruct struct = (RPCStruct)param.getValue();
    RPCMember username = struct.getMember( "username" );
    if( username == null )
    {
      throw new ClientException( "missing parameter 'username' in RPC for ContextSave" );
    }
    RPCMember newContext = struct.getMember( "newContext" );
    if( newContext == null )
    {
      throw new ClientException( "missing parameter 'newContext' in RPC for ContextSave" );
    }
    RPCMember baseContext = struct.getMember( "baseContext" );
    if( baseContext == null )
    {
      throw new ClientException( "missing parameter 'baseContext' in RPC for ContextSave" );
    }
    RPCMember layerList = struct.getMember( "layerList" );
    if( layerList == null )
    {
      throw new ClientException( "missing parameter 'layerList' in RPC for ContextSave" );
    }
    // TODO validate box: should do this in a common (static) method
    // for many listeners that need a bbox
  }

  private void changeBBox( ViewContext vc, GM_Envelope bbox ) throws ClientException
  {
    General gen = vc.getGeneral();

    CS_CoordinateSystem cs = gen.getBoundingBox()[0].getCoordinateSystem();
    GM_Point[] p = new GM_Point[]
    { GeometryFactory.createGM_Point( bbox.getMin(), cs ),
        GeometryFactory.createGM_Point( bbox.getMax(), cs ) };
    try
    {
      gen.setBoundingBox( p );
    }
    catch( ContextException e )
    {
      throw new ClientException( "Error setting new BBOX \n"
          + StringExtend.stackTraceToString( e.getStackTrace() ) );
    }
  }

  private void changeLayerList( ViewContext vc, String csList ) throws ClientException
  {
    LayerList layerList = vc.getLayerList();

    String[] ls = csList.split( "," );
    HashMap lMap = new HashMap( ls.length );
    for( int i = 0; i < ls.length; i++ )
    {
      lMap.put( ls[i], Boolean.TRUE );
    }

    Layer[] layers = layerList.getLayers();
    for( int i = 0; i < layers.length; i++ )
    {
      String n = layers[i].getName();
      // remove unselected layer
      Object o = lMap.get( n );
      if( o != null )
      {
        if( ( (Boolean)o ).booleanValue() )
        {
          //make sure available layers are turned on:
          layers[i].setHidden( false );
        }
      }
      else
      {
        layerList.removeLayer( n );
      }
    }

    try
    {
      vc.setLayerList( layerList );
    }
    catch( ContextException e )
    {
      throw new ClientException( "Error setting new layer list \n"
          + StringExtend.stackTraceToString( e.getStackTrace() ) );
    }
  }

  private String getRpcProperty( RPCStruct struct, String memberName )
  {
    return (String)struct.getMember( memberName ).getValue();
  }

  protected static void internalSave( javax.xml.transform.Result result, Document doc )
      throws ClientException
  {
    try
    {
      Source source = new DOMSource( doc );
      Transformer transformer = TransformerFactory.newInstance().newTransformer();
      transformer.transform( source, result );

    }
    catch( Exception e )
    {
      throw new ClientException( "Error saving context xml \n"
          + StringExtend.stackTraceToString( e.getStackTrace() ) );
    }

  }

}