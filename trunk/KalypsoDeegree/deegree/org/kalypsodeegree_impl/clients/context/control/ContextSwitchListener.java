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

package org.deegree_impl.clients.context.control;

import java.io.StringWriter;
import java.net.MalformedURLException;
import java.net.URL;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.deegree.enterprise.control.FormEvent;
import org.deegree.enterprise.control.RPCMethodCall;
import org.deegree.enterprise.control.RPCParameter;
import org.deegree.enterprise.control.RPCStruct;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree_impl.clients.wcasclient.Constants;
import org.deegree_impl.enterprise.control.AbstractListener;
import org.deegree_impl.enterprise.control.RPCWebEvent;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.StringExtend;

/**
 * This class handles switch of map contexts. The basic logic is (1) receiving
 * an rpc request with a context name (xml) and, possibly, a bounding box, (2)
 * transforming this xml using a provided xsl, and (3) forwarding the result
 * back to the browser. <br/>Most of the action takes place in
 * <code>doTransformContext</code>, and is delegated to the
 * <code>ContextTransformer</code>. <br/>In order to perform the
 * transformation from a context xml to a html, a xslt is provided. This is per
 * default called <code>context2HTML.xsl</code> (see the class member
 * <code>DEFAULT_CTXT2HTML</code>) and should be put under
 * <code>${context-home}/WEB-INF/xml/</code>.<br/>
 * 
 * @author <a href="mailto:taddei@lat-lon.de">Ugo Taddei </a>
 *  
 */
public class ContextSwitchListener extends AbstractListener
{

  /**
   * A <code>String</code> used as a key value for the new html (of the
   * client). This key is used in the JSP which output the new(-ly transformed
   * html.
   */
  public static final String NEW_CONTEXT_HTML = "NEW_CONTEXT_HTML";

  /**
   * A <code>String</code> defining the name of the xsl file that defines the
   * transformation from a context to html. This must be placed, together with
   * the map context xml and helper xsl files, under
   * <code>${context-home}/WEB-INF/xml/</code>.
   */
  protected static final String DEFAULT_CTXT2HTML = "WEB-INF/xml/context2HTML.xsl";

  protected static URL xsltUrl = null;

  /**
   * @see org.deegree.enterprise.control.WebListener#actionPerformed(org.deegree.enterprise.control.FormEvent)
   */
  public void actionPerformed( FormEvent event )
  {
    Debug.debugMethodBegin( this, "actionPerformed" );

    RPCMethodCall mc = ( (RPCWebEvent)event ).getRPCMethodCall();
    RPCParameter[] pars = mc.getParameters();
    RPCStruct struct = (RPCStruct)pars[0].getValue();

    // get map context value
    String curContxt = (String)struct.getMember( "mapContext" ).getValue();

    // now get bbox
    RPCStruct bboxStruct = (RPCStruct)struct.getMember( Constants.RPC_BBOX ).getValue();
    GM_Envelope box = extractBBox( bboxStruct );

    // get the servlet path using the session
    HttpSession session = ( (HttpServletRequest)this.getRequest() ).getSession();

    // path to context dir
    String path2Dir = session.getServletContext().getRealPath( "/" );
    // context and xsl files
    String mapContext = "file:///" + path2Dir + "WEB-INF/xml/" + curContxt;
    String xslFilename = "file:///" + path2Dir + DEFAULT_CTXT2HTML;

    String newHtml = doTransformContext( xslFilename, mapContext );

    session.setAttribute( NEW_CONTEXT_HTML, newHtml );

    // need to keep a reference to the last context...
    // often used when changing/saving the shown context
    session.setAttribute( "CURRENT_CONTEXT", curContxt );

    Debug.debugMethodEnd();
  }

  /**
   * Transforms the context pointed to by <code>context</code> into html using
   * <code>xsltURL</code> (though this is currently fixed; there's really no
   * need to define one's wn xsl).
   * 
   * @param context
   *          the context xml
   * @param xsl
   *          the transformation xml
   */
  protected String doTransformContext( String xsl, String context )
  {
    Debug.debugMethodBegin();

    StringWriter sw = new StringWriter();

    try
    {
      URL ctxtUrl = new URL( context );
      StreamSource sSrc = new StreamSource( ctxtUrl.openStream() );

      if( xsltUrl == null )
      {
        xsltUrl = new URL( xsl );
      }

      Transformer transformer = null;
      TransformerFactory tFactory = TransformerFactory.newInstance();
      transformer = tFactory.newTransformer( new StreamSource( xsltUrl.openStream() ) );

      StreamResult sr = new StreamResult( sw );
      transformer.transform( sSrc, sr );
      return sw.toString();

    }
    catch( MalformedURLException e1 )
    {
      gotoErrorPage( "<b>Error creating the context URL: </b>" + e1.getMessage() + "<br/>"
          + StringExtend.stackTraceToString( e1.getStackTrace() ) );

    }
    catch( TransformerException e1 )
    {
      gotoErrorPage( "<b>Error transforming the context: </b>" + e1.getMessage() + "<br/>"
          + StringExtend.stackTraceToString( e1.getStackTrace() ) );

    }
    catch( Exception e1 )
    {
      gotoErrorPage( "<b>Error in context transformer: </b>" + e1.getMessage() + "<br/>"
          + StringExtend.stackTraceToString( e1.getStackTrace() ) );
    }

    Debug.debugMethodEnd();
    return null;
  }

  /**
   * Convenience method to extract the boundig box from an rpc fragment.
   * 
   * @param bboxStruct
   *          the <code>RPCStruct</code> containing the bounding box. For
   *          example,
   *          <code>&lt;member&gt;&lt;name&gt;boundingBox&lt;/name&gt;etc...</code>.
   * 
   * @return an envelope with the boundaries defined in the rpc structure
   */
  public static GM_Envelope extractBBox( RPCStruct bboxStruct )
  {

    Double minx = (Double)bboxStruct.getMember( Constants.RPC_BBOXMINX ).getValue();
    Double miny = (Double)bboxStruct.getMember( Constants.RPC_BBOXMINY ).getValue();
    Double maxx = (Double)bboxStruct.getMember( Constants.RPC_BBOXMAXX ).getValue();
    Double maxy = (Double)bboxStruct.getMember( Constants.RPC_BBOXMAXY ).getValue();

    GM_Envelope bbox = GeometryFactory.createGM_Envelope( minx.doubleValue(), miny.doubleValue(),
        maxx.doubleValue(), maxy.doubleValue() );
    return bbox;
  }

}