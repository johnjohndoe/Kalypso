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
package org.deegree_impl.clients.context;

import java.awt.Color;
import java.net.URL;
import java.util.ArrayList;

import org.deegree.clients.context.Frontend;
import org.deegree.clients.context.GUIArea;
import org.deegree.clients.context.Module;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Point;
import org.deegree.services.wms.protocol.WMSGetMapRequest;
import org.deegree.tools.ParameterList;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.services.wms.protocol.WMSGetMapRequest_Impl;
import org.deegree_impl.services.wms.protocol.WMSProtocolFactory;
import org.deegree_impl.tools.NetWorker;

/**
 * This class offers methods for creating JSP and JavaSript fragments depending
 * on the Map context document used as configuration for the deegree web map
 * client. <p/>The created fragments partially uses javascript calls as defined
 * in the deegree integrated client. if somebody needs other javascript methods
 * he/she must write its own creator methods or must directly use an instance of
 * a <tt>ViewContext</tt> in a JSP page.
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class JSPCreator
{
  /**
   * creates JSP import statement for the modules being part of an area of the
   * GUI/page. Known areas are GUIArea.WEST, GUIArea.EAST, GUIArea.NORTH,
   * GUIArea.SOUTH and GUIArea.CENTER
   * 
   * @param viewContext
   * @param area
   *          area to create import statements for
   * 
   * @return JSP import fragment
   */
  public static String getArea( ViewContext viewContext, int area )
  {
    GeneralExtension ge = (GeneralExtension)viewContext.getGeneral().getExtension();
    StringBuffer sb = new StringBuffer( 5000 );

    if( ge != null )
    {
      Frontend frontend = ge.getFrontend();
      GUIArea guiarea = null;

      switch( area )
      {
      case GUIArea.WEST:
        guiarea = frontend.getWest();
        break;
      case GUIArea.EAST:
        guiarea = frontend.getEast();
        break;
      case GUIArea.SOUTH:
        guiarea = frontend.getSouth();
        break;
      case GUIArea.NORTH:
        guiarea = frontend.getNorth();
        System.out.println( "NORD in JSPCreator" );
        break;
      case GUIArea.CENTER:
        guiarea = frontend.getCenter();
        break;
      }

      if( guiarea != null )
      {
        Module[] module = guiarea.getModules();

        for( int i = 0; i < module.length; i++ )
        {
          if( !module[i].isHidden() )
          {
            sb.append( "<jsp:include page=\"" ).append( module[i].getContent() ).append(
                "\" flush=\"true\" />\n" );
          }
        }
      }
    }

    return sb.toString();
  }

  /**
   * creates a set of constructor calls that are requierd to initialize the GUI.
   * Each module that is registered will be represented by a JavaScript object.
   * For each of these objects one instance will be created and registered by
   * the central controller which also is a JavaScript object.
   * 
   * @param viewContext
   *          web map context
   * 
   * @return javascript fragment
   */
  public static String createJSInitSection( ViewContext viewContext )
  {
    GeneralExtension ge = (GeneralExtension)viewContext.getGeneral().getExtension();
    StringBuffer sb = new StringBuffer( 5000 );

    sb.append( "var controller = null;\n" );
    sb.append( "function register() { " );
    sb.append( "if ( controller == null ) { \n" );
    // init central javascript controller to be able to establish
    // communication between the inculded JS/JSP modules
    sb.append( "controller = new Controller();\n" );

    if( ge != null )
    {
      // init objects for each defined area of the JSP page
      Frontend frontend = ge.getFrontend();
      GUIArea guiarea = frontend.getWest();

      if( guiarea != null )
      {
        sb.append( createInitCalls( guiarea ) );
      }

      guiarea = frontend.getEast();

      if( guiarea != null )
      {
        sb.append( createInitCalls( guiarea ) );
      }

      guiarea = frontend.getSouth();

      if( guiarea != null )
      {
        sb.append( createInitCalls( guiarea ) );
      }

      guiarea = frontend.getNorth();

      if( guiarea != null )
      {
        sb.append( createInitCalls( guiarea ) );
      }

      guiarea = frontend.getCenter();

      if( guiarea != null )
      {
        sb.append( createInitCalls( guiarea ) );
      }
    }

    sb.append( "}}" );

    return sb.toString();
  }

  /**
   * 
   * 
   * @param guiarea
   * 
   * @return
   */
  private static StringBuffer createInitCalls( GUIArea guiarea )
  {
    StringBuffer sb = new StringBuffer( 1000 );
    Module[] module = guiarea.getModules();

    for( int i = 0; i < module.length; i++ )
    {

      sb.append( "var " ).append( module[i].getName().toLowerCase() ).append( " = new " ).append(
          module[i].getName() ).append( "('" ).append( module[i].getName() );
      // add parameter to constructor call if defined
      ParameterList pList = module[i].getParameter();

      if( pList != null )
      {
        Object[] values = pList.getParameterValues();
        if( values.length > 0 )
        {
          sb.append( "','" );
        }
        for( int k = 0; k < values.length; k++ )
        {
          sb.append( values[k] );
          if( k < ( values.length - 1 ) )
          {
            sb.append( "','" );
          }
        }
      }

      sb.append( "');\n" );
      sb.append( "controller.registerModule( " ).append( module[i].getName().toLowerCase() )
          .append( ");\n" );
    }
    return sb;
  }

  /**
   * @param viewContext
   * @return
   */
  public static String createCommonJSImport( ViewContext viewContext )
  {
    GeneralExtension ge = (GeneralExtension)viewContext.getGeneral().getExtension();
    JSPFrontend fr = (JSPFrontend)ge.getFrontend();
    String[] commonJS = fr.getCommonJS();
    StringBuffer sb = new StringBuffer( 5000 );
    for( int i = 0; i < commonJS.length; i++ )
    {
      sb.append( "<SCRIPT LANGUAGE='JavaScript1.2' TYPE='text/javascript' " );
      sb.append( "src='" ).append( commonJS[i] ).append( "'></SCRIPT>" );
    }

    return sb.toString();
  }

  /**
   * returns a matrix containing an array of layer for each server defined in
   * the map context. the matrix is ordered in that way that in the first row
   * and first field you find the server in the second field you find an array
   * of layers served by the server. In the next row the same is done for
   * another server. In the third row you find another server or the same server
   * as in the first row because the layer order as defined in the map context
   * requiers asking one server two or more times.
   * 
   * @param viewContext
   * 
   * @return
   */
  public static Object[][] getLayersGroupedByService( ViewContext viewContext )
  {
    ArrayList servers = new ArrayList( 50 );
    ArrayList ll = new ArrayList( 50 );

    LayerList layerList = viewContext.getLayerList();
    Layer[] layers = layerList.getLayers();

    for( int i = 0; i < layers.length; i++ )
    {
      System.out.println( "i " + layers[i].getName() + " "
          + layers[i].getStyleList().getCurrentStyle().getName() );
    }

    int i = 0;

    Server server = layers[0].getServer();
    URL url = server.getOnlineResource();
    String key = url.toString();
    String tmp = key;

    while( i < layers.length )
    {
      servers.add( key );
      ArrayList list = new ArrayList();

      while( key.equals( tmp ) && ( i < layers.length ) )
      {
        list.add( layers[i] );
        server = layers[i++].getServer();
        url = server.getOnlineResource();
        tmp = url.toString();
      }

      ll.add( list.toArray( new Layer[list.size()] ) );
      key = tmp;
    }

    Object[][] o = new Object[servers.size()][2];

    for( i = 0; i < servers.size(); i++ )
    {
      o[i][0] = servers.get( i );
      o[i][1] = ll.get( i );
    }

    return o;
  }

  /**
   * creates a list of servers and GetMap requests assigned to them from a
   * <tt>ViewContext</tt>. The requests/servers are order like they appears
   * in the underlying web map context document. The requests are presented in a
   * bottom to top approach. TODO: the method currently doesn't deal with layers
   * using SLD for style description and services others than WMS
   * 
   * @param viewContext
   * 
   * @return
   */
  public static String[][] getServiceRequests( ViewContext viewContext ) throws ContextException
  {
    int width = viewContext.getGeneral().getWindow().width;
    int height = viewContext.getGeneral().getWindow().height;
    GM_Point[] bbox = viewContext.getGeneral().getBoundingBox();
    GM_Envelope env = GeometryFactory.createGM_Envelope( bbox[0].getX(), bbox[0].getY(), bbox[1]
        .getX(), bbox[1].getY() );
    String crs = null;

    try
    {
      crs = bbox[0].getCoordinateSystem().getName();
    }
    catch( Exception e )
    {}

    org.deegree.services.wms.protocol.WMSGetMapRequest.Layer[] reqLay = null;
    String format = null;

    Object[][] serv = getLayersGroupedByService( viewContext );
    String[][] initGMR = new String[serv.length][2];

    for( int i = 0; i < serv.length; i++ )
    {
      Server server = (Server)serv[i][0];
      Layer[] layers = (Layer[])serv[i][1];

      if( usesSLD( layers ) )
      {
        // TODO
        // create GetMap Request is at least one layer uses SLD
      }
      else
      {
        // create requiered layer, style and format attributes if a
        // convetional KVP without SLD should be used
        reqLay = new WMSGetMapRequest.Layer[layers.length];

        for( int k = 0; k < layers.length; k++ )
        {
          if( !layers[i].isHidden() )
          {
            String sName = layers[k].getStyleList().getCurrentStyle().getName();
            String lName = layers[k].getName();
            reqLay[k] = WMSGetMapRequest_Impl.createLayer( lName, sName );
            format = layers[k].getFormatList().getCurrentFormat().getName();
          }
        }
      }

      WMSGetMapRequest gmr = null;

      try
      {
        gmr = WMSProtocolFactory.createGetMapRequest( server.getVersion(), "" + i, reqLay, null,
            null, format, width, height, crs, env, true, Color.WHITE, "application/vnd.ogc.se_xml",
            null, null, null, null );
        initGMR[i][0] = NetWorker.url2String( server.getOnlineResource() );
        initGMR[i][1] = gmr.getRequestParameter();
      }
      catch( Exception e )
      {
        e.printStackTrace();
        throw new ContextException( "couldn't create initial GetMap requests", e );
      }
    }

    return initGMR;
  }

  /**
   * returns true if one of the passed layers uses SLD instead of named styles
   */
  private static boolean usesSLD( Layer[] layers )
  {
    boolean sld = false;

    for( int k = 0; k < layers.length; k++ )
    {
      StyleList styleList = layers[k].getStyleList();
      Style style = styleList.getCurrentStyle();

      if( style != null )
      {
        if( style.getSld() != null )
        {
          sld = true;
          break;
        }
      }
    }

    return sld;
  }

  /**
   * returns the layers contained in the passed <tt>ViewContext</tt> as a HTML
   * table with a Checkbox for visibility and a Radiobutton for query
   * (featureInfo). The method assues that the deegree integrated client or Map
   * client will be used so the corresponding JavaScript calls will be created.
   * 
   * @param viewContext
   * 
   * @return
   */
  public static String getLayerAsHTMLTable( ViewContext viewContext )
  {
    StringBuffer sb = new StringBuffer( 15000 );

    String title = viewContext.getGeneral().getTitle();
    try
    {
      //title = new String( title.getBytes(), "UTF-8");
    }
    catch( Exception e )
    {}
    sb.append( getLayerAsHTMLTableJS() );
    sb.append( "<table border='0' width='97%'>" );
    sb.append( "<tr bgcolor='#E2E2EC' ><td colspan='3'><p><b>&nbsp;" );
    sb.append( title ).append( "</b></p></td><!--td colspan='2'></td--></tr>\n" );
    sb.append( "<tr valign='top'><td colspan='3' valign='top' align='left'>\n" );
    sb.append( "<table id='Tabelle' width='100%' border='0' cellspacing='0' cellpadding='0'>\n" );
    sb.append( "<form id='layerform' name='layerform'>\n" );

    Object[][] serv = getLayersGroupedByService( viewContext );

    int c = 0;
    for( int i = serv.length - 1; i >= 0; i-- )
    {
      Layer[] layers = (Layer[])serv[i][1];
      Server server = layers[0].getServer();
      String wmsName = server.getTitle();
      URL url = server.getOnlineResource();
      String wmsURL = url.toString();
      sb.append( "<tr id='" + c + "' name='" + wmsName + "' value='" + wmsURL + "'>" );

      for( int k = 0; k < layers.length; k++ )
      {
        sb.append( "<tr id='row" ).append( c ).append( "'>" );
        // checkbox section
        sb.append( "<td id ='r" ).append( c ).append(
            "c0'/><input type='checkbox' name='check' onclick='checkBoxes(this);'" );
        if( !layers[k].isHidden() )
        {
          sb.append( "checked " );
        }
        sb.append( "value='" ).append( layers[k].getName() ).append( "|" ).append(
            layers[k].getStyleList().getCurrentStyle().getName() ).append( "'/></td>\n" );
        // title section
        sb.append( "<td id ='r" ).append( c ).append( "c1' onclick=\"change('row" ).append( c )
            .append( "');\"" ).append( " title='" ).append( layers[k].getTitle() ).append( "' />" );
        String tmp = layers[k].getTitle();
        if( tmp.length() > 25 )
          tmp = tmp.substring( 0, 22 ) + "...";
        sb.append( tmp ).append( "</td>\n" );
        // radiobutton section
        sb.append( "<td id ='r" ).append( c ).append(
            "c2'/><input type='radio' name='radiocheck' onclick='radios(this);' " );
        if( layers[k].isQueryable() )
        {
          sb.append( "checked " );
        }
        sb.append( "value='" ).append( layers[k].getName() ).append( "'/></td>\n" );
        sb.append( "</tr> \n" );
        sb.append( "</td></tr>  \n" );
        c++;
      }
    }
    sb.append( "</form></table></td></tr>" );
    sb.append( "<tr><td align='left'><a href='javascript:up();'>"
        + "<img src='bilder/nach_oben.gif' border='0'/></td>" );
    sb.append( "<td align='left'><a href='javascript:down();'>"
        + "<img src='bilder/nach_unten.gif' border='0'/></td></tr> \n" );
    sb.append( "</table>\n" );

    return sb.toString();
  }

  /**
   * 
   * 
   * @return
   */
  private static StringBuffer getLayerAsHTMLTableJS()
  {
    StringBuffer sb = new StringBuffer( 10000 );
    sb.append( "<SCRIPT language='JavaScript1.2'> \n" );
    sb.append( "<!-- \n" );
    sb.append( "function checkBoxes(para) {\n" );
    sb.append( "var module = controller.getModule( 'LayerList' );\n" );
    sb.append( "module.clickCheckBoxes(para);\n" );
    sb.append( "}\n" );
    sb.append( "function radios(para) {\n" );
    sb.append( "var module = controller.getModule( 'LayerList' );\n" );
    sb.append( "module.checkTheRadios(para);\n" );
    sb.append( "}\n" );
    sb.append( "function change(row) {\n" );
    sb.append( "var module = controller.getModule( 'LayerList' );\n" );
    sb.append( "module.change(row);\n" );
    sb.append( "}\n" );
    sb.append( "function up() {\n" );
    sb.append( "var module = controller.getModule( 'LayerList' );\n" );
    sb.append( "module.up();\n" );
    sb.append( "}\n" );
    sb.append( "function down() {\n" );
    sb.append( "var module = controller.getModule( 'LayerList' );\n" );
    sb.append( "module.down();\n" );
    sb.append( "}\n" );
    sb.append( " --> \n " );
    sb.append( "</SCRIPT> \n" );
    return sb;
  }

  /**
   * 
   * 
   * @param viewContext
   * 
   * @return
   */
  public static String getVisibleLayersAsHTMLList( ViewContext viewContext )
  {
    return null;
  }

  /**
   * 
   * 
   * @param viewContext
   * 
   * @return
   */
  public static String getAvailableLayersAsHTMLList( ViewContext viewContext )
  {
    return null;
  }

  /**
   * 
   * 
   * @param viewContext
   * 
   * @return
   */
  public static String getAvailableLayersAsComboBox( ViewContext viewContext )
  {
    return null;
  }

  /**
   * 
   * 
   * @param viewContext
   * 
   * @return
   */
  public static String getControlBarContent( ViewContext viewContext )
  {
    return null;
  }

}