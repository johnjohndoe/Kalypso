// $Header:
// /cvsroot/deegree/deegree/org/deegree_impl/clients/wmsclient2/control/PrintListener.java,v
// 1.1 2004/08/18 06:37:01 taddei Exp $
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
package org.deegree_impl.clients.wmsclient2.control;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.io.EOFException;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.StringTokenizer;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.deegree.enterprise.control.FormEvent;
import org.deegree.enterprise.control.RPCMember;
import org.deegree.enterprise.control.RPCMethodCall;
import org.deegree.enterprise.control.RPCParameter;
import org.deegree.enterprise.control.RPCStruct;
import org.deegree.graphics.Encoders;
import org.deegree.services.InconsistentRequestException;
import org.deegree.services.WebServiceException;
import org.deegree.services.wms.protocol.WMSGetLegendGraphicRequest;
import org.deegree.services.wms.protocol.WMSGetMapRequest;
import org.deegree.xml.XMLParsingException;
import org.deegree_impl.clients.context.GeneralExtension;
import org.deegree_impl.clients.context.IOSettings;
import org.deegree_impl.clients.context.ViewContext;
import org.deegree_impl.clients.wmsclient.control.AbstractMapListener;
import org.deegree_impl.enterprise.control.RPCWebEvent;
import org.deegree_impl.services.wms.protocol.WMSProtocolFactory;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.IDGenerator;
import org.deegree_impl.tools.ImageUtils;

/**
 * will be called if the client forces a print action.
 * <p>
 * -------------------------------------------------------
 * </p>
 * 
 * TODO translate pars into english TODO handle cases where scale bar doesn't
 * exist!
 * 
 * @author <a href="mailto:lupp@lat-lon.de">Katharina Lupp </a>
 * @version $Revision$ $Date$
 */
public class PrintListener extends AbstractMapListener
{
  /**
   * the method will be called if a print action/event occurs.
   */
  public void actionPerformed( FormEvent event )
  {
    Debug.debugMethodBegin();
    super.actionPerformed( event );

    RPCWebEvent rpc = (RPCWebEvent)event;
    RPCMethodCall mc = rpc.getRPCMethodCall();
    RPCParameter[] para = mc.getParameters();
    RPCStruct struct = (RPCStruct)para[0].getValue();
    RPCStruct struct2 = (RPCStruct)para[1].getValue();

    HttpSession session = ( (HttpServletRequest)this.getRequest() ).getSession( true );
    ViewContext vc = (ViewContext)session.getAttribute( "DefaultMapContext" );

    HashMap[] model = createWMSRequestModel( struct2, vc );
    int[] imageSize = calcImageSize( struct, model[0] );
    int width = imageSize[0];
    int height = imageSize[1];
    int xmap = imageSize[2];
    int ymap = imageSize[3];
    int mapStart = imageSize[4];

    try
    {
      WMSGetLegendGraphicRequest legendParam = getLegendRequestParameter();

      //HashMap with legend
      HashMap symbols = getLegend( struct, legendParam, model );
      Rectangle rectLegend = calcLegendSize( struct, symbols );

      Rectangle rectMap = calcMapSize( model, rectLegend, width, height );

      //sets new size of map und creates the adequate BufferedImage
      model = modifyModelSize( rectMap, model, xmap, ymap );
      HashMap[] copy = createCopy( model );
      BufferedImage biReq = getMap( copy );

      // creates scalebar image
      String scalebarrequest = createGetScaleBarRequest( struct, model );
      BufferedImage biScaleBar = getScaleBar( scalebarrequest );

      //creates BufferedImage with required size and creates graphic
      BufferedImage bi = createBackgroundImage( rectMap, rectLegend );

      bi = drawMapToBI( biReq, bi, mapStart );
      bi = drawLegendToBI( struct, symbols, bi, ( mapStart + rectMap.width ) );
      bi = drawScaleBarToBI( biScaleBar, bi, mapStart + rectMap.width + 12, rectLegend.height + 12 );

      saveImage( vc, struct, bi );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    Debug.debugMethodEnd();
  }

  /**
   * creates a background BufferedImage for the attributes (map, legend,
   * scalebar)
   */
  private BufferedImage createBackgroundImage( Rectangle rectMap, Rectangle rectLegend )
  {
    BufferedImage bi = null;

    if( rectMap.height > rectLegend.height )
    {
      bi = new BufferedImage( rectMap.width + rectLegend.width + 40, rectMap.height + 100,
          BufferedImage.TYPE_INT_RGB );
    }
    else
    {
      bi = new BufferedImage( rectMap.width + rectLegend.width + 40, rectLegend.height + 100,
          BufferedImage.TYPE_INT_RGB );
    }

    Graphics g = bi.getGraphics();
    g.setColor( Color.WHITE );
    g.fillRect( 1, 1, bi.getWidth() - 2, bi.getHeight() - 2 );
    g.dispose();

    return bi;
  }

  /**
   * creates copy of the model
   */
  private HashMap[] createCopy( HashMap[] model )
  {
    HashMap[] copy = new HashMap[model.length];
    synchronized( this )
    {
      for( int i = 0; i < model.length; i++ )
      {
        copy[i] = (HashMap)model[i].clone();
      }
    }

    return copy;
  }

  /**
   * creates model with WMS request from the RPCStruct request parameter.
   */
  private HashMap[] createWMSRequestModel( RPCStruct struc, ViewContext vc )
  {

    RPCMember[] member = struc.getMembers();
    HashMap[] getMR = new HashMap[member.length];

    String request = "";
    //for ( int i = 0; i < member.length; i++ ) {
    for( int i = member.length - 1; i >= 0; i-- )
    {
      request = (String)member[i].getValue();
      getMR[i] = toMap( request );
      StringTokenizer st = new StringTokenizer( request, "?" );
      getMR[i].put( "URL", st.nextToken() );
    }
    return getMR;
  }

  /**
   * calculates images size in dependency on chosen paperFormat, resolution and
   * orientation.
   */
  private int[] calcImageSize( RPCStruct struct, HashMap model )
  {
    double width = 0;
    double height = 0;
    double mapSize = 0;

    double mapWI = Double.parseDouble( (String)model.get( "WIDTH" ) );
    double mapHE = Double.parseDouble( (String)model.get( "HEIGHT" ) );

    String paperFormat = (String)struct.getMember( "paperFormat" ).getValue();
    String resolution = (String)struct.getMember( "resolution" ).getValue();
    String orientation = (String)struct.getMember( "orientation" ).getValue();

    if( paperFormat.equals( "A4" ) )
    {
      if( orientation.equals( "hoch" ) )
      {
        width = 8.2;
        height = 11.6;
        mapSize = 6.5; //legend beside
      }
      else
      {
        width = 11.6;
        height = 8.2;
        mapSize = 9.5;
      }
    }
    else if( paperFormat.equals( "A3" ) )
    {
      if( orientation.equals( "hoch" ) )
      {
        width = 11.6;
        height = 16.5;
        mapSize = 9.8; //legend beside
      }
      else
      {
        width = 16.5;
        height = 11.6;
        mapSize = 14;
      }
    }
    else if( paperFormat.equals( "A5" ) )
    {
      if( orientation.equals( "hoch" ) )
      {
        width = 5.8;
        height = 8.2;
        mapSize = 4.1; //legend beside
      }
      else
      {
        width = 8.2;
        height = 5.8;
        mapSize = 6.5;
      }
    }

    width = width * Double.parseDouble( resolution );
    height = height * Double.parseDouble( resolution );
    mapSize = mapSize * Double.parseDouble( resolution );
    double mapSt = 0.3 * Double.parseDouble( resolution );

    int xmap = (int)Math.round( mapSize );
    double fac = mapWI / mapHE;
    int ymap = (int)Math.round( mapSize / fac );
    int mapStart = 45;
    int[] imagesize = new int[]
    { (int)Math.round( width ), (int)Math.round( height ), xmap, ymap, mapStart };

    return imagesize;
  }

  /**
   * calculates size (width and height) of legend depending on chosen
   * paperFormat and resolution.
   */
  private Rectangle calcLegendSize( RPCStruct struct, HashMap map )
  {

    String paperFormat = (String)struct.getMember( "paperFormat" ).getValue();
    String resolution = (String)struct.getMember( "resolution" ).getValue();

    String[] layers = (String[])map.get( "NAMES" );
    BufferedImage[] legs = (BufferedImage[])map.get( "IMAGES" );

    int w = 0;
    int h = 0;
    int size = 12;
    double tmph1 = 0;
    double tmph2 = 0;

    if( paperFormat.equals( "A4" ) )
    {
      tmph1 = size / 3d;
    }
    else if( paperFormat.equals( "A3" ) )
    {
      tmph1 += 2 * size / 3;
    }

    double res = Double.parseDouble( resolution );
    tmph2 = ( ( res - 150 ) / 150d ) * size;
    size += (int)Math.round( tmph2 + tmph1 );
    for( int i = 0; i < layers.length; i++ )
    {
      h += ( legs[i].getHeight() + 6 );

      Graphics g = legs[i].getGraphics();
      Font f = new Font( g.getFont().getFontName(), g.getFont().getStyle(), size );
      g.setFont( f );
      Rectangle2D rect = g.getFontMetrics().getStringBounds( layers[i], g );
      g.dispose();

      if( rect.getWidth() > w )
      {
        w = (int)rect.getWidth();
      }
    }
    w += 150;

    return new Rectangle( w, h );
  }

  /**
   * calculates map size in dependency of the chosen paperFormat and resolution.
   */
  private Rectangle calcMapSize( HashMap[] model, Rectangle rectLegend, int width, int height )
  {
    int w = width - rectLegend.width - 20;
    double wi = Double.parseDouble( (String)model[0].get( "WIDTH" ) );
    double he = Double.parseDouble( (String)model[0].get( "HEIGHT" ) );
    double fac = he / wi;
    int h = (int)Math.round( fac * w );

    return new Rectangle( w, h );
  }

  /**
   * draws legend with symbols and name of the layers to the background
   * BufferedImage.
   */
  private BufferedImage drawLegendToBI( RPCStruct struct, HashMap map, BufferedImage bi, int start )
  {

    String paperFormat = (String)struct.getMember( "paperFormat" ).getValue();
    String resolution = (String)struct.getMember( "resolution" ).getValue();

    int h = 5;
    Graphics g = bi.getGraphics();
    g.setColor( Color.WHITE );
    String[] layers = (String[])map.get( "NAMES" );
    BufferedImage[] legs = (BufferedImage[])map.get( "IMAGES" );

    for( int i = layers.length - 1; i >= 0; i-- )
    {
      g.drawImage( legs[i], start + 10, h, null );
      g.setColor( Color.BLACK );

      if( legs[i].getHeight() < 50 )
      {
        g.drawString( layers[i], start + 25 + legs[i].getWidth(), h
            + (int)( legs[i].getHeight() / 1.2 ) );
      }

      h += ( legs[i].getHeight() + 5 );
    }

    g.dispose();
    return bi;
  }

  /**
   * modifies width and height in the WMSGetMapRequest in dependency of the
   * chosen paperFormat and resolution. New width and height are calculated in
   * the method "calcMapSize".
   */
  private HashMap[] modifyModelSize( Rectangle rectMap, HashMap[] model, int width, int height )
  {
    int w = rectMap.width;
    double fac = (double)height / (double)width;
    double h = fac * w;

    for( int i = 0; i < model.length; i++ )
    {
      model[i].put( "HEIGHT", "" + (int)Math.round( h ) );
      model[i].put( "WIDTH", "" + w );
    }

    return model;
  }

  /**
   * gets the map with corresponding mapsizes built-in the urls.
   */
  private BufferedImage getMap( HashMap[] model ) throws MalformedURLException, IOException,
      XMLParsingException, InconsistentRequestException, WebServiceException
  {
    int w = Integer.parseInt( (String)model[0].get( "WIDTH" ) );
    int h = Integer.parseInt( (String)model[0].get( "HEIGHT" ) );
    BufferedImage bJ = new BufferedImage( w, h, BufferedImage.TYPE_INT_RGB );
    Graphics g = bJ.getGraphics();
    g.setColor( Color.WHITE );
    g.fillRect( 1, 1, bJ.getWidth() - 2, bJ.getHeight() - 2 );
    g.dispose();
    WMSGetMapRequest gmr = null;

    for( int i = model.length - 1; i >= 0; i-- )
    {
      String urls = (String)model[i].remove( "URL" );
      String s = URLDecoder.decode( (String)model[i].get( "FORMAT" ), "UTF-8" );
      model[i].put( "FORMAT", s );
      //String version = (String)model[i].get( "VERSION" );
      gmr = WMSProtocolFactory.createGetMapRequest( "1.1.1", model[i] );
      URL url = new URL( urls + "?" + gmr.getRequestParameter() );
      BufferedImage tmp = ImageUtils.loadImage( url );
      g = bJ.getGraphics();
      g.drawImage( tmp, 0, 0, null );
      g.dispose();
    }

    return bJ;
  }

  /**
   * draws the map to the background BufferedImage
   */
  private BufferedImage drawMapToBI( BufferedImage reqIm, BufferedImage bi, int mapStart )
  {
    Graphics g = bi.getGraphics();
    g.drawImage( reqIm, mapStart, mapStart, null );
    g.dispose();

    return bi;
  }

  /**
   * draws the scalebar to the background BufferedImage
   */
  private BufferedImage drawScaleBarToBI( BufferedImage scaleBar, BufferedImage bi, int xOffset,
      int yOffset )
  {
    Graphics g = bi.getGraphics();
    g.drawImage( scaleBar, xOffset, yOffset, null );
    g.dispose();

    return bi;
  }

  /**
   * gets scalebar as BufferedImage
   */
  private BufferedImage getScaleBar( String scalebarrequest ) throws IOException,
      MalformedURLException, InconsistentRequestException, XMLParsingException
  {
    BufferedImage sbbi = null;

    if( scalebarrequest != null )
    {
      URL sburl = new URL( scalebarrequest );

      try
      {
        sbbi = ImageUtils.loadImage( sburl );
      }
      catch( EOFException e )
      {
        System.out.println( "\nCould not generate scalebar from: \n" + sburl );
        sbbi = null;
        //e.printStackTrace();
      }
    }

    return sbbi;
  }

  /**
   * saves image with BufferedImage including map, legend and scalebar in chosen
   * image format and directory set in mapcontext.xml file. Via the attribute
   * "ImageSource" the image can be called for.
   */
  private void saveImage( ViewContext vc, RPCStruct struct, BufferedImage bg )
  {
    String format = (String)struct.getMember( "format" ).getValue();
    format = format.substring( format.indexOf( "/" ) + 1, format.length() );
    GeneralExtension ge = (GeneralExtension)vc.getGeneral().getExtension();
    IOSettings ios = ge.getIOSettings();
    String dir = ios.getPrintDirectory().getDirectoryName();
    long l = IDGenerator.getInstance().generateUniqueID() % 100;
    String file = "Map" + l + "." + format;

    try
    {
      File f = new File( dir + "/" + file );
      f.deleteOnExit();
      FileOutputStream fos = new FileOutputStream( f );

      if( format.equals( "jpeg" ) )
      {
        Encoders.encodeJpeg( fos, bg, 1 );
      }
      else if( format.equals( "gif" ) )
      {
        Encoders.encodeGif( fos, bg );
      }
      else if( format.equals( "bmp" ) )
      {
        Encoders.encodeBmp( fos, bg );
      }
      else if( format.equals( "png" ) )
      {
        Encoders.encodePng( fos, bg );
      }
      else if( format.equals( "tiff" ) )
      {
        Encoders.encodeTiff( fos, bg );
      }

      fos.close();
    }
    catch( Exception e )
    {
      System.out.println( "Error occurred in saving image: " + e );
    }

    int pos = dir.lastIndexOf( '/' );
    String access = "./" + dir.substring( pos + 1, dir.length() ) + "/" + file;
    this.getRequest().setAttribute( "ImageSource", access );

  }

  /**
   * sets the request for scalebar.
   */
  private String createGetScaleBarRequest( RPCStruct struct, HashMap[] model )
  {
    Debug.debugMethodBegin();

    String paperFormat = (String)struct.getMember( "paperFormat" ).getValue();
    String resolution = (String)struct.getMember( "resolution" ).getValue();

    int fontsize = 12;
    double fontsize1 = 0;
    if( paperFormat.equals( "A4" ) )
    {
      fontsize1 += fontsize / 3;
    }
    else if( paperFormat.equals( "A3" ) )
    {
      fontsize1 += 2 * fontsize / 3;
    }
    double res = Double.parseDouble( resolution );
    double fontsize2 = ( ( res - 150 ) / 150d ) * fontsize;
    fontsize += ( fontsize1 + fontsize2 );
    String fontSize = "" + fontsize;
    // TODO what if legend under model[0].get( "URL" )
    // doesn't exist?
    String wmsurl = "" + model[0].get( "URL" );
    StringBuffer sb = new StringBuffer( 1000 );
    sb.append( wmsurl ).append( '?' ).append( "SERVICE=WMS" ).append( "&VERSION=1.1.1" ).append(
        "&REQUEST=GetScaleBar" ).append( "&FORMAT=image/jpeg" ).append( "&WIDTH=" ).append(
        model[0].get( "WIDTH" ) ).append( "&HEIGHT=" ).append( model[0].get( "HEIGHT" ) ).append(
        "&EXCEPTIONS=application/vnd.ogc.se_inimage" ).append( "&BBOX=" ).append(
        model[0].get( "BBOX" ) ).append( "&SIZE=150" ).append( "&UNITS=meter" ).append(
        "&TOPLABEL=SIZE" ).append( "&BOTTOMLABEL=SCALE" ).append( "&LABELCOLOR=0x000000" ).append(
        "&FONT=ARIAL" ).append( "&FONTSIZE=" ).append( fontSize ).append( "&COLOR=0x000000" )
        .append( "&BGCOLOR=0xFFFFFF" ).append( "&SRS=" ).append( model[0].get( "SRS" ) );
    Debug.debugMethodEnd();
    return sb.toString();
  }

  /**
   * gets parameters of the LegendRequest
   */
  private WMSGetLegendGraphicRequest getLegendRequestParameter() throws XMLParsingException,
      MalformedURLException, InconsistentRequestException
  {
    HashMap legend = toMap( "VERSION=1.1.1&REQUEST=GetLegendGraphic&FORMAT=image/jpeg&WIDTH=50&HEIGHT=50&"
        + "EXCEPTIONS=application/vnd.ogc.se_inimage&LAYER=europe:major_rivers&STYLE=default&"
        + "SLD=file:///C:/Projekte/UmweltInfo/deegreewms/WEB-INF/xml/styles.xml" );
    WMSGetLegendGraphicRequest legendReq = (WMSGetLegendGraphicRequest)WMSProtocolFactory
        .createRequest( "1.1.1", legend );

    return legendReq;
  }

  /**
   * gets legend with layers and styles. The size of the symbols is adjusted to
   * the chosen paperFomrat and resolution.
   */
  private HashMap getLegend( RPCStruct struct, WMSGetLegendGraphicRequest glr, HashMap[] model )
      throws MalformedURLException, IOException
  {

    Debug.debugMethodBegin( this, "setScaleBarURL" );

    ArrayList list1 = new ArrayList();
    ArrayList list2 = new ArrayList();

    StringTokenizer st = null;
    String format = glr.getFormat();
    if( format.equals( "image/jpg" ) )
      format = "image/jpeg";

    String legendURL = "";
    int lgHeight = 0;

    String paperFormat = (String)struct.getMember( "paperFormat" ).getValue();
    String resolution = (String)struct.getMember( "resolution" ).getValue();
    int h = 15;
    int w = 15;
    double tmph1 = 0;
    double tmpw1 = 0;
    double tmph2 = 0;
    double tmpw2 = 0;

    if( paperFormat.equals( "A4" ) )
    {
      tmph1 = h / 3d;
      tmpw1 = w / 3d;
    }
    else if( paperFormat.equals( "A3" ) )
    {
      tmph1 = 2 * h / 3d;
      tmpw1 = 2 * w / 3d;
    }

    double res = Double.parseDouble( resolution );
    tmph2 = ( ( res - 150 ) / 150d ) * h;
    tmpw2 = ( ( res - 150 ) / 150d ) * w;

    w += (int)Math.round( tmpw1 + tmpw2 );
    h += (int)Math.round( tmph1 + tmph2 );

    for( int i = 0; i < model.length; i++ )
    {
      if( model[i].get( "SLD" ) != null )
      {
        continue;
      }

      String style = (String)model[i].get( "STYLE" );

      if( style != null )
      {
        st = new StringTokenizer( style, "," );
        style = st.nextToken();
      }
      else
      {
        style = "default";
      }

      st = new StringTokenizer( (String)model[i].get( "LAYERS" ), "," );

      for( int j = 0; j <= st.countTokens(); j++ )
      {
        String layer = st.nextToken();
        legendURL = createLegendURL( w, h, layer, style, format, glr, model[0] );
        BufferedImage legendGraphic = null;
        try
        {
          legendGraphic = ImageUtils.loadImage( new URL( legendURL ) );
        }
        catch( Exception e )
        {
          System.out.println( "\n\nLegend graphic for layer '" + layer
              + "' is not available. Skipping.\n\n" );
          legendGraphic = null;
          //e.printStackTrace();
        }
        if( legendGraphic != null )
        {
          list1.add( layer );
          list2.add( legendGraphic );
        }
      }
    }

    Debug.debugMethodEnd();
    String[] layers = (String[])list1.toArray( new String[list1.size()] );
    BufferedImage[] legs = (BufferedImage[])list2.toArray( new BufferedImage[list2.size()] );
    HashMap map = new HashMap();
    map.put( "NAMES", layers );
    map.put( "IMAGES", legs );

    return map;
  }

  /**
   * creates legend URL with corresponding key values.
   */
  private String createLegendURL( int w, int h, String layer, String style, String format,
      WMSGetLegendGraphicRequest glr, HashMap model )
  {
    String legendReq = model.get( "URL" ) + "?" + "SERVICE=" + glr.getService() + "&VERSION="
        + glr.getVersion() + "&REQUEST=GetLegendGraphic" + "&FORMAT=" + format + "&WIDTH=" + w
        + "&HEIGHT=" + h + "&EXCEPTIONS=application/vnd.ogc.se_inimage" + "&LAYER=" + layer
        + "&STYLE=" + style;

    return legendReq;
  }
}
/*******************************************************************************
 * Changes to this class. What the people have been up to: $Log:
 * PrintListener.java,v $ Revision 1.1 2004/08/18 06:37:01 taddei Listener with
 * minor bug fixes: catching exceptions etc...
 * 
 * 
 *  
 ******************************************************************************/