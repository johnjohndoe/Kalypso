/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ogc.gml;

import java.awt.Graphics;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.logging.Logger;

import javax.media.jai.JAI;
import javax.media.jai.RenderedOp;
import javax.media.jai.TiledImage;

import org.kalypso.template.gismapview.GismapviewType.LayersType.Layer;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.kalypsodeegree_impl.model.ct.GeoTransformer;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.WMSHelper;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * KalypsoPictureTheme
 * <p>
 * 
 * created by
 * 
 * @author kuepfer (20.05.2005)
 */
public class KalypsoPictureTheme extends AbstractKalypsoTheme

{
  private final static String SUFFIX_TIFF = "TFW";

  private final static String SUFFIX_JPG = "JGW";

  private final static String SUFFIX_PNG = "PGW";

  //private final static String SUFFIX_GIF = "GFW";

  private TiledImage m_image;

  private GM_Envelope m_origBBox;

  private CS_CoordinateSystem m_localCS;

  private double m_dx = 0;

  // Andreas: dieses Pattern (variable m_rx und m_ry nicht benutzt)
  // taucht im Code verteilt mehrfach auf; doppelter code?
//  private double m_rx = 0;

//  private double m_ry = 0;

  private double m_dy = 0;

  private CS_CoordinateSystem m_imageCS;

  private static final Logger LOGGER = Logger.getLogger( KalypsoPictureTheme.class.getName() );

  private String m_themeName;

  private String m_linkType;

  private String m_source;

  public KalypsoPictureTheme( String themeName, String linktype, String source, CS_CoordinateSystem cs )
  {
    super( themeName, linktype.toUpperCase() );
    m_themeName = themeName;
    m_linkType = linktype;
    m_source = source;
    m_localCS = cs;
    URL worldFileURL = null;
    String[] result = source.split( "#" );
    String wf = null;
    if( linktype.equals( "tif" ) )
    {
      wf = ( result[0].substring( 0, ( source.lastIndexOf( "." ) + 1 ) ) ).concat( SUFFIX_TIFF );
    }
    if( linktype.equals( "jpg" ) )
    {
      wf = ( result[0].substring( 0, ( source.lastIndexOf( "." ) + 1 ) ) ).concat( SUFFIX_JPG );
    }
    if( linktype.equals( "png" ) )
    {
      wf = ( result[0].substring( 0, ( source.lastIndexOf( "." ) + 1 ) ) ).concat( SUFFIX_PNG );
    }
    //if( linktype.equals( "gif" ) )
    //{
    //  wf = ( result[0].substring( 0, ( source.lastIndexOf( "." ) + 1 ) ) )
    //      .concat( SUFFIX_GIF );
    //}

    double ulcx = 0;
    double ulcy = 0;

    //read worldfile
    try
    {
      worldFileURL = new URL( wf );
      InputStream is = worldFileURL.openStream();
      BufferedReader br = new BufferedReader( new InputStreamReader( is ) );
      m_dx = Double.parseDouble( br.readLine().trim() );
      /*m_rx = */Double.parseDouble( br.readLine().trim() );
      /*m_ry =*/ Double.parseDouble( br.readLine().trim() );
      m_dy = Double.parseDouble( br.readLine().trim() );
      ulcx = Double.parseDouble( br.readLine().trim() );
      ulcy = Double.parseDouble( br.readLine().trim() );
    }
    catch( MalformedURLException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( IOException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

    URL imageFileURL = null;
    try
    {
      imageFileURL = new URL( source );
    }
    catch( MalformedURLException e1 )
    {
      e1.printStackTrace();
      System.out.println( "Ungültige URL der Datei: " + source );
    }
    String imagePath = imageFileURL.getPath().substring( 1, imageFileURL.getPath().length() );
    RenderedOp image = JAI.create( "fileload", imagePath );
    m_image = new TiledImage( image, true );
    //BufferedImage image = m_image.getAsBufferedImage();
    //    m_image = image.getAsBufferedImage();
    int height = m_image.getHeight();
    int width = m_image.getWidth();

    //    ColorModel cm = m_image.getColorModel();
    //    int[] size = cm.getComponentSize();
    //    int transparency = cm.getTransparency();
    //
    //    ColorSpace colorSpace = cm.getColorSpace();
    //    int type = colorSpace.getType();
    //
    //    Raster raster = m_image.getData();
    //
    //    DataBuffer databuffer = raster.getDataBuffer();
    //
    //    int bufferType = databuffer.getDataType();
    //
    //    System.out.println( "bounds: " + image.getBounds() + ",\ttransparency: "
    //        + transparency + ",\tcolor space type: " + type );
    //
    //    ColorModel colorModel = new ComponentColorModel( colorSpace, size, false,
    // false, Transparency.TRANSLUCENT,
    //        bufferType );
    //
    //    BufferedImage bi = new BufferedImage(colorModel, (WritableRaster)raster,
    // false, getProperties(image));
    //    
    //    m_image = bi;

    m_origBBox = GeometryFactory.createGM_Envelope( ulcx, ulcy + ( height * m_dy ), ulcx + ( width * m_dx ), ulcy );

    m_imageCS = ConvenienceCSFactory.getInstance().getOGCCSByName( result[1] );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#dispose()
   */
  public void dispose()
  {
  //nothing
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#paintSelected(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, double,
   *      org.kalypsodeegree.model.geometry.GM_Envelope, int)
   */
  public void paintSelected( Graphics g, GeoTransform p, double scale, GM_Envelope bbox, int selectionId )
  {
    if( selectionId != 0 )
      return;
    try
    {
      WMSHelper.transformImage( m_image, m_origBBox, m_localCS, m_imageCS, p, g );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

  }

  public void paintSelected( Graphics g, Graphics hg, GeoTransform p, double scale, GM_Envelope bbox, int selectionId )
  {
    paintSelected( hg, p, scale, bbox, selectionId );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getBoundingBox()
   */
  public GM_Envelope getBoundingBox()
  {
    GM_Envelope bbox = null;
    try
    {
      GeoTransformer gt = new GeoTransformer( m_localCS );
      bbox = gt.transformEnvelope( m_origBBox, m_imageCS );
    }
    catch( Exception e2 )
    {
      e2.printStackTrace();
      LOGGER.warning( "Transformation of bbox for full extend failed" );
    }
    return bbox;
  }

  public void fillLayerType( Layer layer, String id, boolean visible )
  {
    layer.setName( m_themeName );
    layer.setFeaturePath( "" );

    layer.setVisible( visible );
    layer.setId( id );
    layer.setHref( m_source );
    layer.setLinktype( m_linkType );
    layer.setActuate( "onRequest" );
    layer.setType( "simple" );
  }

  //  public void saveTheme( IProgressMonitor monitor )
  //  {
  //    // TODO Auto-generated method stub
  //    
  //  }

}