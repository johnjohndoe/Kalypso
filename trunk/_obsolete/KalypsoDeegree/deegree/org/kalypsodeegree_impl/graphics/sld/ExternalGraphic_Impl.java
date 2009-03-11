/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.graphics.sld;

import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.awt.image.DataBufferInt;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;

import javax.media.jai.JAI;
import javax.media.jai.RenderedOp;

import org.apache.batik.transcoder.TranscoderException;
import org.apache.batik.transcoder.TranscoderInput;
import org.apache.batik.transcoder.TranscoderOutput;
import org.apache.batik.transcoder.image.PNGTranscoder;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.PaletteData;
import org.eclipse.swt.widgets.Display;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.net.IUrlResolver2;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.graphics.sld.ExternalGraphic;
import org.kalypsodeegree.xml.Marshallable;
import org.kalypsodeegree_impl.tools.Debug;
import org.kalypsodeegree_impl.tools.NetWorker;

import com.sun.media.jai.codec.MemoryCacheSeekableStream;

/**
 * The ExternalGraphic element allows a reference to be made to an external graphic file with a Web URL. The
 * OnlineResource sub-element gives the URL and the Format sub-element identifies the expected document MIME type of a
 * successful fetch. Knowing the MIME type in advance allows the styler to select the best- supported format from the
 * list of URLs with equivalent content.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public class ExternalGraphic_Impl implements ExternalGraphic, Marshallable
{
  private BufferedImage m_image = null;

  private String m_format = null;

  private String m_onlineResource = null;

  private final IUrlResolver2 m_resolver;

  private Image m_swtImage;

  private ByteArrayOutputStream m_bos;

  private TranscoderOutput m_output;

  private PNGTranscoder m_transcoder;

  private TranscoderInput m_transcoderInput;

  /**
   * Creates a new ExternalGraphic_Impl object.
   * 
   * @param format
   * @param onlineResource
   */
  ExternalGraphic_Impl( final IUrlResolver2 resolver, final String format, final String onlineResource )
  {
    m_resolver = resolver;
    setFormat( format );
    setOnlineResource( onlineResource );
  }

  public void dispose( )
  {
    if( m_swtImage != null )
    {
      m_swtImage.dispose();
    }
  }

  /**
   * the Format sub-element identifies the expected document MIME type of a successful fetch.
   * 
   * @return Format of the external graphic
   */
  public String getFormat( )
  {
    return m_format;
  }

  /**
   * sets the format (MIME type)
   * 
   * @param format
   *          Format of the external graphic
   */
  public void setFormat( final String format )
  {
    m_format = format;
  }

  /**
   * The OnlineResource gives the URL of the external graphic
   * 
   * @return URL of the external graphic
   */
  public String getOnlineResource( )
  {
    return m_onlineResource;
  }

  /**
   * @see org.kalypsodeegree.graphics.sld.ExternalGraphic#getOnlineResourceURL()
   */
  public URL getOnlineResourceURL( ) throws MalformedURLException
  {
    return m_resolver.resolveURL( m_onlineResource );
  }

  /**
   * sets the online resource / URL of the external graphic
   * 
   * @param onlineResource
   *          URL of the external graphic
   */
  public void setOnlineResource( final String onlineResource )
  {
    m_image = null;
    m_swtImage = null;
    m_onlineResource = onlineResource;

    try
    {
      final URL url = getOnlineResourceURL();
      final String file = url.getFile();
      final int idx = file.indexOf( "$" );
      if( idx == -1 )
      {
        retrieveImage( url );
      }
    }
    catch( final MalformedURLException e )
    {
      KalypsoDeegreePlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }
  }

  /**
   * @param onlineResource
   */
  private void retrieveImage( final URL onlineResource )
  {

    try
    {
      final String t = onlineResource.toExternalForm();
      if( t.trim().toLowerCase().endsWith( ".svg" ) )
      {
        // initialize the the classes required for svg handling
        m_bos = new ByteArrayOutputStream( 2000 );
        m_output = new TranscoderOutput( m_bos );
        // PNGTranscoder is needed to handle transparent parts
        // of a SVG
        m_transcoder = new PNGTranscoder();
        try
        {
          m_transcoderInput = new TranscoderInput( NetWorker.url2String( onlineResource ) );
        }
        catch( final Exception e )
        {
          e.printStackTrace();
        }
      }
      else
      {
        final InputStream is = onlineResource.openStream();
        final MemoryCacheSeekableStream mcss = new MemoryCacheSeekableStream( is );
        final RenderedOp rop = JAI.create( "stream", mcss );
        m_image = rop.getAsBufferedImage();
        mcss.close();
        is.close();
      }
    }
    catch( final IOException e )
    {
      System.out.println( "Yikes: " + e );
    }
  }

  /**
   * returns the external graphic as an image. this method is not part of the sld specifications but it is added for
   * speed up applications
   * 
   * @return the external graphic as BufferedImage
   */
  public BufferedImage getAsImage( final int targetSizeX, final int targetSizeY )
  {
    if( m_image == null )
    {
      if( m_transcoderInput != null )
      {
        m_transcoder.addTranscodingHint( PNGTranscoder.KEY_HEIGHT, new Float( targetSizeX ) );
        m_transcoder.addTranscodingHint( PNGTranscoder.KEY_WIDTH, new Float( targetSizeY ) );
        try
        {
          m_transcoder.transcode( m_transcoderInput, m_output );
          try
          {
            m_bos.flush();
            m_bos.close();
          }
          catch( final IOException e3 )
          {
            e3.printStackTrace();
          }
        }
        catch( final TranscoderException e )
        {
          e.printStackTrace();
        }
        try
        {
          final ByteArrayInputStream is = new ByteArrayInputStream( m_bos.toByteArray() );
          final MemoryCacheSeekableStream mcss = new MemoryCacheSeekableStream( is );
          final RenderedOp rop = JAI.create( "stream", mcss );
          m_image = rop.getAsBufferedImage();
          mcss.close();
        }
        catch( final IOException e1 )
        {
          e1.printStackTrace();
        }
      }
    }

    return m_image;
  }

  /**
   * @see org.kalypsodeegree.graphics.sld.ExternalGraphic#paintAwt(java.awt.Graphics2D)
   */
  public void paintAwt( final Graphics2D g, final int targetSizeX, final int targetSizeY )
  {
    /* Make sure buffered image is created */
    getAsImage( targetSizeX, targetSizeY );

    // Is there a better way? Is it possible to render a Jai-Image directly into an awt-graphics?
    if( m_image != null )
    {
      g.drawImage( m_image, 0, 0, m_image.getWidth(), m_image.getHeight(), null );
    }
  }

  /**
   * @see org.kalypsodeegree.graphics.sld.ExternalGraphic#paint(org.eclipse.swt.graphics.GC)
   */
  public void paint( final GC gc )
  {
    if( m_image != null )
    {
      if( m_swtImage == null )
      {
        try
        {
          m_swtImage = makeSWTImage( null, m_image );
        }
        catch( final Exception e )
        {
          KalypsoDeegreePlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
        }
      }

      gc.drawImage( m_swtImage, 0, 0 );
    }
  }

  /**
   * implementation taken from http://dev.eclipse.org/newslists/news.eclipse.platform.swt/msg10712.html
   */
  private static Image makeSWTImage( final Display display, final java.awt.Image ai ) throws Exception
  {
    // TODO transparent pixel (ATM transparent pixels are converted into black pixels)
    
    final int width = ai.getWidth( null );
    final int height = ai.getHeight( null );

    final BufferedImage bufferedImage = new BufferedImage( width, height, BufferedImage.TYPE_INT_RGB );

    final Graphics2D g2d = bufferedImage.createGraphics();
    g2d.drawImage( ai, 0, 0, null );
    g2d.dispose();

    final int[] data = ((DataBufferInt) bufferedImage.getData().getDataBuffer()).getData();
    final ImageData imageData = new ImageData( width, height, 24, new PaletteData( 0xFF0000, 0x00FF00, 0x0000FF ) );
    imageData.setPixels( 0, 0, data.length, data, 0 );

    final Image swtImage = new Image( display, imageData );

    return swtImage;
  }

  /**
   * exports the content of the ExternalGraphic as XML formated String
   * 
   * @return xml representation of the ExternalGraphic
   */
  public String exportAsXML( )
  {
    Debug.debugMethodBegin();

    final StringBuffer sb = new StringBuffer( 200 );
    sb.append( "<ExternalGraphic>" );
    sb.append( "<OnlineResource xmlns:xlink='http://www.w3.org/1999/xlink' " );
    sb.append( "xlink:type='simple' xlink:href='" );
    // sb.append( NetWorker.url2String( m_onlineResource ) + "'/>" );
    sb.append( m_onlineResource + "'/>" );
    sb.append( "<Format>" ).append( m_format ).append( "</Format>" );
    sb.append( "</ExternalGraphic>" );

    Debug.debugMethodEnd();
    return sb.toString();
  }

}