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
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;

import javax.media.jai.JAI;
import javax.media.jai.RenderedOp;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.net.IUrlResolver2;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.graphics.sld.ExternalGraphic;
import org.kalypsodeegree.xml.Marshallable;
import org.kalypsodeegree_impl.tools.Debug;

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
      m_swtImage.dispose();
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
   *            Format of the external graphic
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
   *            URL of the external graphic
   */
  public void setOnlineResource( final String onlineResource )
  {
    m_image = null;
    m_swtImage = null;
    m_onlineResource = onlineResource;
  }

  /**
   * returns the external graphic as an image. this method is not part of the sld specifications but it is added for
   * speed up applications
   * 
   * @return the external graphic as BufferedImage
   */
  public BufferedImage getAsImage( )
  {
    if( m_image == null )
    {
      RenderedOp rop = null;
      try
      {
        final URL url = m_resolver.resolveURL( m_onlineResource );
        rop = JAI.create( "url", url );
        m_image = rop.getAsBufferedImage();
      }
      catch( final IOException e )
      {
        e.printStackTrace();
      }
      finally
      {
        if( rop != null )
          rop.dispose();
      }

    }
    return m_image;
  }

  /**
   * @see org.kalypsodeegree.graphics.sld.ExternalGraphic#paintAwt(java.awt.Graphics2D)
   */
  public void paintAwt( final Graphics2D g )
  {
    /* Make sure buffered image is created */
    getAsImage();

    // Is there a better way? Is it possible to render a Jai-Image directly into an awt-graphics?
    if( m_image != null )
      g.drawImage( m_image, 0, 0, m_image.getWidth(), m_image.getHeight(), null );
  }

  /**
   * @see org.kalypsodeegree.graphics.sld.ExternalGraphic#paint(org.eclipse.swt.graphics.GC)
   */
  public void paint( final GC gc )
  {
    final Image image = loadImage( gc );
    gc.drawImage( image, 0, 0 );
  }

  private Image loadImage( final GC gc )
  {
    if( m_swtImage == null )
    {
      try
      {
        final URL url = m_resolver.resolveURL( m_onlineResource );
        final ImageDescriptor imgDesc = ImageDescriptor.createFromURL( url );
        final Image swtImage = imgDesc.createImage( true, gc.getDevice() );
        m_swtImage = swtImage;
      }
      catch( final MalformedURLException e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e, "Could not load online resource: " + m_onlineResource );
        KalypsoDeegreePlugin.getDefault().getLog().log( status );
      }
    }

    if( m_swtImage == null )
      m_swtImage = ImageDescriptor.getMissingImageDescriptor().createImage();

    return m_swtImage;
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