/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

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

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 
 history:
 
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
 
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypsodeegree.graphics;

import java.awt.image.BufferedImage;
import java.io.OutputStream;

import Acme.JPM.Encoders.GifEncoder;

import com.sun.image.codec.jpeg.JPEGCodec;
import com.sun.image.codec.jpeg.JPEGImageEncoder;
import com.sun.media.jai.codec.BMPEncodeParam;
import com.sun.media.jai.codec.ImageCodec;
import com.sun.media.jai.codec.PNGEncodeParam;
import com.sun.media.jai.codec.TIFFEncodeParam;

/**
 * 
 * This class offers three methods to encode a <tt>BuffererImage</tt> to a gif-, jpeg- or png-image.
 * 
 * <p>
 * -----------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public final class Encoders
{
  /**
   * 
   * 
   * @param out
   * @param img
   * 
   * @throws Exception
   */
  public static synchronized void encodeGif( OutputStream out, BufferedImage img ) throws Exception
  {
    GifEncoder encoder = new GifEncoder( img, out );
    encoder.encode();
    out.close();
  }

  /**
   * 
   * 
   * @param out
   * @param img
   * 
   * @throws Exception
   */
  public static synchronized void encodeBmp( OutputStream out, BufferedImage img ) throws Exception
  {
    BMPEncodeParam encodeParam = new BMPEncodeParam();

    com.sun.media.jai.codec.ImageEncoder encoder = ImageCodec.createImageEncoder( "BMP", out, encodeParam );

    encoder.encode( img );
    out.close();
  }

  /**
   * 
   * 
   * @param out
   * @param img
   * 
   * @throws Exception
   */
  public static synchronized void encodePng( OutputStream out, BufferedImage img ) throws Exception
  {
    PNGEncodeParam encodeParam = PNGEncodeParam.getDefaultEncodeParam( img );

    com.sun.media.jai.codec.ImageEncoder encoder = ImageCodec.createImageEncoder( "PNG", out, encodeParam );

    encoder.encode( img );
    out.close();
  }

  /**
   * 
   * 
   * @param out
   * @param img
   * 
   * @throws Exception
   */
  public static synchronized void encodeTiff( OutputStream out, BufferedImage img ) throws Exception
  {
    TIFFEncodeParam encodeParam = new TIFFEncodeParam();

    com.sun.media.jai.codec.ImageEncoder encoder = ImageCodec.createImageEncoder( "TIFF", out, encodeParam );

    encoder.encode( img );
    out.close();
  }

  /**
   * 
   * 
   * @param out
   * @param img
   * 
   * @throws Exception
   */
  public static synchronized void encodeJpeg( OutputStream out, BufferedImage img ) throws Exception
  {

    // encode JPEG
    JPEGImageEncoder encoder = JPEGCodec.createJPEGEncoder( out );
    com.sun.image.codec.jpeg.JPEGEncodeParam jpegParams = encoder.getDefaultJPEGEncodeParam( img );
    jpegParams.setQuality( 0.95f, false );
    encoder.setJPEGEncodeParam( jpegParams );

    encoder.encode( img );
  }

  /**
   * 
   * 
   * @param out
   * @param img
   * @param quality
   * 
   * @throws Exception
   */
  public static synchronized void encodeJpeg( OutputStream out, BufferedImage img, float quality ) throws Exception
  {

    // encode JPEG
    JPEGImageEncoder encoder = JPEGCodec.createJPEGEncoder( out );
    com.sun.image.codec.jpeg.JPEGEncodeParam jpegParams = encoder.getDefaultJPEGEncodeParam( img );
    jpegParams.setQuality( quality, false );
    encoder.setJPEGEncodeParam( jpegParams );

    encoder.encode( img );
  }
}