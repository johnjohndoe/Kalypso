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
package org.deegree.graphics;

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
 * This class offers three methods to encode a <tt>BuffererImage</tt> to
 * a gif-, jpeg- or png-image.
 *
 * <p>-----------------------------------------------------</p>
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
public final class Encoders {
    /**
     *
     *
     * @param out 
     * @param img 
     *
     * @throws Exception 
     */
    public static synchronized void encodeGif( OutputStream out, BufferedImage img )
                                       throws Exception {
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
    public static synchronized void encodeBmp( OutputStream out, BufferedImage img )
                                       throws Exception {
        BMPEncodeParam encodeParam = new BMPEncodeParam();

        com.sun.media.jai.codec.ImageEncoder encoder = ImageCodec.createImageEncoder( "BMP", out, 
                                                                                      encodeParam );

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
    public static synchronized void encodePng( OutputStream out, BufferedImage img )
                                       throws Exception {
        PNGEncodeParam encodeParam = PNGEncodeParam.getDefaultEncodeParam( img );

        com.sun.media.jai.codec.ImageEncoder encoder = ImageCodec.createImageEncoder( "PNG", out, 
                                                                                      encodeParam );

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
    public static synchronized void encodeTiff( OutputStream out, BufferedImage img )
                                        throws Exception {
        TIFFEncodeParam encodeParam = new TIFFEncodeParam();

        com.sun.media.jai.codec.ImageEncoder encoder = ImageCodec.createImageEncoder( "TIFF", out, 
                                                                                      encodeParam );

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
    public static synchronized void encodeJpeg( OutputStream out, BufferedImage img )
                                        throws Exception {

        // encode JPEG
        JPEGImageEncoder encoder = JPEGCodec.createJPEGEncoder( out );
        com.sun.image.codec.jpeg.JPEGEncodeParam jpegParams = encoder.getDefaultJPEGEncodeParam( 
                                                                      img );
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
    public static synchronized void encodeJpeg( OutputStream out, BufferedImage img, float quality )
                                        throws Exception {

        // encode JPEG
        JPEGImageEncoder encoder = JPEGCodec.createJPEGEncoder( out );
        com.sun.image.codec.jpeg.JPEGEncodeParam jpegParams = encoder.getDefaultJPEGEncodeParam( 
                                                                      img );
        jpegParams.setQuality( quality, false );
        encoder.setJPEGEncodeParam( jpegParams );

        encoder.encode( img );
    }
}