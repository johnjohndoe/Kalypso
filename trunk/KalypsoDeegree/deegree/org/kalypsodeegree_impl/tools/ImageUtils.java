// $Header: /cvsroot/deegree/deegree/org/deegree_impl/tools/ImageUtils.java,v
// 1.1 2004/04/02 06:41:56 poth Exp $
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
package org.deegree_impl.tools;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.InputStream;
import java.io.RandomAccessFile;
import java.net.URL;

import javax.media.jai.JAI;
import javax.media.jai.RenderedOp;

import com.sun.media.jai.codec.FileSeekableStream;
import com.sun.media.jai.codec.MemoryCacheSeekableStream;
import com.sun.media.jai.codec.SeekableStream;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author last edited by: $Author$
 * 
 * @version 1.0. $Revision$, $Date$
 * 
 * @since 1.1
 */
public class ImageUtils
{
  /**
   * 
   * 
   * @param url
   * 
   * @return @throws
   *         IOException
   */
  public static BufferedImage loadImage( URL url ) throws IOException
  {
    InputStream is = url.openStream();
    SeekableStream fss = new MemoryCacheSeekableStream( is );
    RenderedOp ro = JAI.create( "stream", fss );
    BufferedImage img = ro.getAsBufferedImage();
    fss.close();
    is.close();
    return img;
  }

  /**
   * 
   * 
   * @param fileName
   * 
   * @return @throws
   *         IOException
   */
  public static BufferedImage loadImage( String fileName ) throws IOException
  {
    SeekableStream fss = new FileSeekableStream( new RandomAccessFile( fileName, "r" ) );
    RenderedOp ro = JAI.create( "stream", fss );
    BufferedImage img = ro.getAsBufferedImage();
    fss.close();

    return img;
  }
}
/*******************************************************************************
 * Changes to this class. What the people have been up to: $Log:
 * ImageUtils.java,v $ Revision 1.1 2004/04/02 06:41:56 poth no message
 * 
 * 
 *  
 ******************************************************************************/