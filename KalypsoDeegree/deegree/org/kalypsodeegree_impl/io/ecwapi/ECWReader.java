/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2003 by:
 IDgis bv, Holten, The Netherlands
 http://www.idgis.nl

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

 ---------------------------------------------------------------------------*/
package org.deegree_impl.io.ecwapi;

import java.awt.Graphics;
import java.awt.image.BufferedImage;

import org.deegree.model.geometry.GM_Envelope;
import org.deegree_impl.tools.Debug;

import com.ermapper.ecw.JNCSException;
import com.ermapper.ecw.JNCSFile;

/**
 * ECWReader.java
 * 
 * @author Herman Assink
 * @version 1.0 2003-11-06
 */

public class ECWReader
{
  private JNCSFile ecwFile;

  /**
   * read part from ECW-file which falls within env and return this part
   * dimenions width and height
   * 
   * @param fileName
   *          full pathname of the ECW-file
   */
  public ECWReader( String fileName ) throws JNCSException
  {
    this.ecwFile = new JNCSFile( fileName, false );
  }

  /**
   * read part from ECW-file which falls within env and return this part as
   * BufferedImage with dimenions width and height
   * 
   * @param env
   *          bounding box in world coordinates of requested part
   * @param width
   *          width of the returned image
   * @param height
   *          height of the returned image
   */
  public BufferedImage getBufferedImage( GM_Envelope env, int width, int height )
      throws JNCSException
  {

    Debug.debugMethodBegin();

    int bandlist[];
    int line, pRGBArray[] = null;

    // Setup the view parameters for the ecw file.
    bandlist = new int[ecwFile.numBands];
    for( int i = 0; i < ecwFile.numBands; i++ )
    {
      bandlist[i] = i;
    }

    //Check if the envelope is within the area of the ecw-image
    double dWorldTLX = env.getMin().getX();
    double dWorldTLY = env.getMax().getY();

    //System.out.println("tlx: " + dWorldTLX + " tly: " + dWorldTLY);

    if( dWorldTLX < ecwFile.originX )
      dWorldTLX = ecwFile.originX;
    if( dWorldTLY > ecwFile.originY )
      dWorldTLY = ecwFile.originY;
    double dWorldBRX = env.getMax().getX();
    double dWorldBRY = env.getMin().getY();

    //System.out.println("brx: " + dWorldBRX + " bry: " + dWorldBRY);

    if( dWorldBRX > ( ecwFile.originX + ( ( ecwFile.width - 1 ) * ecwFile.cellIncrementX ) ) ) // Huh?
      // ECW
      // does
      // not
      // except
      // the
      // full
      // width
      dWorldBRX = ecwFile.originX + ( ( ecwFile.width - 1 ) * ecwFile.cellIncrementX );
    if( dWorldBRY < ( ecwFile.originY + ( ecwFile.height * ecwFile.cellIncrementY ) - ( ecwFile.cellIncrementY / 2 ) ) )
      dWorldBRY = ecwFile.originY + ( ecwFile.height * ecwFile.cellIncrementY )
          - ( ecwFile.cellIncrementY / 2 );

    // Work out the correct aspect for the setView call.
    double dEnvAspect = ( dWorldBRX - dWorldTLX ) / ( dWorldTLY - dWorldBRY );
    double dImgAspect = (double)width / (double)height;

    //System.out.println("tlx: " + dWorldTLX + " tly: " + dWorldTLY);
    //System.out.println("brx: " + dWorldBRX + " bry: " + dWorldBRY);
    //System.out.println("width: " + width + " height: " + height);

    // Check for supersampling
    int viewWidth = width;
    int viewHeight = height;
    double viewIncrX = ( dWorldBRX - dWorldTLX ) / width;
    double viewIncrY = ( dWorldTLY - dWorldBRY ) / height;
    if( viewIncrX < ecwFile.cellIncrementX || viewIncrY < ecwFile.cellIncrementY )
    {
      //Remember: cellIncrementX & -Y can be different
      if( ecwFile.cellIncrementX / viewIncrX > ecwFile.cellIncrementY / viewIncrY )
      {
        viewWidth = (int)( ( dWorldBRX - dWorldTLX ) / (double)ecwFile.cellIncrementX );
        viewHeight = (int)(double)( viewWidth / dImgAspect );
      }
      else
      {
        viewHeight = (int)( ( dWorldTLY - dWorldBRY ) / (double)ecwFile.cellIncrementY * -1.0 );
        viewWidth = (int)( (double)viewHeight * dImgAspect );
      }
    }

    // Create an image of the ecw file.
    BufferedImage ecwImage = new BufferedImage( viewWidth, viewHeight, BufferedImage.TYPE_INT_RGB );
    pRGBArray = new int[width];

    //System.out.println("viewWidth: " + viewWidth + " viewHeight: " +
    // viewHeight);

    // Set the view
    ecwFile.setView( ecwFile.numBands, bandlist, dWorldTLX, dWorldTLY, dWorldBRX, dWorldBRY,
        viewWidth, viewHeight );

    // Read the scan lines
    for( line = 0; line < viewHeight; line++ )
    {
      ecwFile.readLineRGBA( pRGBArray );
      ecwImage.setRGB( 0, line, viewWidth, 1, pRGBArray, 0, viewWidth );
    }

    if( width != viewWidth || height != viewHeight )
    {
      //System.out.println("enlarge image");
      BufferedImage enlargedImg = new BufferedImage( width, height, BufferedImage.TYPE_INT_RGB );
      Graphics g = enlargedImg.getGraphics();
      g.drawImage( ecwImage, 0, 0, width, height, 0, 0, viewWidth, viewHeight, null );
      ecwImage = enlargedImg;
    }

    Debug.debugMethodEnd();
    return ecwImage;

  }
}