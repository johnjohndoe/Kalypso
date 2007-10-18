package org.kalypso.risk.model.jaitests;

import java.awt.Dimension;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.DataBuffer;
import java.awt.image.Raster;
import java.awt.image.RenderedImage;
import java.awt.image.SampleModel;
import java.awt.image.WritableRaster;
import java.awt.image.renderable.ParameterBlock;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.util.HashMap;

import javax.media.jai.ImageLayout;
import javax.media.jai.Interpolation;
import javax.media.jai.JAI;
import javax.media.jai.PlanarImage;
import javax.media.jai.RenderedOp;

import com.sun.media.jai.codec.ImageCodec;
import com.sun.media.jai.codec.ImageEncoder;
import com.sun.media.jai.codec.JPEGEncodeParam;

/**
 * A utility class that cuts images into multiple subimages and saves them in the JPEG format.. To run this utility
 * type: java MultipleImageFactory <imagefile> <tilewidth> <tile height>
 * 
 * @version 1.0 1 April 2002
 * @author Lawrence Rodrigues
 */
public class MultipleImageFactory
{

  public static void main( String[] args )
  {
    MultipleImageFactory mfact = new MultipleImageFactory();
    int tileWidth = 256;
    int tileHeight = 256;
//    if( args.length < 1 )
//    {
//      System.out.println( "Enter a valid image file name" );
//      System.exit( 0 );
//
//    }
//    else
//    {
//      if( args.length > 2 )
//      {
//        try
//        {
//          tileWidth = Integer.parseInt( args[1] );
//          tileHeight = Integer.parseInt( args[2] );
//
//        }
//        catch( Exception e )
//        {
//        }
//
//      }
//    }
//    PlanarImage img = MultipleImageFactory.readAsPlanarImage( args[0] );
    PlanarImage img = MultipleImageFactory.readAsPlanarImage( "D:/__test/jai/test01.jpg" );
    mfact.createTilesAndSave( img, tileWidth, tileHeight );
  }

  public void createTilesAndSave( PlanarImage image, int tileWidth, int tileHeight )
  {
    if( image == null )
      return;
    int imageWidth = image.getWidth();
    int imageHeight = image.getHeight();

    PlanarImage rfmtImage = reformatImage( image, new Dimension( tileWidth, tileHeight ) );
    SampleModel sampleModel = rfmtImage.getSampleModel();
    ColorModel colorModel = rfmtImage.getColorModel();
    int numXTiles = rfmtImage.getNumXTiles();
    int numYTiles = rfmtImage.getNumYTiles();

    for( int j = 0; j < numYTiles; j++ )
    {
      for( int i = 0; i < numXTiles; i++ )
      {
        Raster tile = rfmtImage.getTile( i, j );
        DataBuffer dataBuffer = tile.getDataBuffer();
        WritableRaster wr = tile.createWritableRaster( sampleModel, dataBuffer, new Point( 0, 0 ) );
        BufferedImage bi = new BufferedImage( colorModel, wr, colorModel.isAlphaPremultiplied(), null );

        try
        {
          saveAsJPEG( bi, new String( "tile" + (new Integer( j )).toString() + (new Integer( i )).toString() ) );
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }

      }

    }
  }

  public static PlanarImage readAsPlanarImage( String filename )
  {
    return JAI.create( "fileload", filename );
  }

  public static void saveAsJPEG( RenderedImage image, String file ) throws java.io.IOException
  {
    String filename = file;
    if( !filename.endsWith( ".jpg" ) )
      filename = new String( file + ".jpg" );
    OutputStream out = new FileOutputStream( filename );
    JPEGEncodeParam param = new JPEGEncodeParam();
    ImageEncoder encoder = ImageCodec.createImageEncoder( "JPEG", out, param );
    encoder.encode( image );
    out.close();
  }

  public static RenderedOp reformatImage( PlanarImage img, Dimension tileDim )
  {
    ImageLayout tileLayout = new ImageLayout( img );
    tileLayout.setTileWidth( tileDim.width );
    tileLayout.setTileHeight( tileDim.width );

    HashMap map = new HashMap();
    map.put( JAI.KEY_IMAGE_LAYOUT, tileLayout );
    map.put( JAI.KEY_INTERPOLATION, Interpolation.getInstance( Interpolation.INTERP_BICUBIC ) );

    RenderingHints tileHints = new RenderingHints( map );
    ParameterBlock pb = new ParameterBlock();
    pb.addSource( img );
    return JAI.create( "format", pb, tileHints );
  }
}
