/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree
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
 E-Mail: fitzke@giub.uni-bonn.de


 ---------------------------------------------------------------------------*/
package org.deegree_impl.model.gc;

import java.awt.Graphics;
import java.awt.color.ColorSpace;
import java.awt.image.BufferedImage;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferByte;
import java.io.IOException;
import java.rmi.RemoteException;

import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.coverage.CoverageCreationException;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree_impl.graphics.transformation.WorldToScreenTransform;
import org.deegree_impl.model.cv.CV_Coverage_Impl;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.tools.Debug;
import org.opengis.cs.CS_CoordinateSystem;
import org.opengis.cv.CV_PaletteInterpretation;
import org.opengis.cv.CV_SampleDimensionType;
import org.opengis.gc.GC_GridCoverage;
import org.opengis.gc.GC_GridGeometry;
import org.opengis.gc.GC_GridPacking;
import org.opengis.gc.GC_GridRange;
import org.opengis.pt.PT_CoordinatePoint;
import org.opengis.pt.PT_Envelope;

/**
 * Represent the basic implementation which provides access to grid coverage
 * data. A <code>GC_GridCoverage</code> implementation may provide the ability
 * to update grid values.
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 2.11.2002
 */
public class ImageGridCoverage extends CV_Coverage_Impl implements GC_GridCoverage
{
  private boolean isEditable = false;

  private GC_GridGeometry[] overviewGridGeometry = null;

  private GC_GridCoverage[] overviews = null;

  private GC_GridGeometry gridGeometry = null;

  private GC_GridPacking gridPacking = null;

  private GeoTransform gt = null;

  private PT_Envelope envImg = null;

  private BufferedImage image = null;

  /**
   * initialzies a simple <tt>CV_Coverage</tt> with an image, coordinate
   * reference system, an envelope describing the spatial extension of of the
   * coverage and the coverages metadata.
   * 
   * @param isEditable
   *          indicates if the grid coverages data can be edited
   */
  public ImageGridCoverage( BufferedImage image, GM_Envelope bbox, CS_CoordinateSystem crs,
      boolean isEditable )
  {
    super( crs );

    this.image = image;

    this.isEditable = isEditable;

    double minx = bbox.getMin().getX();
    double maxx = bbox.getMax().getX();
    double miny = bbox.getMin().getY();
    double maxy = bbox.getMax().getY();

    double minz = 0;
    double maxz = 0;

    envelope = new PT_Envelope();
    envelope.minCP = new PT_CoordinatePoint( minx, miny );
    envelope.maxCP = new PT_CoordinatePoint( maxx, maxy );

    envImg = new PT_Envelope();
    envImg.minCP = new PT_CoordinatePoint( 0, 0 );
    envImg.maxCP = new PT_CoordinatePoint( image.getWidth() - 1, image.getHeight() - 1 );

    gt = new WorldToScreenTransform( envelope.minCP.ord[0], envelope.minCP.ord[1],
        envelope.maxCP.ord[0], envelope.maxCP.ord[1], envImg.minCP.ord[0], envImg.minCP.ord[1],
        envImg.maxCP.ord[0], envImg.maxCP.ord[1] );
  }

  /**
   * returns the paletteinterpretation of the grid coverages bands
   */
  public CV_PaletteInterpretation getPaletteInterpretation()
  {
    int type = 11111;
    CV_PaletteInterpretation pi = null;
    switch( type )
    {
    case ColorSpace.TYPE_RGB:
      pi = new CV_PaletteInterpretation( CV_PaletteInterpretation.CV_RGB );
      break;
    case ColorSpace.TYPE_CMYK:
      pi = new CV_PaletteInterpretation( CV_PaletteInterpretation.CV_CMYK );
      break;
    case ColorSpace.TYPE_GRAY:
      pi = new CV_PaletteInterpretation( CV_PaletteInterpretation.CV_Gray );
      break;
    case ColorSpace.TYPE_HLS:
      pi = new CV_PaletteInterpretation( CV_PaletteInterpretation.CV_HLS );
      break;
    }
    return pi;
  }

  public CV_SampleDimensionType getSampleDimensionType()
  {
    return new CV_SampleDimensionType( CV_SampleDimensionType.CV_8BIT_U );
  }

  /**
   * Returns <code>true</code> if grid data can be edited.
   * 
   * @return <code>true</code> if grid data can be edited.
   * @throws RemoteException
   *           if a remote method call failed.
   */
  public boolean isDataEditable() throws RemoteException
  {
    return isEditable;
  }

  /**
   * Set a block of grid coverage data for all sample dimensions. See
   * <code>getDataBlock</code> for details on how to pack the values.
   * 
   * The requested grid range must satisfy the following rules for each
   * dimension of the grid coverage:
   * 
   * <blockquote>
   * 
   * <pre>
   * Min grid coordinate <= grid range minimum <= grid range maximum <= maximum grid coordinate
   * </pre>
   * 
   * </blockquote>
   * 
   * For byte padding rules see <code>getDataBlock</code>.
   * 
   * @param gridRange
   *          Grid range for block of data to be accessed.
   * @param values
   *          Sequence of grid values for the given region.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public void setPackedDataBlock( GC_GridRange gridRange, byte[] values ) throws RemoteException
  {}

  /**
   * Set a block of bint values for all sample dimensions.
   * 
   * @param gridRange
   *          Grid range for block of data to be accessed.
   * @param values
   *          Sequence of grid values for the given region.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public void setDataBlockAsInteger( GC_GridRange gridRange, int[] values ) throws RemoteException
  {}

  /**
   * Set a block of double values for all sample dimensions.
   * 
   * @param gridRange
   *          Grid range for block of data to be accessed.
   * @param values
   *          Sequence of grid values for the given region.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public void setDataBlockAsDouble( GC_GridRange gridRange, double[] values )
      throws RemoteException
  {}

  /**
   * Set a block of byte values for all sample dimensions.
   * 
   * @param gridRange
   *          Grid range for block of data to be accessed.
   * @param values
   *          Sequence of grid values for the given region.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public void setDataBlockAsByte( GC_GridRange gridRange, byte[] values ) throws RemoteException
  {}

  /**
   * Set a block of boolean values for all sample dimensions. The requested grid
   * range must satisfy the following rules for each dimension of the grid
   * coverage:
   * 
   * <blockquote>
   * 
   * <pre>
   * Min grid coordinate <= grid range minimum <= grid range maximum <= maximum grid coordinate
   * </pre>
   * 
   * </blockquote>
   * 
   * The number of values must equal:
   * 
   * <blockquote>
   * 
   * <pre>
   * (Max1   Min1 + 1) * (Max2   Min2 + 1)... * (Maxn   Minn + 1) * numberSampleDimensions
   * </pre>
   * 
   * </blockquote>
   * 
   * Where
   * 
   * <UL>
   * <li>Min is the mimium ordinate in the grid range</li>
   * <li>Max is the maximum ordinate in the grid range</li>
   * <li>N is the number of dimensions in the grid coverage</li>
   * </UL>
   * 
   * @param gridRange
   *          Grid range for block of data to be accessed.
   * @param values
   *          Sequence of grid values for the given region.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public void setDataBlockAsBoolean( GC_GridRange gridRange, boolean[] values )
      throws RemoteException
  {}

  /**
   * Return a sequence of double values for a block. A value for each sample
   * dimension will be returned.
   * 
   * @param gridRange
   *          Grid range for block of data to be accessed.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public double[] getValueBlockAsDouble( GC_GridRange gridRange ) throws RemoteException
  {
    return null;
  }

  /**
   * Return a block of grid coverage data for all sample dimensions. A value for
   * each sample dimension will be returned. This operation provides efficient
   * access of the grid values. The sequencing order of the values in the
   * sequence will follow the rules given by valueInBytePacking and bandPacking
   * defined in GC_GridPacking.
   * 
   * The requested grid range must satisfy the following rules for each
   * dimension of the grid coverage:
   * 
   * <blockquote>
   * 
   * <pre>
   * Min grid coordinate <= grid range minimum <= grid range maximum <= maximum grid coordinate
   * </pre>
   * 
   * </blockquote>
   * 
   * The sequence of bytes returned will match the data type of the dimension.
   * For example, a grid with one 16 bit unsigned (CV_16BIT_U) sample dimension
   * will return 2 bytes for every cell in the block. <br>
   * <br>
   * <strong>Byte padding Rules for grid values of less than 8 bits </strong>
   * <br>
   * For 2 D grid coverages, padding is to the nearest byte for the following
   * cases:
   * 
   * <table border=0>
   * <tr>
   * <td>For PixelInterleaved</td>
   * <td>For grids with multiple sample dimensions, padding occurs between
   * pixels for each change in dimension type.</td>
   * </tr>
   * <tr>
   * <td>For LineInterleaved</td>
   * <td>Padding occurs at the end of each row or column (depending on the
   * valueSequence of the grid).</td>
   * </tr>
   * <tr>
   * <td>For BandSequencial</td>
   * <td>Padding occurs at the end of every sample dimension.</td>
   * </tr>
   * </table>
   * 
   * For grid values smaller than 8 bits, their order within each byte is given
   * by the value defined in
   * {@link GC_GridPacking#getValueInBytePacking valueInBytePacking}. For grid
   * values bigger than 8 bits, the order of their bytes is given by the value
   * defined in {@link GC_GridPacking#getByteInValuePacking byteInValuePacking}.
   * 
   * @param gridRange
   *          Grid range for block of data to be accessed.
   * @return a block of grid coverage data for all sample dimensions.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public byte[] getPackedDataBlock( GC_GridRange gridRange ) throws RemoteException
  {
    return null;
  }

  /**
   * Return the grid geometry for an overview.
   * 
   * @param overviewIndex
   *          Overview index for which to retrieve grid geometry. Indices start
   *          at 0.
   * @return the grid geometry for an overview.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public GC_GridGeometry getOverviewGridGeometry( int overviewIndex ) throws RemoteException
  {
    return overviewGridGeometry[overviewIndex];
  }

  /**
   * Returns a pre-calculated overview for a grid coverage. The overview indices
   * are numbered from 0 to <code>numberOverviews-1</code>. The overviews are
   * ordered from highest (index 0) to lowest (numberOverviews -1) resolution.
   * Overview grid coverages will have overviews which are the overviews for the
   * grid coverage with lower resolution than the overview. For example, a 1
   * meter grid coverage with 3, 9, and 27 meter overviews will be ordered as
   * follows:
   * 
   * <table border=0 align="center">
   * <tr>
   * <td align="center">Index</td>
   * <td align="center">resolution</td>
   * </tr>
   * <tr>
   * <td align="center">0</td>
   * <td align="center">3</td>
   * </tr>
   * <tr>
   * <td align="center">1</td>
   * <td align="center">9</td>
   * </tr>
   * <tr>
   * <td align="center">2</td>
   * <td align="center">27</td>
   * </tr>
   * </table> <br>
   * <br>
   * 
   * The 3 meter overview will have 2 overviews as follows: <table border=0
   * align="center">
   * <tr>
   * <td align="center">Index</td>
   * <td align="center">resolution</td>
   * </tr>
   * <tr>
   * <td align="center">0</td>
   * <td align="center">9</td>
   * </tr>
   * <tr>
   * <td align="center">1</td>
   * <td align="center">27</td>
   * </tr>
   * </table>
   * 
   * @param overviewIndex
   *          Index of grid coverage overview to retrieve. Indexes start at 0.
   * @return a pre-calculated overview for a grid coverage.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public GC_GridCoverage getOverview( int overviewIndex ) throws RemoteException
  {
    return overviews[overviewIndex];
  }

  /**
   * Optimal size to use for each dimension when accessing grid values. These
   * values together give the optimal block size to use when retrieving grid
   * coverage values. For example, a client application can achieve better
   * performance for a 2-D grid coverage by reading blocks of 128 by 128 if the
   * grid is tiled into blocks of this size. The sequence is ordered by
   * dimension. If the implementation does not have optimal sizes the sequence
   * will be empty.
   * 
   * @return the optimal size to use for each dimension when accessing grid
   *         values.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public int[] getOptimalDataBlockSizes() throws RemoteException
  {
    return null;
  }

  /**
   * Number of predetermined overviews for the grid.
   * 
   * @return the number of predetermined overviews for the grid.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public int getNumOverviews() throws RemoteException
  {
    if( overviews != null )
    {
      return overviews.length;
    }
    else
    {
      return 0;
    }
  }

  /**
   * Information for the packing of grid coverage values.
   * 
   * @return the information for the packing of grid coverage values.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public GC_GridPacking getGridPacking() throws RemoteException
  {
    return gridPacking;
  }

  /**
   * Information for the grid coverage geometry. Grid geometry includes the
   * valid range of grid coordinates and the georeferencing.
   * 
   * @return the information for the grid coverage geometry.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public GC_GridGeometry getGridGeometry() throws RemoteException
  {
    return gridGeometry;
  }

  /**
   * Return a sequence of int values for a block. A value for each sample
   * dimension will be returned.
   * 
   * @param gridRange
   *          Grid range for block of data to be accessed.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public int[] getDataBlockAsInteger( GC_GridRange gridRange ) throws RemoteException
  {
    return null;
  }

  /**
   * Return a sequence of byte values for a block. A value for each sample
   * dimension will be returned.
   * 
   * @param gridRange
   *          Grid range for block of data to be accessed.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public byte[] getDataBlockAsByte( GC_GridRange gridRange ) throws RemoteException
  {

    int[] hi = gridRange.getHi();
    int[] lo = gridRange.getLo();

    if( hi.length != lo.length )
      throw new RemoteException( "Invalid Parameter" );

    int c1 = lo[0];
    int c2 = hi[0];
    int r1 = 0;
    int r2 = (int)( envImg.maxCP.ord[0] - envImg.minCP.ord[0] );
    if( hi.length > 1 )
    {
      r1 = lo[1];
      r2 = hi[1];
    }

    double minx = gt.getSourceX( c1 );
    double miny = gt.getSourceY( r1 );
    double maxx = gt.getSourceX( c2 );
    double maxy = gt.getSourceY( r2 );

    GM_Envelope env = GeometryFactory.createGM_Envelope( minx, miny, maxx, maxy );

    BufferedImage gc = null;
    try
    {
      gc = (BufferedImage)getRaster( env, c2 - c1, r2 - r1 );
    }
    catch( IOException io )
    {
      throw new RemoteException( io.toString() );
    }

    // extract byte data from the renderedOps and write them to a byte matrix
    byte[] data = null;
    DataBuffer db = gc.getData().getDataBuffer();
    if( db.getDataType() == DataBuffer.TYPE_BYTE )
    {
      DataBufferByte dbb = (DataBufferByte)db;
      data = dbb.getData();
    }

    return data;
  }

  public PT_Envelope getGridCorverageSize()
  {
    return null;
  }

  /**
   * Return a sequence of boolean values for a block. A value for each sample
   * dimension will be returned.
   * 
   * @param gridRange
   *          Grid range for block of data to be accessed.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public boolean[] getDataBlockAsBoolean( GC_GridRange gridRange ) throws RemoteException
  {
    return null;
  }

  /**
   * returns a part of the GridCoverage defined by the submitted bounding box
   * (coordinates at the gc's CRS) as <tt>BufferedImage</tt>
   * 
   * @param env
   *          bounding box of the area to extracted
   */
  public Object getRaster( GM_Envelope env ) throws IOException
  {
    return getRaster( env, 100, 100 );
  }

  /**
   * returns a part of the GridCoverage defined by the submitted bounding box
   * (coordinates at the gc's CRS) as <tt>BufferedImage</tt>
   * 
   * @param env
   *          bounding box of the area to extracted
   * @param width
   *          width of the returned raster
   * @param height
   *          height of the returned raster
   * @param imageType
   *          (color model) type of the image will only be used if the result is
   *          a <tt>BufferedImage</tt>
   * @see java.awt.image.BufferedImage
   */
  public Object getRaster( GM_Envelope env, int width, int height, int imageType )
      throws IOException, CoverageCreationException
  {

    Debug.debugMethodBegin( this, "getRaster(GM_Envelope , int , int , int )" );

    BufferedImage img = new BufferedImage( width, height, imageType );

    if( (long)( env.getMin().getX() * 10000 ) == (long)( envelope.minCP.ord[0] * 10000 )
        && (long)( env.getMin().getY() * 10000 ) == (long)( envelope.minCP.ord[1] * 10000 )
        && (long)( env.getMax().getX() * 10000 ) == (long)( envelope.maxCP.ord[0] * 10000 )
        && (long)( env.getMax().getY() * 10000 ) == (long)( envelope.maxCP.ord[1] * 10000 ) )
    {
      if( width == image.getWidth() && height == image.getHeight() )
      {
        img = image;
      }
      else
      {
        Graphics g = img.getGraphics();
        g.drawImage( image, 0, 0, width, height, null );
        g.dispose();
      }

    }
    else
    {
      int x1 = (int)Math.round( gt.getDestX( env.getMin().getX() ) );
      int y1 = (int)Math.round( gt.getDestY( env.getMax().getY() ) );
      int x2 = (int)Math.round( gt.getDestX( env.getMax().getX() ) );
      int y2 = (int)Math.round( gt.getDestY( env.getMin().getY() ) );

      BufferedImage img_ = image.getSubimage( x1, y1, Math.abs( x2 - x1 ), Math.abs( y2 - y1 ) );

      Graphics g = img.getGraphics();
      g.drawImage( img_, 0, 0, width, height, null );
      g.dispose();
    }

    Debug.debugMethodEnd();
    return img;
  }

  /**
   * returns a part of the GridCoverage defined by the submitted bounding box
   * (coordinates at the gc's CRS)
   * 
   * @param env
   *          bounding box of the area to extracted
   * @param width
   *          width of the returned raster
   * @param height
   *          height of the returned raster
   */
  public Object getRaster( GM_Envelope env, int width, int height ) throws IOException
  {
    Object o = null;
    try
    {
      o = getRaster( env, width, height, BufferedImage.TYPE_INT_ARGB );
    }
    catch( Exception e )
    {
      throw new IOException( e.toString() );
    }
    return o;
  }

}