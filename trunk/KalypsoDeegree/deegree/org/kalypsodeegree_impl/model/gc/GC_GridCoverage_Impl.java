// $Header:
// /var/lib/cvs/backupdeegree/deegree/org/deegree_impl/model/gc/GC_GridCoverage_Impl.java,v
// 1.1.1.1 2004/05/11 16:43:24 doemming Exp $
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
package org.deegree_impl.model.gc;

import java.awt.Graphics;
import java.awt.color.ColorSpace;
import java.awt.image.BufferedImage;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferByte;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.net.URL;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.media.jai.JAI;
import javax.media.jai.RenderedOp;

import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.coverage.CVDescriptor;
import org.deegree.model.coverage.CoverageCreationException;
import org.deegree.model.coverage.GridCoverageLayer;
import org.deegree.model.coverage.GridExtentDescription;
import org.deegree.model.coverage.Level;
import org.deegree.model.coverage.Tile;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.services.RangeParamList;
import org.deegree.tools.Cache;
import org.deegree_impl.graphics.transformation.WorldToScreenTransform;
import org.deegree_impl.io.ecwapi.ECWReader;
import org.deegree_impl.io.imgapi.IMGReader;
import org.deegree_impl.model.cv.CVRange;
import org.deegree_impl.model.cv.CVRangeFactory;
import org.deegree_impl.model.cv.CV_Coverage_Impl;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.services.RangeParam;
import org.deegree_impl.tools.Cache_Impl;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.StringExtend;
import org.opengis.cv.CV_PaletteInterpretation;
import org.opengis.cv.CV_SampleDimensionType;
import org.opengis.gc.GC_GridCoverage;
import org.opengis.gc.GC_GridGeometry;
import org.opengis.gc.GC_GridPacking;
import org.opengis.gc.GC_GridRange;
import org.opengis.pt.PT_CoordinatePoint;
import org.opengis.pt.PT_Envelope;

import com.sun.media.jai.codec.FileSeekableStream;
import com.sun.media.jai.codec.SeekableStream;

/**
 * Represent the basic implementation which provides access to grid coverage
 * data. A <code>GC_GridCoverage</code> implementation may provide the ability
 * to update grid values.
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 2.11.2002
 */
public class GC_GridCoverage_Impl extends CV_Coverage_Impl implements GC_GridCoverage
{
  private static Cache cache = null;

  private GC_GridGeometry gridGeometry = null;

  private GC_GridPacking gridPacking = null;

  private GeoTransform gt = null;

  private PT_Envelope envImg = null;

  private GC_GridGeometry[] overviewGridGeometry = null;

  private GC_GridCoverage[] overviews = null;

  private boolean isEditable = false;

  /**
   * initialzies a simple <tt>CV_Coverage</tt> with an image, coordinate
   * reference system, an envelope describing the spatial extension of of the
   * coverage and the coverages metadata.
   * 
   * @param descriptor
   *          descrition of the GC
   * @param isEditable
   *          indicates if the grid coverages data can be edited
   */
  public GC_GridCoverage_Impl( CVDescriptor descriptor, boolean isEditable ) throws RemoteException
  {
    super( descriptor );

    if( cache == null )
    {
      cache = new Cache_Impl( 10000 );
    }

    this.isEditable = isEditable;

    GridCoverageLayer gcl = (GridCoverageLayer)descriptor.getCoverageLayer();
    GridExtentDescription ged = (GridExtentDescription)gcl.getDomainSetExtentDescription();

    double minx = ged.getSpatialExtent().getXExtent().getMin();
    double maxx = ged.getSpatialExtent().getXExtent().getMax();

    double miny = ged.getSpatialExtent().getYExtent().getMin();
    double maxy = ged.getSpatialExtent().getYExtent().getMax();

    // TODO
    //        double minz = ged.getSpatialExtent().getZExtent().getMin();
    //        double maxz = ged.getSpatialExtent().getZExtent().getMax();
    envelope = new PT_Envelope();
    envelope.minCP = new PT_CoordinatePoint( minx, miny );
    envelope.maxCP = new PT_CoordinatePoint( maxx, maxy );

    double[] lo = ged.getGrid().getGridRange().getLow();
    double[] hi = ged.getGrid().getGridRange().getHigh();

    envImg = new PT_Envelope();
    envImg.minCP = new PT_CoordinatePoint( lo[0], lo[1] );
    envImg.maxCP = new PT_CoordinatePoint( hi[0], hi[1] );

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

  /**
   * 
   * 
   * @return
   */
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
    {
      throw new RemoteException( "Invalid Parameter" );
    }

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

  /**
   * 
   * 
   * @return
   */
  public PT_Envelope getGridCoverageSize()
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
  public Object getRaster( GM_Envelope env, int width, int height, int imageType,
      RangeParamList rangeParams ) throws IOException, CoverageCreationException
  {
    Debug.debugMethodBegin( this, "getRaster(GM_Envelope , int , int , int )" );

    ArrayList collector = new ArrayList();
    Level level = filterRange( rangeParams, collector );

    Tile[] tiles = getTiles( level, env, getScale( env, width, height ) );

    Object o = new float[0][0];

    if( ( tiles != null ) && ( tiles.length > 0 ) )
    {
      String s = tiles[0].getResourceURL().getFile().toLowerCase();

      if( s.endsWith( ".img" ) )
      {
        GridExtentDescription ged = (GridExtentDescription)descriptor.getCoverageLayer()
            .getDomainSetExtentDescription();
        double xmin = ged.getSpatialExtent().getXExtent().getMin();
        double xmax = ged.getSpatialExtent().getXExtent().getMax();
        double ymin = ged.getSpatialExtent().getYExtent().getMin();
        double ymax = ged.getSpatialExtent().getYExtent().getMax();
        int inWidth = (int)ged.getGrid().getGridRange().getHigh()[0] + 1;
        int inHeight = (int)ged.getGrid().getGridRange().getHigh()[1] + 1;
        GM_Envelope bbox = GeometryFactory.createGM_Envelope( xmin, ymin, xmax, ymax );
        int hh = inHeight;
        int ww = inWidth;

        if( tiles[0].getLevel() != null )
        {
          double d = tiles[0].getLevel().getMaxScale();
          hh = 1 + (int)( hh / d );
          ww = 1 + (int)( ww / d );
        }

        String tileUrl = tiles[0].getResourceURL().getFile();
        String detokUrl = CVRangeFactory.substToken( rangeParams, collector, tileUrl );

        //				System.out.println("DETOKEN:: "+tileUrl);
        //				System.out.println("----->>:: "+detokUrl);
        IMGReader ir = new IMGReader( detokUrl, ww, hh, bbox, IMGReader.FLOAT );
        o = ir.read( env );

        // just for speed up; maybe should be removed in the future
        ImgResize imgs = new ImgResize( (float[][])o, width / 2, height / 2 );
        o = imgs.rectStretch();
      }
      else
      {
        o = getBufferedImage( env, width, height, imageType, tiles, rangeParams, collector );
      }
    }

    Debug.debugMethodEnd();
    return o;
  }

  /**
   * Return the Level matching the ranges given in the request. If no CVRange
   * matches, the upper level Level is returned -- FIXME check in default values
   * instead
   * 
   * TODO only first-level Ranges are explored. We must recurse into deeper
   * levels.
   * 
   * @param rangeParams
   *          a Map of RangeParam; key is the name
   * @param rangeCollector
   *          will be filled with the selected ranges
   * 
   * @return the Level matching the given params
   * 
   * @author ETj
   */
  private Level filterRange( RangeParamList rangeParams, List rangeCollector )
  {
    if( rangeParams == null )
    {
      //System.out.println(" *** Warning: reqranges is null");
      return descriptor.getLevel();
    }

    for( Iterator iter = descriptor.getRanges().iterator(); iter.hasNext(); )
    {
      CVRange erange = (CVRange)iter.next();
      RangeParam rp = (RangeParam)rangeParams.getParameter( erange.getName() );

      if( rp == null ) // there are no params selecting this range

      {
        continue;
      }

      if( !erange.match( rp ) ) // given param doesn't match this range

      {
        continue;
      }

      // TODO: search for subranges here
      //...
      // This range is selected
      rangeCollector.add( erange );

      return erange.getLevel();
    }

    System.out.println( " *** FilterRange:: default Level returned" );
    return descriptor.getLevel();
  }

  /**
   * reads a grid coverage from image sources and returns it as <tT>
   * BufferedImage</TT>
   */
  private BufferedImage getBufferedImage( GM_Envelope env, int width, int height, int imageType,
      Tile[] tiles, RangeParamList rangeParams, List usedRanges ) throws IOException
  {
    BufferedImage bi = new BufferedImage( width, height, imageType );

    GeoTransform gtd = new WorldToScreenTransform( env.getMin().getX(), env.getMin().getY(), env
        .getMax().getX(), env.getMax().getY(), 0, 0, width, height );

    Graphics g = bi.getGraphics();

    for( int i = 0; i < tiles.length; i++ )
    {
      if( ( i % 1000 ) == 0 )
      {
        System.gc();
      }

      // get URL of the current Tile
      URL url0 = tiles[i].getResourceURL();
      String surl = url0.toExternalForm();

      if( url0.getProtocol().equals( "file" ) )
      {
        surl = StringExtend.replace( surl, "file://", "file:///", false ); // FIXME
      }

      String detokUrl = CVRangeFactory.substToken( rangeParams, usedRanges, surl );
      URL url = new URL( detokUrl );

      // get the tile's bounding box
      GM_Envelope tenv = tiles[i].getBoundingBox();

      // create intersection envelope of the Tile's bounding box and the
      // the bounding box of the target map
      GM_Envelope senv = tenv.createIntersection( env );

      if( senv != null )
      {
        BufferedImage img = null;
        // get target rectangle
        int dx1 = (int)Math.round( gtd.getDestX( senv.getMin().getX() ) );
        int dy1 = (int)Math.round( gtd.getDestY( senv.getMax().getY() ) );
        int dx2 = (int)Math.round( gtd.getDestX( senv.getMax().getX() ) );
        int dy2 = (int)Math.round( gtd.getDestY( senv.getMin().getY() ) );

        if( url.getFile().toLowerCase().endsWith( ".ecw" ) )
        {
          try
          {
            // load current file from the cache
            ECWReader ecwFile = (ECWReader)cache.get( url );

            if( ecwFile == null )
            {
              String tmp = url.getFile();

              if( System.getProperty( "os.name" ).toUpperCase().startsWith( "WINDOWS" ) )
              {
                //dispose leading "/" from file-string for windows OS
                tmp = tmp.substring( 1 );
              }

              ecwFile = new ECWReader( tmp );
              cache.push( url, ecwFile );
            }

            //get image using intersection of dest-env & tile-env and width and
            // height of dest-rect
            img = ecwFile.getBufferedImage( senv, dx2 - dx1, dy2 - dy1 );
          }
          catch( Exception e )
          {
            e.printStackTrace();
            throw new IOException( e.toString() );
          }
        }
        else
        {
          // load current Tile from the cache
          img = (BufferedImage)cache.get( url );

          // if the image isn't contained within the cache load it
          if( img == null )
          {
            img = getBufferedImage( url );
            cache.push( url, img );
          }

          // create coordinate transform object for the Tile
          GeoTransform gts = new WorldToScreenTransform( tenv.getMin().getX(),
              tenv.getMin().getY(), tenv.getMax().getX(), tenv.getMax().getY(), 0, 0, img
                  .getWidth(), img.getHeight() );

          // get source rectangle (required rect of the tile)
          int sx1 = (int)Math.round( gts.getDestX( senv.getMin().getX() ) );
          int sy1 = (int)Math.round( gts.getDestY( senv.getMax().getY() ) );
          int sx2 = (int)Math.round( gts.getDestX( senv.getMax().getX() ) );
          int sy2 = (int)Math.round( gts.getDestY( senv.getMin().getY() ) );

          if( ( ( sx2 - sx1 ) > 0 ) && ( ( sy2 - sy1 ) > 0 ) && ( ( dx2 - dx1 ) > 0 )
              && ( ( dy2 - dy1 ) > 0 ) )
          {
            // extract required rect of the Tile
            img = img.getSubimage( sx1, sy1, sx2 - sx1, sy2 - sy1 );
            // draw tile (or a part of it) on the target map
          }
          else
          {
            img = null;
          }
        }

        if( img != null )
        {
          g.drawImage( img, dx1, dy1, dx2 - dx1, dy2 - dy1, null );
        }
      }
    }

    g.dispose();

    return bi;
  }

  /**
   * reads a <tt>BufferedImage</tt> from a <tt>URL</tt>
   */
  private BufferedImage getBufferedImage( URL url ) throws IOException
  {
    //InputStream is = url.openStream();
    SeekableStream fss = new FileSeekableStream( new RandomAccessFile( url.getFile(), "r" ) );
    RenderedOp ro = JAI.create( "stream", fss );
    BufferedImage img = ro.getAsBufferedImage();
    fss.close();
    //is.close();
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
      o = getRaster( env, width, height, BufferedImage.TYPE_INT_ARGB, null ); //ETJ
      // FIXME
    }
    catch( Exception e )
    {
      throw new IOException( e.toString() );
    }

    return o;
  }

  /**
   * returns the scale of the requested gc
   */
  public double getScale( GM_Envelope env, int width, int height )
  {
    double minx = env.getMin().getX();
    double miny = env.getMin().getY();
    double maxx = env.getMax().getX();
    double maxy = env.getMax().getY();

    double sx = Math.sqrt( Math.pow( maxx - minx, 2 ) + Math.pow( maxy - miny, 2 ) );
    double px = Math.sqrt( ( width * width + width * width ) );

    //TODO
    // dependece to coordinate system
    return ( sx / px );
  }
}
/*******************************************************************************
 * Changes to this class. What the people have been up to: $Log:
 * GC_GridCoverage_Impl.java,v $ Revision 1.1.1.1 2004/05/11 16:43:24 doemming
 * backup of local modified deegree sources
 * 
 * Revision 1.31 2004/03/26 11:19:29 poth no message
 * 
 * 
 *  
 ******************************************************************************/