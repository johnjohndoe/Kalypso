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
package org.deegree_impl.model.cv;

import java.io.Serializable;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

import org.deegree.model.coverage.CVDescriptor;
import org.deegree.model.coverage.CoverageCreationException;
import org.deegree.model.coverage.CoverageLayer;
import org.deegree.model.coverage.Directory;
import org.deegree.model.coverage.Level;
import org.deegree.model.coverage.Tile;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree_impl.model.cs.Adapters;
import org.deegree_impl.model.cs.ConvenienceCSFactory;
import org.deegree_impl.model.cs.CoordinateSystem;
import org.opengis.cs.CS_CoordinateSystem;
import org.opengis.cv.CV_Coverage;
import org.opengis.cv.CV_SampleDimension;
import org.opengis.gc.GC_GridCoverage;
import org.opengis.pt.PT_CoordinatePoint;
import org.opengis.pt.PT_Envelope;

/**
 * Provides access to an OpenGIS coverage. The essential property of coverage is
 * to be able to generate a value for any point within its domain. How coverage
 * is represented internally is not a concern.
 * 
 * For example consider the following different internal representations of
 * coverage: <br>
 * <OL>
 * <li>A coverage may be represented by a set of polygons which exhaustively
 * tile a plane (that is each point on the plane falls in precisely one
 * polygon). The value returned by the coverage for a point is the value of an
 * attribute of the polygon that contains the point.</li>
 * <li>A coverage may be represented by a grid of values. The value returned by
 * the coverage for a point is that of the grid value whose location is nearest
 * the point.</li>
 * <li>Coverage may be represented by a mathematical function. The value
 * returned by the coverage for a point is just the return value of the function
 * when supplied the coordinates of the point as arguments.</li>
 * <li>Coverage may be represented by combination of these. For example,
 * coverage may be represented by a combination of mathematical functions valid
 * over a set of polynomials.</LI>
 * </OL>
 * 
 * A coverage has a corresponding {@link CV_SampleDimension}for each sample
 * dimension in the coverage.
 * 
 * @version 1.00
 * @since 1.00
 */
public class CV_Coverage_Impl implements CV_Coverage, Serializable
{

  private CS_CoordinateSystem crs = null;

  protected CVDescriptor descriptor = null;

  protected String[] dimensionNames = null;

  protected PT_Envelope envelope = null;

  protected HashMap metadata = null;

  protected int numSources = -1;

  protected CV_SampleDimension[] sampleDimensions = null;

  /**
   * initialzies a simple <tt>CV_Coverage</tt>
   * 
   * @param descriptor
   *          containing a description of the coverage
   */
  public CV_Coverage_Impl( CVDescriptor descriptor ) throws Exception
  {
    if( descriptor != null )
    {
      this.descriptor = descriptor;
      CoverageLayer cvl = descriptor.getCoverageLayer();
      String crsName = cvl.getCRS()[0];
      CoordinateSystem crs_ = ConvenienceCSFactory.getInstance().getCSByName( crsName );
      crs = Adapters.getDefault().export( crs_ );
    }
    else
    {
      throw new Exception( "layer not found" );
    }
  }

  /**
   * initialzies a simple, enpty <tt>CV_Coverage</tt> just with a CRS
   * 
   * @param crs
   *          coordinate reference system of the coverage
   */
  public CV_Coverage_Impl( CS_CoordinateSystem crs )
  {
    this.crs = crs;
  }

  /**
   * Return the value vector for a given point in the coverage. A value for each
   * sample dimension is included in the vector. The default interpolation type
   * used when accessing grid values for points which fall between grid cells is
   * nearest neighbor. The coordinate system of the point is the same as the
   * grid coverage coordinate system (specified by the
   * {@link #getCoordinateSystem}).
   * 
   * @param point
   *          Point at which to find the grid values.
   * @return the value vector for a given point in the coverage.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public Object evaluate( PT_CoordinatePoint point ) throws RemoteException
  {
    return null;
  }

  /**
   * Return a sequence of Boolean values for a given point in the coverage. A
   * value for each sample dimension is included in the sequence. The default
   * interpolation type used when accessing grid values for points which fall
   * between grid cells is nearest neighbor. The coordinate system of the point
   * is the same as the grid coverage coordinate system.
   * 
   * @param point
   *          Point at which to find the coverage values.
   * @return a sequence of Boolean values for a given point in the coverage.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public boolean[] evaluateAsBoolean( PT_CoordinatePoint point ) throws RemoteException
  {
    return null;
  }

  /**
   * Return a sequence of unsigned byte values for a given point in the
   * coverage. A value for each sample dimension is included in the sequence.
   * The default interpolation type used when accessing grid values for points
   * which fall between grid cells is nearest neighbor. The coordinate system of
   * the point is the same as the grid coverage coordinate system.
   * 
   * @param point
   *          Point at which to find the coverage values.
   * @return a sequence of unsigned byte values for a given point in the
   *         coverage.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public byte[] evaluateAsByte( PT_CoordinatePoint point ) throws RemoteException
  {
    return null;
  }

  /**
   * Return a sequence of double values for a given point in the coverage. A
   * value for each sample dimension is included in the sequence. The default
   * interpolation type used when accessing grid values for points which fall
   * between grid cells is nearest neighbor. The coordinate system of the point
   * is the same as the grid coverage coordinate system.
   * 
   * @param point
   *          Point at which to find the grid values.
   * @return a sequence of double values for a given point in the coverage.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public double[] evaluateAsDouble( PT_CoordinatePoint point ) throws RemoteException
  {
    return null;
  }

  /**
   * Return a sequence of integer values for a given point in the coverage. A
   * value for each sample dimension is included in the sequence. The default
   * interpolation type used when accessing grid values for points which fall
   * between grid cells is nearest neighbor. The coordinate system of the point
   * is the same as the grid coverage coordinate system.
   * 
   * @param point
   *          Point at which to find the grid values.
   * @return a sequence of integer values for a given point in the coverage.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public int[] evaluateAsInteger( PT_CoordinatePoint point ) throws RemoteException
  {
    return null;
  }

  /**
   * This specifies the coordinate system used when accessing a coverage or grid
   * coverage with the <code>evaluate</code> methods. It is also the
   * coordinate system of the coordinates used with the math transform (see
   * {@linkorg.opengis.gc.GC_GridGeometry#getGridToCoordinateSystem
   * gridToCoordinateSystem}).
   * 
   * This coordinate system is usually different than the grid coordinate system
   * of the grid. grid coverage can be accessed (re-projected) with new
   * coordinate system with the {@link org.opengis.gp.GP_GridCoverageProcessor}
   * component. In this case, a new instance of a grid coverage is created. <br>
   * <br>
   * Note: If a coverage does not have an associated coordinate system, the
   * returned value will be <code>null</code>. The
   * {@link org.opengis.gc.GC_GridGeometry#getGridToCoordinateSystem
   * gridToCoordinateSystem}) attribute should also be <code>null</code> if
   * the coordinate system is <code>null</code>.
   * 
   * @return the coordinate system used when accessing a coverage or grid
   *         coverage with the <code>evaluate</code> methods.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public CS_CoordinateSystem getCoordinateSystem() throws RemoteException
  {
    return crs;
  }

  /**
   * The names of each dimension in the coverage. Typically these names are
   * <var>x </var>, <var>y </var>, <var>z </var> and <var>t </var>. The number
   * of items in the sequence is the number of dimensions in the coverage. Grid
   * coverages are typically 2D ( <var>x </var>, <var>y </var>) while other
   * coverages may be 3D ( <var>x </var>, <var>y </var>, <var>z </var>) or 4D (
   * <var>x </var>, <var>y </var>, <var>z </var>, <var>t </var>). The number of
   * dimensions of the coverage is the number of entries in the list of
   * dimension names.
   * 
   * @return the names of each dimension in the coverage.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public String[] getDimensionNames() throws RemoteException
  {
    return dimensionNames;
  }

  /**
   * The bounding box for the coverage domain in coordinate system coordinates.
   * For grid coverages, the grid cells are centered on each grid coordinate.
   * The envelope for a 2-D grid coverage includes the following corner
   * positions.
   * 
   * <blockquote>
   * 
   * <pre>
   * (Minimum row - 0.5, Minimum column - 0.5) for the minimum coordinates
   * (Maximum row - 0.5, Maximum column - 0.5) for the maximum coordinates
   * </pre>
   * 
   * </blockquote>
   * 
   * If a grid coverage does not have any associated coordinate system, the
   * minimum and maximum coordinate points for the envelope will be empty
   * sequences.
   * 
   * @return the bounding box for the coverage domain in coordinate system
   *         coordinates.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public PT_Envelope getEnvelope() throws RemoteException
  {
    return envelope;
  }

  /**
   * List of metadata keywords for a coverage. If no metadata is available, the
   * sequence will be empty.
   * 
   * @return the list of metadata keywords for a coverage.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public String[] getMetadataNames() throws RemoteException
  {
    Iterator iterator = metadata.keySet().iterator();
    String[] names = new String[metadata.size()];
    int i = 0;
    while( iterator.hasNext() )
    {
      names[i++] = (String)iterator.next();
    }
    return names;
  }

  /**
   * Retrieve the metadata value for a given metadata name.
   * 
   * @param name
   *          Metadata keyword for which to retrieve data.
   * @return the metadata value for a given metadata name.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public String getMetadataValue( String name ) throws RemoteException
  {
    return (String)metadata.get( name );
  }

  /**
   * The number of sample dimensions in the coverage. For grid coverages, a
   * sample dimension is a band.
   * 
   * @return the number of sample dimensions in the coverage.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public int getNumSampleDimensions() throws RemoteException
  {
    return sampleDimensions.length;
  }

  /**
   * Number of grid coverages which the grid coverage was derived from. This
   * implementation specification does not include interfaces for creating
   * collections of coverages therefore this value will usually be one
   * indicating an adapted grid coverage, or zero indicating a raw grid
   * coverage.
   * 
   * @return the number of grid coverages which the grid coverage was derived
   *         from.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public int getNumSources() throws RemoteException
  {
    return numSources;
  }

  /**
   * Retrieve sample dimension information for the coverage. For a grid coverage
   * a sample dimension is a band. The sample dimension information include such
   * things as description, data type of the value (bit, byte, integer...), the
   * no data values, minimum and maximum values and a color table if one is
   * associated with the dimension. A coverage must have at least one sample
   * dimension.
   * 
   * @param index
   *          Index for sample dimension to retrieve. Indices are numbered 0 to
   *          (n-1).
   * @return Sample dimension information for the coverage.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public CV_SampleDimension getSampleDimension( int index ) throws RemoteException
  {
    return sampleDimensions[index];
  }

  /**
   * sets the sample dimensions for the coverage
   * 
   * @param sampleDimensions
   *          Sample dimensions for the coverage
   */
  public void setSampleDimension( CV_SampleDimension[] sampleDimensions )
  {
    this.sampleDimensions = sampleDimensions;
  }

  /**
   * Returns the source data for a grid coverage. If the {@link GC_GridCoverage}
   * was produced from an underlying dataset (by <code>createFromName</code>
   * or <code>createFromSubName</code> for instance) the
   * {@link #getNumSources}method should returns zero, and this method should
   * not be called.
   * 
   * If the <code>GC_GridCoverage}</code> was produced using {link
   * org.opengis.gp.GP_GridCoverageProcessor} then it should return the source
   * grid coverage of the one used as input to
   * <code>GP_GridCoverageProcessor</code>. In general the source() method is
   * intended to return the original <code>GC_GridCoverage</code> on which it
   * depends.
   * 
   * This is intended to allow applications to establish what
   * <code>GC_GridCoverage</code> s will be affected when others are updated,
   * as well as to trace back to the "raw data".
   * 
   * @param sourceDataIndex
   *          Source grid coverage index. Indexes start at 0.
   * @return the source data for a grid coverage.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public GC_GridCoverage getSource( int sourceDataIndex ) throws RemoteException
  {
    return null;
  }

  /**
   * return all <tt>Tile<tt>s that matches the bounding box and the scale.
   */
  protected static Tile[] getTiles( Level level, GM_Envelope boundingBox, double scale )
      throws CoverageCreationException
  {
    ArrayList list = new ArrayList( 1000 );

    if( level.getDirectories() != null && level.getDirectories().length > 0 )
    {
      list = getTilesByDir( boundingBox, scale, level, list );
    }
    else
    {
      list = getTiles( boundingBox, scale, level, list );
    }

    return (Tile[])list.toArray( new Tile[list.size()] );
  }

  /**
   * @param boundingBox
   * @param scale
   * @param level
   * @param list
   * @return @throws
   *         CoverageCreationException
   */
  private static ArrayList getTilesByDir( GM_Envelope boundingBox, double scale, Level level,
      ArrayList list ) throws CoverageCreationException
  {

    if( scale >= level.getMinScale() && scale < level.getMaxScale() )
    {
      Directory dir[] = level.getDirectories();
      for( int i = 0; i < dir.length; i++ )
      {
        if( dir[i].getBoundingBox().intersects( boundingBox ) )
        {
          Tile[] tiles = dir[i].getTiles( boundingBox );
          for( int k = 0; k < tiles.length; k++ )
          {
            list.add( tiles[k] );
          }
        }
      }

    }
    else
    {
      Directory[] dir = level.getDirectories();
      for( int i = 0; i < dir.length; i++ )
      {
        Level lev = dir[i].getLevel();
        if( lev != null )
        {
          list = getTilesByDir( boundingBox, scale, lev, list );
        }
      }
    }
    return list;
  }

  /**
   * recursive method for selecting all Tiles that matches the submitted
   * conditions.
   * 
   * @param boundingBox
   * @param scale
   * @param level
   * @param list
   * @return
   */
  private static ArrayList getTiles( GM_Envelope boundingBox, double scale, Level level,
      ArrayList list )
  {

    if( scale >= level.getMinScale() && scale < level.getMaxScale() )
    {
      Tile[] tiles = level.getTiles();
      for( int i = 0; i < tiles.length; i++ )
      {
        if( tiles[i].getBoundingBox().intersects( boundingBox ) )
        {
          ArrayList l = new ArrayList( 100 );
          l = getTiles( boundingBox, tiles[i], l );
          list.addAll( l );
        }
      }
    }
    else
    {
      Tile[] tiles = level.getTiles();
      for( int i = 0; i < tiles.length; i++ )
      {
        Level lev = tiles[i].getLevel();
        if( lev != null )
        {
          list = getTiles( boundingBox, scale, lev, list );
        }
      }
    }
    return list;
  }

  /**
   * recursive method for selecting all Tiles that matches the submitted
   * conditions.
   */
  private static ArrayList getTiles( GM_Envelope boundingBox, Tile tile, ArrayList list )
  {
    Tile[] tiles = tile.getTiles();
    if( tiles != null )
    {
      for( int i = 0; i < tiles.length; i++ )
      {
        if( tiles[i].getBoundingBox().intersects( boundingBox ) )
        {
          list = getTiles( boundingBox, tiles[i], list );
        }
      }
    }
    else
    {
      list.add( tile );
    }
    return list;
  }

}

