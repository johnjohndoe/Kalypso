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
package org.deegree_impl.model.ct;

import org.deegree_impl.model.cs.ConvenienceCSFactory;
import org.deegree_impl.model.cs.CoordinateSystem;
import org.deegree_impl.model.cs.GeographicCoordinateSystem;
import org.deegree_impl.model.cs.ProjectedCoordinateSystem;
import org.deegree_impl.tools.Debug;

/**
 * factory for
 * 
 * <p>
 * ------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public class ConvenienceTransformFactory
{
  private static ConvenienceTransformFactory tfFactory = null;

  private ConvenienceCSFactory csFactory = null;

  private CoordinateTransformationFactory trFactory = null;

  /**
   * Creates a new ConvenienceTransformFactory object.
   */
  private ConvenienceTransformFactory()
  {
    trFactory = CoordinateTransformationFactory.getDefault();
    csFactory = ConvenienceCSFactory.getInstance();
  }

  /**
   * 
   * 
   * @return
   */
  public static ConvenienceTransformFactory getInstance()
  {
    if( tfFactory == null )
    {
      tfFactory = new ConvenienceTransformFactory();
    }

    return tfFactory;
  }

  /**
   * returns a <tt>Transform</tt> object that's able to perform a coordinate
   * tranformation from the source- to the destination-coordinate system. At the
   * moment only transformations between geographic coordinate systems
   * geographic and projected coordinated systems and vice versa are supported.
   * 
   * @param src
   *          source coordinated system
   * @param dest
   *          destination (target) coodinate system
   * @return an instance of <tt>net.ct.Transform</tt>
   * @exception Exception
   *              will be thrown if the desired transformaion isn't possible
   */
  public MathTransform getTransform( String src, String dest ) throws Exception
  {
    Debug.debugMethodBegin( this, "getTransform(String,String)" );

    CoordinateSystem src_cs = csFactory.getCSByName( src );
    CoordinateSystem dest_cs = csFactory.getCSByName( dest );
    MathTransform tr = getTransform( src_cs, dest_cs );

    Debug.debugMethodEnd();

    return tr;
  }

  /**
   * returns a <tt>Transform</tt> object that's able to perform a coordinate
   * tranformation from the source- to the destination-coordinate system. At the
   * moment only transformations between geographic coordinate systems
   * geographic and projected coordinated systems and vice versa are supported.
   * 
   * @param src
   *          source coordinated system
   * @param dest
   *          destination (target) coodinate system
   * @return an instance of <tt>net.ct.Transform</tt>
   * @exception Exception
   *              will be thrown if the desired transformaion isn't possible
   */
  public synchronized MathTransform getTransform( CoordinateSystem src, CoordinateSystem dest )
      throws Exception
  {
    Debug.debugMethodBegin( this, "getTransform(CoordinateSystem, CoordinateSystem)" );

    MathTransform finalTransform = null;

    if( ( src instanceof GeographicCoordinateSystem && dest instanceof ProjectedCoordinateSystem ) )
    {
      finalTransform = getGeogrToProjected( src, dest );
    }
    else if( src instanceof GeographicCoordinateSystem
        && dest instanceof GeographicCoordinateSystem )
    {
      CoordinateTransformation transformation = trFactory.createFromCoordinateSystems( src, dest );
      finalTransform = transformation.getMathTransform();
    }
    else if( src instanceof ProjectedCoordinateSystem && dest instanceof ProjectedCoordinateSystem )
    {
      finalTransform = getProjectedToProjected( src, dest );
    }
    else if( src instanceof ProjectedCoordinateSystem && dest instanceof GeographicCoordinateSystem )
    {
      finalTransform = getProjectedToGeogr( src, dest );
    }
    else
    {
      throw new Exception( "Not supported transformation!" );
    }

    Debug.debugMethodEnd();
    return finalTransform;
  }

  /**
   * retuns a <tt>MathTransform</tt> object for the transformation of
   * coodinates from a geographic to a projected coodinate systems
   */
  private MathTransform getGeogrToProjected( CoordinateSystem src, CoordinateSystem dest )
      throws Exception
  {
    Debug.debugMethodBegin( this, "getGeogrToProjected" );

    // get transfrom to EPSG:4326
    CoordinateSystem tmp = csFactory.getCSByName( "EPSG:4326" );
    CoordinateTransformation transformation = trFactory.createFromCoordinateSystems( src, tmp );
    MathTransform transform1 = transformation.getMathTransform();

    // get transfrom EPSG:4326 to projected coordinate system
    // (only tested for TransverseMecator)
    MathTransformFactory mtf = MathTransformFactory.getDefault();
    MathTransform transform2 = mtf.createParameterizedTransform( ( (ProjectedCoordinateSystem)dest )
        .getProjection() );
    MathTransform finalTransform = mtf.createConcatenatedTransform( transform1, transform2 );

    Debug.debugMethodEnd();

    return finalTransform;
  }

  /**
   * retuns a <tt>MathTransform</tt> object for the transformation of
   * coodinates between two projected coodinate systems
   */
  private MathTransform getProjectedToProjected( CoordinateSystem src, CoordinateSystem dest )
      throws Exception
  {
    Debug.debugMethodBegin( this, "getProjectedToProjected" );

    // get transfrom of src to EPSG:4326
    CoordinateSystem tmp = csFactory.getCSByName( "EPSG:4326" );
    CoordinateTransformation transformation = trFactory.createFromCoordinateSystems( src, tmp );
    MathTransform transform1 = transformation.getMathTransform();

    // get transfrom EPSG:4326 to projected coordinate system
    // (only tested for TransverseMecator)
    CoordinateTransformation tran = trFactory.createFromCoordinateSystems( tmp, dest );
    MathTransform transform2 = tran.getMathTransform();

    MathTransformFactory mtf = MathTransformFactory.getDefault();
    MathTransform finalTransform = mtf.createConcatenatedTransform( transform1, transform2 );

    Debug.debugMethodEnd();

    return finalTransform;
  }

  /**
   * retuns a <tt>MathTransform</tt> object for the transformation of
   * coodinates from a projected to a geographic coodinate systems
   */
  private MathTransform getProjectedToGeogr( CoordinateSystem src, CoordinateSystem dest )
      throws Exception
  {
    Debug.debugMethodBegin( this, "getProjectedToGeogr" );

    // get transfrom of dest to EPSG:4326. inverting this
    // results in a transformation of a projected system to
    // EPSG:4326
    CoordinateSystem tmp = csFactory.getCSByName( "EPSG:4326" );
    CoordinateTransformation transformation = trFactory.createFromCoordinateSystems( dest, tmp );
    MathTransform transform1 = transformation.getMathTransform();

    // get transfrom EPSG:4326 to projected system
    MathTransformFactory mtf = MathTransformFactory.getDefault();
    MathTransform transform2 = mtf.createParameterizedTransform( ( (ProjectedCoordinateSystem)src )
        .getProjection() );
    MathTransform finalTransform = mtf.createConcatenatedTransform( transform1, transform2 );
    finalTransform = finalTransform.inverse();

    Debug.debugMethodEnd();

    return finalTransform;
  }
}