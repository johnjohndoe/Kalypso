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
package org.deegree_impl.model.ct;

// OpenGIS dependencies (SEAGIS)
import java.io.Serializable;

import org.deegree_impl.model.pt.CoordinatePoint;
import org.deegree_impl.model.pt.Matrix;
import org.deegree_impl.model.pt.MismatchedDimensionException;
import org.deegree_impl.model.resources.Utilities;

/**
 * Transform which passes through a subset of ordinates to another transform.
 * This allows transforms to operate on a subset of ordinates. For example, if
 * you have ( <var>latitude </var>, <var>longitude </var>, <var>height </var>)
 * coordinates, then you may wish to convert the height values from feet to
 * meters without affecting the latitude and longitude values.
 * 
 * @version 1.00
 * @author OpenGIS (www.opengis.org)
 * @author Martin Desruisseaux
 */
final class PassThroughTransform extends AbstractMathTransform implements Serializable
{
  /**
   * Serial number for interoperability with different versions.
   */
  private static final long serialVersionUID = -1673997634240223449L;

  /**
   * Index of the first affected ordinate.
   */
  protected final int firstAffectedOrdinate;

  /**
   * Number of unaffected ordinates after the affected ones. Always 0 when used
   * through the strict OpenGIS API.
   */
  protected final int numTrailingOrdinates;

  /**
   * The sub transform.
   */
  protected final MathTransform transform;

  /**
   * The inverse transform. This field will be computed only when needed.
   */
  private transient PassThroughTransform inverse;

  /**
   * Create a pass through transform.
   * 
   * @param firstAffectedOrdinate
   *          Index of the first affected ordinate.
   * @param transform
   *          The sub transform.
   * @param numTrailingOrdinates
   *          Number of trailing ordinates to pass through. Affected ordinates
   *          will range from <code>firstAffectedOrdinate</code> inclusive to
   *          <code>dimTarget-numTrailingOrdinates</code> exclusive.
   */
  public PassThroughTransform( final int firstAffectedOrdinate, final MathTransform transform,
      final int numTrailingOrdinates )
  {
    if( transform instanceof PassThroughTransform )
    {
      final PassThroughTransform passThrough = (PassThroughTransform)transform;
      this.firstAffectedOrdinate = passThrough.firstAffectedOrdinate + firstAffectedOrdinate;
      this.numTrailingOrdinates = passThrough.numTrailingOrdinates + numTrailingOrdinates;
      this.transform = passThrough.transform;
    }
    else
    {
      this.firstAffectedOrdinate = firstAffectedOrdinate;
      this.numTrailingOrdinates = numTrailingOrdinates;
      this.transform = transform;
    }
  }

  /**
   * Gets the dimension of input points.
   */
  public int getDimSource()
  {
    return firstAffectedOrdinate + transform.getDimSource() + numTrailingOrdinates;
  }

  /**
   * Gets the dimension of output points.
   */
  public int getDimTarget()
  {
    return firstAffectedOrdinate + transform.getDimTarget() + numTrailingOrdinates;
  }

  /**
   * Tests whether this transform does not move any points.
   */
  public boolean isIdentity()
  {
    return transform.isIdentity();
  }

  /**
   * Transforms a list of coordinate point ordinal values.
   */
  public void transform( final float[] srcPts, int srcOff, final float[] dstPts, int dstOff,
      int numPts ) throws TransformException
  {
    final int subDimSource = transform.getDimSource();
    final int subDimTarget = transform.getDimTarget();
    int srcStep = numTrailingOrdinates;
    int dstStep = numTrailingOrdinates;
    if( srcPts == dstPts && srcOff < dstOff )
    {
      final int dimSource = getDimSource();
      final int dimTarget = getDimTarget();
      srcOff += numPts * dimSource;
      dstOff += numPts * dimTarget;
      srcStep -= 2 * dimSource;
      dstStep -= 2 * dimTarget;
    }
    while( --numPts >= 0 )
    {
      System.arraycopy( srcPts, srcOff, dstPts, dstOff, firstAffectedOrdinate );
      transform.transform( srcPts, srcOff += firstAffectedOrdinate, dstPts,
          dstOff += firstAffectedOrdinate, 1 );
      System.arraycopy( srcPts, srcOff += subDimSource, dstPts, dstOff += subDimTarget,
          numTrailingOrdinates );
      srcOff += srcStep;
      dstOff += dstStep;
    }
  }

  /**
   * Transforms a list of coordinate point ordinal values.
   */
  public void transform( final double[] srcPts, int srcOff, final double[] dstPts, int dstOff,
      int numPts ) throws TransformException
  {
    final int subDimSource = transform.getDimSource();
    final int subDimTarget = transform.getDimTarget();
    int srcStep = numTrailingOrdinates;
    int dstStep = numTrailingOrdinates;
    if( srcPts == dstPts && srcOff < dstOff )
    {
      final int dimSource = getDimSource();
      final int dimTarget = getDimTarget();
      srcOff += numPts * dimSource;
      dstOff += numPts * dimTarget;
      srcStep -= 2 * dimSource;
      dstStep -= 2 * dimTarget;
    }
    while( --numPts >= 0 )
    {
      System.arraycopy( srcPts, srcOff, dstPts, dstOff, firstAffectedOrdinate );
      transform.transform( srcPts, srcOff += firstAffectedOrdinate, dstPts,
          dstOff += firstAffectedOrdinate, 1 );
      System.arraycopy( srcPts, srcOff += subDimSource, dstPts, dstOff += subDimTarget,
          numTrailingOrdinates );
      srcOff += srcStep;
      dstOff += dstStep;
    }
  }

  /**
   * Gets the derivative of this transform at a point.
   */
  public Matrix derivative( final CoordinatePoint point ) throws TransformException
  {
    final int nSkipped = firstAffectedOrdinate + numTrailingOrdinates;
    final int transDim = transform.getDimSource();
    final int pointDim = point.getDimension();
    if( pointDim != transDim + nSkipped )
    {
      throw new MismatchedDimensionException( pointDim, transDim + nSkipped );
    }
    final CoordinatePoint subPoint = new CoordinatePoint( transDim );
    System.arraycopy( point.ord, firstAffectedOrdinate, subPoint.ord, 0, transDim );
    final Matrix subMatrix = transform.derivative( subPoint );
    final int numRow = subMatrix.getNumRow();
    final int numCol = subMatrix.getNumCol();
    final Matrix matrix = new Matrix( nSkipped + numRow, nSkipped + numCol );
    matrix.setZero();

    //  Set UL part to 1: [ 1 0 ]
    //                      [ 0 1 ]
    //                      [ ]
    //                      [ ]
    //                      [ ]
    for( int j = 0; j < firstAffectedOrdinate; j++ )
      matrix.setElement( j, j, 1 );

    //  Set central part: [ 1 0 0 0 0 0 ]
    //                      [ 0 1 0 0 0 0 ]
    //                      [ 0 0 ? ? ? 0 ]
    //                      [ 0 0 ? ? ? 0 ]
    //                      [ ]
    subMatrix.copySubMatrix( 0, 0, numRow, numCol, firstAffectedOrdinate, firstAffectedOrdinate,
        matrix );

    //  Set LR part to 1: [ 1 0 0 0 0 0 ]
    //                      [ 0 1 0 0 0 0 ]
    //                      [ 0 0 ? ? ? 0 ]
    //                      [ 0 0 ? ? ? 0 ]
    //                      [ 0 0 0 0 0 1 ]
    final int offset = numCol - numRow;
    for( int j = pointDim - numTrailingOrdinates; j < pointDim; j++ )
      matrix.setElement( j, j + offset, 1 );

    return matrix;
  }

  /**
   * Creates the inverse transform of this object.
   */
  public synchronized MathTransform inverse() throws NoninvertibleTransformException
  {
    if( inverse == null )
    {
      inverse = new PassThroughTransform( firstAffectedOrdinate, transform.inverse(),
          numTrailingOrdinates );
      inverse.inverse = this;
    }
    return inverse;
  }

  /**
   * Compares the specified object with this math transform for equality.
   */
  public boolean equals( final Object object )
  {
    if( object == this )
      return true;
    if( super.equals( object ) )
    {
      final PassThroughTransform that = (PassThroughTransform)object;
      return this.firstAffectedOrdinate == that.firstAffectedOrdinate
          && this.numTrailingOrdinates == that.numTrailingOrdinates
          && Utilities.equals( this.transform, that.transform );
    }
    return false;
  }

  /**
   * Returns the WKT for this math transform.
   */
  public String toString()
  {
    final StringBuffer buffer = new StringBuffer( "PASSTHROUGH_MT[" );
    buffer.append( firstAffectedOrdinate );
    buffer.append( ',' );
    if( numTrailingOrdinates != 0 )
    {
      // TODO: This parameter is not part of OpenGIS specification!
      //       We should returns a more complex WKT here, using an
      //       affine transform to change the coordinates order.
      buffer.append( numTrailingOrdinates );
      buffer.append( ',' );
    }
    buffer.append( transform );
    buffer.append( ']' );
    return buffer.toString();
  }
}