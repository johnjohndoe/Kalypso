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

// OpenGIS (SEAS) dependencies
//import org.deegree_impl.model.pt.ConvexHull;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;

import org.deegree_impl.model.pt.CoordinatePoint;
import org.deegree_impl.model.pt.Matrix;
import org.deegree_impl.model.resources.XAffineTransform;
import org.deegree_impl.model.resources.css.ResourceKeys;
import org.deegree_impl.model.resources.css.Resources;

/**
 * Transforms two-dimensional coordinate points using an {@link AffineTransform}.
 * 
 * @version 1.00
 * @author Martin Desruisseaux
 */
final class AffineTransform2D extends XAffineTransform implements MathTransform2D
{
  /**
   * Serial number for interoperability with different versions.
   */
  private static final long serialVersionUID = -5299837898367149069L;

  /**
   * The inverse transform. This field will be computed only when needed.
   */
  private transient AffineTransform2D inverse;

  /**
   * Construct an affine transform.
   */
  protected AffineTransform2D( final AffineTransform transform )
  {
    super( transform );
  }

  /**
   * Throws an {@link UnsupportedOperationException}when a mutable method is
   * invoked, since <code>AffineTransform2D</code> must be immutable.
   */
  protected void checkPermission()
  {
    throw new UnsupportedOperationException( Resources
        .format( ResourceKeys.ERROR_UNMODIFIABLE_AFFINE_TRANSFORM ) );
  }

  /**
   * Gets the dimension of input points.
   */
  public int getDimSource()
  {
    return 2;
  }

  /**
   * Gets the dimension of output points.
   */
  public int getDimTarget()
  {
    return 2;
  }

  /**
   * Transforms the specified <code>ptSrc</code> and stores the result in
   * <code>ptDst</code>.
   */
  public CoordinatePoint transform( final CoordinatePoint ptSrc, CoordinatePoint ptDst )
  {
    if( ptDst == null )
    {
      ptDst = new CoordinatePoint( 2 );
    }
    transform( ptSrc.ord, 0, ptDst.ord, 0, 1 );
    return ptDst;
  }

  /**
   * Gets the derivative of this transform at a point. For an affine transform,
   * the derivative is the same everywhere.
   */
  public Matrix derivative( final Point2D point )
  {
    final Matrix matrix = new Matrix( 2 );
    matrix.setElement( 0, 0, getScaleX() );
    matrix.setElement( 1, 1, getScaleY() );
    matrix.setElement( 0, 1, getShearX() );
    matrix.setElement( 1, 0, getShearY() );
    return matrix;
  }

  /**
   * Gets the derivative of this transform at a point. For an affine transform,
   * the derivative is the same everywhere.
   */
  public Matrix derivative( final CoordinatePoint point )
  {
    return derivative( (Point2D)null );
  }

  /**
   * Creates the inverse transform of this object.
   */
  public synchronized MathTransform inverse() throws NoninvertibleTransformException
  {
    if( inverse == null )
      try
      {
        if( !isIdentity() )
        {
          inverse = new AffineTransform2D( createInverse() );
          inverse.inverse = this;
        }
        else
          inverse = this;
      }
      catch( java.awt.geom.NoninvertibleTransformException exception )
      {
        throw new NoninvertibleTransformException( exception.getLocalizedMessage(), exception );
      }
    return inverse;
  }

  /**
   * Returns the WKT for this affine transform.
   */
  public String toString()
  {
    return MatrixTransform.toString( new Matrix( this ) );
  }
}