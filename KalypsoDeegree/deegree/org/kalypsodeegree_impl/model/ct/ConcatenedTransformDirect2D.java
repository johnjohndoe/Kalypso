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
package org.kalypsodeegree_impl.model.ct;

// Geometry
import java.awt.Shape;
import java.awt.geom.Point2D;

import org.kalypsodeegree_impl.model.pt.Matrix;

/**
 * Concatened transform where both transforms are two-dimensional.
 * 
 * @version 1.0
 * @author Martin Desruisseaux
 */
final class ConcatenedTransformDirect2D extends ConcatenedTransformDirect implements MathTransform2D
{
  /**
   * Serial number for interoperability with different versions.
   */
  private static final long serialVersionUID = 6009454091075588885L;

  /**
   * The first math transform. This field is identical to {@link ConcatenedTransform#m_transform1}. Only the type is
   * different.
   */
  private final MathTransform2D transform1;

  /**
   * The second math transform. This field is identical to {@link ConcatenedTransform#m_transform1}. Only the type is
   * different.
   */
  private final MathTransform2D transform2;

  /**
   * Construct a concatenated transform.
   */
  public ConcatenedTransformDirect2D( final MathTransformFactory provider, final MathTransform2D transform1,
      final MathTransform2D transform2 )
  {
    super( provider, transform1, transform2 );
    this.transform1 = transform1;
    this.transform2 = transform2;
  }

  /**
   * Check if transforms are compatibles with this implementation.
   */
  protected boolean isValid()
  {
    return super.isValid() && getDimSource() == 2 && getDimTarget() == 2;
  }

  /**
   * Transforms the specified <code>ptSrc</code> and stores the result in <code>ptDst</code>.
   */
  public Point2D transform( final Point2D ptSrc, Point2D ptDst ) throws TransformException
  {
    ptDst = transform1.transform( ptSrc, ptDst );
    return transform2.transform( ptDst, ptDst );
  }

  /**
   * Transform the specified shape.
   */
  public Shape createTransformedShape( final Shape shape ) throws TransformException
  {
    return transform2.createTransformedShape( transform1.createTransformedShape( shape ) );
  }

  /**
   * Gets the derivative of this transform at a point.
   * 
   * @param point
   *          The coordinate point where to evaluate the derivative.
   * @return The derivative at the specified point (never <code>null</code>).
   * @throws TransformException
   *           if the derivative can't be evaluated at the specified point.
   */
  public Matrix derivative( final Point2D point ) throws TransformException
  {
    final Matrix matrix1 = transform1.derivative( point );
    final Matrix matrix2 = transform2.derivative( transform1.transform( point, null ) );
    matrix2.mul( matrix1 );
    return matrix2;
  }
}