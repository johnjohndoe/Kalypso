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

// OpenGIS dependencies (SEAGIS)
import java.io.Serializable;

import org.kalypsodeegree_impl.model.pt.CoordinatePoint;
import org.kalypsodeegree_impl.model.pt.Matrix;
import org.kalypsodeegree_impl.model.resources.Utilities;
import org.kalypsodeegree_impl.model.resources.css.ResourceKeys;
import org.kalypsodeegree_impl.model.resources.css.Resources;

/**
 * Base class for concatened transform. Concatened transforms are serializable if all their step transforms are
 * serializables.
 * 
 * @version 1.0
 * @author Martin Desruisseaux
 */
class ConcatenedTransform extends AbstractMathTransform implements Serializable
{
  /**
   * Serial number for interoperability with different versions.
   */
  private static final long serialVersionUID = 5772066656987558634L;

  /**
   * The math transform factory that created this concatened transform. Will be used for creating the inverse transform
   * when needed.
   */
  private MathTransformFactory provider;

  /**
   * The first math transform.
   */
  protected final MathTransform m_transform1;

  /**
   * The second math transform.
   */
  protected final MathTransform m_transform2;

  /**
   * The inverse transform. This field will be computed only when needed.
   */
  private transient MathTransform inverse;

  /**
   * Construct a concatenated transform.
   */
  public ConcatenedTransform( final MathTransformFactory provider, final MathTransform transform1,
      final MathTransform transform2 )
  {
    this.provider = provider;
    this.m_transform1 = transform1;
    this.m_transform2 = transform2;
    if( !isValid() )
    {
      throw new IllegalArgumentException( Resources.format( ResourceKeys.ERROR_CANT_CONCATENATE_CS_$2,
          getName( transform1 ), getName( transform2 ) ) );
    }
  }

  /**
   * Returns a name for the specified coordinate system.
   */
  private static final String getName( final MathTransform transform )
  {
    if( transform instanceof AbstractMathTransform )
    {
      String name = ( (AbstractMathTransform)transform ).getName( null );
      if( name != null && ( name = name.trim() ).length() != 0 )
        return name;
    }
    return Utilities.getShortClassName( transform );
  }

  /**
   * Check if transforms are compatibles. The default implementation check if transfert dimension match.
   */
  protected boolean isValid()
  {
    return m_transform1.getDimTarget() == m_transform2.getDimSource();
  }

  /**
   * Gets the dimension of input points.
   */
  public final int getDimSource()
  {
    return m_transform1.getDimSource();
  }

  /**
   * Gets the dimension of output points.
   */
  public final int getDimTarget()
  {
    return m_transform2.getDimTarget();
  }

  /**
   * Transforms the specified <code>ptSrc</code> and stores the result in <code>ptDst</code>.
   */
  public CoordinatePoint transform( final CoordinatePoint ptSrc, CoordinatePoint ptDst ) throws TransformException
  {
    //  Note: If we know that the transfert dimension is the same than source
    //        and target dimension, then we don't need to use an intermediate
    //        point. This optimization is done in ConcatenedTransformDirect.
    return m_transform2.transform( m_transform1.transform( ptSrc, null ), ptDst );
  }

  /**
   * Transforms a list of coordinate point ordinal values.
   */
  public void transform( final double[] srcPts, final int srcOff, final double[] dstPts, final int dstOff,
      final int numPts ) throws TransformException
  {
    //  Note: If we know that the transfert dimension is the same than source
    //        and target dimension, then we don't need to use an intermediate
    //        buffer. This optimization is done in ConcatenedTransformDirect.
    final double[] tmp = new double[numPts * m_transform1.getDimTarget()];
    m_transform1.transform( srcPts, srcOff, tmp, 0, numPts );
    m_transform2.transform( tmp, 0, dstPts, dstOff, numPts );
  }

  /**
   * Transforms a list of coordinate point ordinal values.
   */
  public void transform( final float[] srcPts, final int srcOff, final float[] dstPts, final int dstOff,
      final int numPts ) throws TransformException
  {
    //  Note: If we know that the transfert dimension is the same than source
    //        and target dimension, then we don't need to use an intermediate
    //        buffer. This optimization is done in ConcatenedTransformDirect.
    final float[] tmp = new float[numPts * m_transform1.getDimTarget()];
    m_transform1.transform( srcPts, srcOff, tmp, 0, numPts );
    m_transform2.transform( tmp, 0, dstPts, dstOff, numPts );
  }

  /**
   * Creates the inverse transform of this object.
   */
  public synchronized final MathTransform inverse() throws NoninvertibleTransformException
  {
    if( inverse == null )
    {
      if( provider == null )
      {
        provider = MathTransformFactory.getDefault();
      }
      inverse = provider.createConcatenatedTransform( m_transform2.inverse(), m_transform1.inverse() );
      if( inverse instanceof ConcatenedTransform )
      {
        ( (ConcatenedTransform)inverse ).inverse = this;
      }
    }
    return inverse;
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
  public Matrix derivative( final CoordinatePoint point ) throws TransformException
  {
    final Matrix matrix1 = m_transform1.derivative( point );
    final Matrix matrix2 = m_transform2.derivative( m_transform1.transform( point, null ) );
    // Compute "matrix = matrix2 * matrix1". Reuse an existing matrix object
    // if possible, which is always the case when both matrix are square.
    final int numRow = matrix2.getNumRow();
    final int numCol = matrix1.getNumCol();
    final Matrix matrix;
    if( numCol == matrix2.getNumCol() )
    {
      matrix = matrix2;
      matrix2.mul( matrix1 );
    }
    else
    {
      matrix = new Matrix( numRow, numCol );
      matrix.mul( matrix2, matrix1 );
    }
    return matrix;
  }

  /**
   * Tests whether this transform does not move any points. Default implementation check if the two transforms are
   * identity. This a way too conservative aproach, but it it doesn't hurt since ConcatenedTransform should not have
   * been created if it were to result in an identity transform (this case should have been detected earlier).
   */
  public final boolean isIdentity()
  {
    return m_transform1.isIdentity() && m_transform2.isIdentity();
  }

  /**
   * Returns a hash value for this transform.
   */
  public final int hashCode()
  {
    return m_transform1.hashCode() + 37 * m_transform2.hashCode();
  }

  /**
   * Compares the specified object with this math transform for equality.
   */
  public final boolean equals( final Object object )
  {
    if( object == this )
      return true; // Slight optimization
    if( super.equals( object ) )
    {
      final ConcatenedTransform that = (ConcatenedTransform)object;
      return Utilities.equals( this.m_transform1, that.m_transform1 )
          && Utilities.equals( this.m_transform2, that.m_transform2 );
    }
    return false;
  }

  /**
   * Returns the WKT for this math transform.
   */
  public final String toString()
  {
    final StringBuffer buffer = new StringBuffer( "CONCAT_MT[" );
    addWKT( buffer, this, true );
    buffer.append( ']' );
    return buffer.toString();
  }

  /**
   * Append to a string buffer the WKT for the specified math transform.
   */
  private static void addWKT( final StringBuffer buffer, final MathTransform transform, final boolean first )
  {
    if( transform instanceof ConcatenedTransform )
    {
      final ConcatenedTransform concat = (ConcatenedTransform)transform;
      addWKT( buffer, concat.m_transform1, first );
      addWKT( buffer, concat.m_transform2, false );
    }
    else
    {
      if( !first )
        buffer.append( ", " );
      buffer.append( transform );
    }
  }
}