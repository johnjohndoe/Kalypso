/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

 It has been implemented within SEAGIS - An OpenSource implementation of OpenGIS specification
 (C) 2001, Institut de Recherche pour le Développement (http://sourceforge.net/projects/seagis/)
 SEAGIS Contacts:  Surveillance de l'Environnement Assistée par Satellite
 Institut de Recherche pour le Développement / US-Espace
 mailto:seasnet@teledetection.fr


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

// OpenGIS dependencies
import java.io.Serializable;
import java.rmi.RemoteException;
import java.util.Arrays;

import org.deegree_impl.model.pt.CoordinatePoint;
import org.deegree_impl.model.pt.Matrix;
import org.deegree_impl.model.resources.Utilities;
import org.opengis.ct.CT_MathTransform;
import org.opengis.pt.PT_CoordinatePoint;

/**
 * Wrap an {@link CT_MathTransform}into a {@link MathTransform}. This class is
 * provided for compatibility with OpenGIS. It is serializable if the underlying
 * {@link CT_MathTransform}is serializable too.
 * 
 * @version 1.0
 * @author Martin Desruisseaux
 */
class MathTransformAdapter extends AbstractMathTransform implements Serializable
{
  /**
   * Serial number for interoperability with different versions.
   */
  private static final long serialVersionUID = 4497134108375420674L;

  /**
   * The OpenGIS math transform.
   */
  protected final CT_MathTransform transform;

  /**
   * Dimension of output points.
   */
  private final int dimSource;

  /**
   * Dimension of input points.
   */
  private final int dimTarget;

  /**
   * <code>true</code> if this transform does not move any points.
   */
  private final boolean isIdentity;

  /**
   * The inverse transform. This field will be computed only when needed.
   */
  protected transient MathTransformAdapter inverse;

  /**
   * Construct an adapter.
   * 
   * @throws RemoteException
   *           if a remote call failed.
   */
  public MathTransformAdapter( final CT_MathTransform transform ) throws RemoteException
  {
    this.transform = transform;
    this.dimSource = transform.getDimSource();
    this.dimTarget = transform.getDimTarget();
    this.isIdentity = transform.isIdentity();
  }

  /**
   * Transforms a list of coordinate point ordinal values.
   * 
   * @throws TransformException
   *           if the points can't be transformed, or if a remote call failed.
   */
  public final void transform( final double[] srcPts, final int srcOff, final double[] dstPts,
      final int dstOff, final int numPts ) throws TransformException
  {
    try
    {
      if( srcOff == 0 && dstOff == 0 && srcPts == dstPts && dimSource == dimTarget
          && numPts * dimSource == srcPts.length )
      {
        // Special optimization without intermediate buffer.
        final double[] array = transform.transformList( srcPts );
        if( array != dstPts )
        {
          System.arraycopy( array, 0, dstPts, 0, array.length );
          Arrays.fill( dstPts, array.length, dstPts.length, Double.NaN );
        }
      }
      else
      {
        // The following array way be larger than necessary, but we make
        // it large enough to give a change to 'transformList' to reuse it.
        double[] array = new double[numPts * Math.max( dimSource, dimTarget )];
        System.arraycopy( srcPts, srcOff, array, 0, numPts * dimSource );
        array = transform.transformList( array );
        System.arraycopy( array, 0, dstPts, dstOff, array.length );
        Arrays.fill( dstPts, array.length, numPts * dimTarget, Double.NaN );
      }
    }
    catch( RemoteException exception )
    {
      final Throwable cause = exception.detail;
      if( cause instanceof TransformException )
      {
        throw (TransformException)cause;
      }
      throw new TransformException( exception.getLocalizedMessage(), exception );
    }
  }

  /**
   * Transforms a list of coordinate point ordinal values.
   * 
   * @throws TransformException
   *           if the points can't be transformed, or if a remote call failed.
   */
  public final void transform( final float[] srcPts, final int srcOff, final float[] dstPts,
      final int dstOff, final int numPts ) throws TransformException
  {
    try
    {
      // The following array way be larger than necessary, but we make
      // it large enough to give a change to 'transformList' to reuse it.
      double[] array = new double[numPts * Math.max( dimSource, dimTarget )];
      for( int i = numPts * dimSource; --i >= 0; )
        array[i] = srcPts[i + srcOff];
      array = transform.transformList( array );
      for( int i = array.length; --i >= 0; )
        dstPts[i + dstOff] = (float)array[i];
      Arrays.fill( dstPts, array.length, numPts * dimTarget, Float.NaN );
    }
    catch( RemoteException exception )
    {
      final Throwable cause = exception.detail;
      if( cause instanceof TransformException )
      {
        throw (TransformException)cause;
      }
      throw new TransformException( exception.getLocalizedMessage(), exception );
    }
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
    try
    {
      final PT_CoordinatePoint ogPoint;
      if( point != null )
      {
        ogPoint = new PT_CoordinatePoint();
        ogPoint.ord = point.ord;
      }
      else
        ogPoint = null;
      return new Matrix( transform.derivative( ogPoint ).elt );
    }
    catch( RemoteException exception )
    {
      final Throwable cause = exception.detail;
      if( cause instanceof TransformException )
      {
        throw (TransformException)cause;
      }
      throw new TransformException( exception.getLocalizedMessage(), exception );
    }
  }

  /**
   * Creates the inverse transform of this object.
   * 
   * @throws NoninvertibleTransformException
   *           if the inverse transform can't be created, or if a remote call
   *           failed.
   */
  public synchronized MathTransform inverse() throws NoninvertibleTransformException
  {
    if( inverse == null )
      try
      {
        inverse = new MathTransformAdapter( transform.inverse() );
        inverse.inverse = this;
      }
      catch( RemoteException exception )
      {
        final Throwable cause = exception.detail;
        if( cause instanceof NoninvertibleTransformException )
        {
          throw (NoninvertibleTransformException)cause;
        }
        throw new NoninvertibleTransformException( exception.getLocalizedMessage(), exception );
      }
    return inverse;
  }

  /**
   * Gets the dimension of input points.
   */
  public final int getDimSource()
  {
    return dimSource;
  }

  /**
   * Gets the dimension of output points.
   */
  public final int getDimTarget()
  {
    return dimTarget;
  }

  /**
   * Tests whether this transform does not move any points.
   */
  public final boolean isIdentity()
  {
    return isIdentity;
  }

  /**
   * Returns the underlying OpenGIS interface.
   */
  final Object toOpenGIS( final Object adapters )
  {
    return transform;
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
      return Utilities.equals( ( (MathTransformAdapter)object ).transform, transform );
    }
    return false;
  }
}