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

// OpenGIS dependencies
import java.awt.geom.Point2D;
import java.rmi.RemoteException;

import org.kalypsodeegree_impl.model.pt.Matrix;
import org.opengis.ct.CT_MathTransform;
import org.opengis.pt.PT_CoordinatePoint;

/**
 * Wrap an {@link CT_MathTransform}into a {@link MathTransform2D}.
 * 
 * @version 1.0
 * @author Martin Desruisseaux
 */
final class MathTransformAdapter2D extends MathTransformAdapter implements MathTransform2D
{
  /**
   * Serial number for interoperability with different versions.
   */
  private static final long serialVersionUID = 2870952997133267365L;

  /**
   * Construct an adapter.
   * 
   * @throws RemoteException
   *           if a remote call failed.
   */
  public MathTransformAdapter2D( final CT_MathTransform transform ) throws RemoteException
  {
    super( transform );
    if( getDimSource() != 2 || getDimTarget() != 2 )
    {
      throw new IllegalArgumentException();
    }
  }

  /**
   * Transforms the specified <code>ptSrc</code> and stores the result in
   * <code>ptDst</code>.
   */
  public Point2D transform( final Point2D ptSrc, final Point2D ptDst ) throws TransformException
  {
    /*
     * //----- BEGIN JDK 1.4 DEPENDENCIES ----- // assert getDimSource()==2 &&
     * getDimTarget()==2;
     *///----- END OF JDK 1.4 DEPENDENCIES ----
    try
    {
      double[] array = transform.transformList( new double[]
      { ptSrc.getX(), ptSrc.getY() } );
      if( ptDst != null )
      {
        ptDst.setLocation( array[0], array[1] );
        return ptDst;
      }
      else
        return new Point2D.Double( array[0], array[1] );
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
  public Matrix derivative( final Point2D point ) throws TransformException
  {
    try
    {
      final PT_CoordinatePoint ogPoint = ( point != null ) ? new PT_CoordinatePoint( point.getX(),
          point.getY() ) : null;
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
}