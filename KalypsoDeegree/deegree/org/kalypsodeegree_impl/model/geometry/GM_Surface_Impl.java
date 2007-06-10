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
package org.kalypsodeegree_impl.model.geometry;

import java.io.Serializable;
import java.rmi.RemoteException;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_GenericSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Ring;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfaceBoundary;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * default implementation of the GM_Surface interface from package jago.model.
 * <p>
 * </p>
 * for simplicity of the implementation it is assumed that a surface is build from just one surface patch. this isn't
 * completly confrom to the ISO 19107 and the OGC GAIA specification but sufficient for most applications.
 * <p>
 * </p>
 * It will be extended to fullfill the complete specs as soon as possible.
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @version 05.04.2002
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
class GM_Surface_Impl extends GM_OrientableSurface_Impl implements GM_Surface, GM_GenericSurface, Serializable
{
  /** Use serialVersionUID for interoperability. */
  private final static long serialVersionUID = -2148069106391096842L;

  protected GM_SurfacePatch m_patch = null;

  private double area = 0;

  /**
   * initializes the surface with default orientation submitting one surface patch.
   * 
   * @param surfacePatch
   *            patches of the surface.
   */
  public GM_Surface_Impl( final GM_SurfacePatch surfacePatch ) throws GM_Exception
  {
    this( '+', surfacePatch );
  }

  /**
   * initializes the surface submitting the orientation and one surface patch.
   * 
   * @param surfacePatch
   *            patches of the surface.
   */
  public GM_Surface_Impl( final char orientation, final GM_SurfacePatch surfacePatch ) throws GM_Exception
  {
    super( surfacePatch.getCoordinateSystem(), orientation );

    m_patch = surfacePatch;

    setValid( false );
  }

  /**
   * initializes the surface with default orientation submitting the surfaces boundary
   * 
   * @param boundary
   *            boundary of the surface
   */
  public GM_Surface_Impl( final GM_SurfaceBoundary boundary ) throws GM_Exception
  {
    this( '+', boundary );
  }

  /**
   * initializes the surface submitting the orientation and the surfaces boundary.
   * 
   * @param boundary
   *            boundary of the surface
   */
  public GM_Surface_Impl( final char orientation, final GM_SurfaceBoundary boundary ) throws GM_Exception
  {
    // todo
    // extracting surface patches from the boundary
    super( boundary.getCoordinateSystem(), orientation );

    setBoundary( boundary );
  }

  /**
   * calculates the centroid and area of the surface
   */
  private void calculateCentroidArea( )
  {
    setCentroid( m_patch.getCentroid() );
    area = m_patch.getArea();
  }

  /**
   * calculates the boundary and area of the surface
   */
  private void calculateBoundary( )
  {
    try
    {
      final GM_Ring ext = new GM_Ring_Impl( m_patch.getExteriorRing(), getCoordinateSystem() );
      final GM_Position[][] inn_ = m_patch.getInteriorRings();
      GM_Ring[] inn = null;

      if( inn_ != null )
      {
        inn = new GM_Ring_Impl[inn_.length];

        for( int i = 0; i < inn_.length; i++ )
        {
          inn[i] = new GM_Ring_Impl( inn_[i], getCoordinateSystem() );
        }
      }

      setBoundary( new GM_SurfaceBoundary_Impl( ext, inn ) );
    }
    catch( final Exception e )
    {
      System.out.println( e );
    }
  }

  /**
   * calculates area, centroid and the envelope of the surface
   */
  @Override
  protected void calculateParam( )
  {
    calculateCentroidArea();
    calculateEnvelope();
    calculateBoundary();
    setValid( true );
    setCentroid( GeometryUtilities.guessPointOnSurface( this, getCentroid(), 3 ) );
  }

  /**
   * calculates the envelope of the surface
   */
  private void calculateEnvelope( )
  {
    setEnvelope( m_patch.getEnvelope() );
  }

  /**
   * returns the length of all boundaries of the surface in a reference system appropriate for measuring distances.
   */
  public double getPerimeter( )
  {
    return -1;
  }

  /**
   * The operation "area" shall return the area of this GM_GenericSurface. The area of a 2 dimensional geometric object
   * shall be a numeric measure of its surface area Since area is an accumulation (integral) of the product of two
   * distances, its return value shall be in a unit of measure appropriate for measuring distances squared.
   */
  public double getArea( )
  {
    if( !isValid() )
    {
      calculateParam();
    }
    return area;
  }

  /**
   * returns the boundary of the surface as surface boundary
   */
  public GM_SurfaceBoundary getSurfaceBoundary( )
  {
    if( !isValid() )
    {
      calculateParam();
    }
    return (GM_SurfaceBoundary) getBoundary();
  }

  /**
   * returns the number of patches building the surface
   */
  public int getNumberOfSurfacePatches( )
  {
    return 1;
  }

  /**
   * returns the surface patch at the submitted index
   */
  public GM_SurfacePatch getSurfacePatchAt( final int index ) throws GM_Exception
  {
    if( index != 0 )
    {
      throw new GM_Exception( "invalid index/position to get a patch!" );
    }

    return m_patch;
  }

  /**
   * writes a surface patch to the surface at submitted position. the old patch will be deleted
   */
  public void setSurfacePatchAt( final GM_SurfacePatch patch, final int index ) throws GM_Exception
  {
    if( index != 0 )
    {
      throw new GM_Exception( "invalid index/position to set a patch!" );
    }

    m_patch = patch;

    setValid( false );
  }

  /**
   * inserts a surface patch in the curve at the submitted position. all points with a position that equals index or is
   * higher will be shifted
   */
  public void insertSurfacePatchAt( final GM_SurfacePatch patch, final int index ) throws GM_Exception
  {
    if( index != 0 )
    {
      throw new GM_Exception( "invalid index/position to insert a patch!" );
    }

    m_patch = patch;

    setValid( false );
  }

  /**
   * adds a surface patch at the end of the curve
   */
  public void addSurfacePatch( final GM_SurfacePatch patch )
  {
    throw new NoSuchMethodError( "Surfaces made of more then one surface patch " + "are not supported at the moment." );
  }

  /**
   * deletes the surface patch at the submitted index
   */
  public void deleteSurfacePatchAt( final int index )
  {
    throw new NoSuchMethodError( "Surfaces made of more then one surface patch " + "are not supported at the moment. Because " + "empty surface are not allowed you can't delete "
        + "the only existing patch." );
  }

  /**
   * checks if this surface is completly equal to the submitted geometry
   * 
   * @param other
   *            object to compare to
   */
  @Override
  public boolean equals( final Object other )
  {
    if( !super.equals( other ) )
    {
      return false;
    }

    if( !(other instanceof GM_Surface_Impl) )
    {
      return false;
    }

    if( getEnvelope() == null )
    {
      calculateEnvelope();
    }
    if( !getEnvelope().equals( ((GM_Object) other).getEnvelope() ) )
    {
      return false;
    }

    try
    {
      if( !m_patch.equals( ((GM_Surface) other).getSurfacePatchAt( 0 ) ) )
      {
        return false;
      }
    }
    catch( final Exception e )
    {
      return false;
    }

    return true;
  }

  /**
   * The operation "dimension" shall return the inherent dimension of this GM_Object, which shall be less than or equal
   * to the coordinate dimension. The dimension of a collection of geometric objects shall be the largest dimension of
   * any of its pieces. Points are 0-dimensional, curves are 1-dimensional, surfaces are 2-dimensional, and solids are
   * 3-dimensional.
   */
  public int getDimension( )
  {
    return 2;
  }

  /**
   * The operation "coordinateDimension" shall return the dimension of the coordinates that define this GM_Object, which
   * must be the same as the coordinate dimension of the coordinate reference system for this GM_Object.
   */
  public int getCoordinateDimension( )
  {
    return m_patch.getExteriorRing()[0].getAsArray().length;
  }

  /**
   * returns a shallow copy of the geometry
   */
  @Override
  public Object clone( ) throws CloneNotSupportedException
  {
    // kuch
    try
    {
      final GM_SurfacePatch myPatch = (GM_SurfacePatch) m_patch.clone();

      return new GM_Surface_Impl( getOrientation(), myPatch );
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }

    throw (new IllegalStateException());
  }

  /**
   * translate each point of the surface with the values of the submitted double array.
   */
  @Override
  public void translate( final double[] d )
  {
    final GM_Position[] ext = m_patch.getExteriorRing();
    final GM_Position[][] inn = m_patch.getInteriorRings();

    for( final GM_Position element : ext )
    {
      element.translate( d );
    }

    if( inn != null )
    {
      for( final GM_Position[] element : inn )
      {
        for( final GM_Position element2 : element )
        {
          element2.translate( d );
        }
      }
    }
    setValid( false );
  }

  /**
   * The boolean valued operation "intersects" shall return TRUE if this <tt>GM_Surface_Impl</tt> intersects with the
   * given <tt>GM_Object</t>.
   * Within a <tt>GM_Complex</tt>, the <tt>GM_Primitives</tt> do not
   * intersect one another. In general, topologically structured data uses
   * shared geometric objects to capture intersection information.
   * @param gmo the <tt>GM_Object</tt> to test for intersection
   * @return true if the <tt>GM_Object</tt> intersects with this
   */
  @Override
  public boolean intersects( final GM_Object gmo )
  {
    if( !isValid() )
    {
      calculateParam();
    }
    return m_patch.contains( gmo ) || getBoundary().intersects( gmo );
  }

  /**
   * The Boolean valued operation "contains" shall return TRUE if this GM_Object contains a single point given by a
   * coordinate.
   * <p>
   * </p>
   */
  @Override
  public boolean contains( final GM_Position position )
  {
    return contains( new GM_Point_Impl( position, null ) );
  }

  /**
   * The Boolean valued operation "contains" shall return TRUE if this GM_Object contains another GM_Object.
   * <p>
   * </p>
   */
  @Override
  public boolean contains( final GM_Object gmo )
  {
    if( !isValid() )
    {
      calculateParam();
    }
    return getBoundary().contains( gmo );
  }

  /**
   * 
   *  
   */
  @Override
  public String toString( )
  {
    String ret = getClass().getName() + ":\n";
    ret += ("envelope = " + getEnvelope() + "\n");
    try
    {
      ret += " CRS: " + getCoordinateSystem().getName() + "\n";
    }
    catch( final RemoteException e )
    {
      e.printStackTrace();
    }
    ret += ("patch = " + m_patch + "\n");
    return ret;
  }

  /**
   * @see org.kalypsodeegree_impl.model.geometry.GM_Object_Impl#invalidate()
   */
  @Override
  public void invalidate( )
  {
    m_patch.invalidate();

    super.invalidate();
  }

  /**
   * @see org.kalypsodeegree_impl.model.geometry.GM_Primitive_Impl#getAdapter(java.lang.Class)
   */
  @SuppressWarnings("unchecked")
  @Override
  public Object getAdapter( final Class adapter )
  {
    if( adapter == GM_Curve.class )
    {
      {
        {
          final GM_SurfacePatch surfacePatchAt = m_patch;
          final GM_Position[] exteriorRing = surfacePatchAt.getExteriorRing();
          try
          {
            return GeometryFactory.createGM_Curve( exteriorRing, getCoordinateSystem() );
          }
          catch( final GM_Exception e )
          {
            final IStatus status = StatusUtilities.statusFromThrowable( e );
            KalypsoDeegreePlugin.getDefault().getLog().log( status );
            return null;
          }
        }
      }
    }

    return super.getAdapter( adapter );
  }
}