/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.model.geometry;

import java.io.Serializable;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import org.deegree.crs.transformations.CRSTransformation;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_GenericSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Ring;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfaceBoundary;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree.model.geometry.ISurfacePatchVisitor;
import org.kalypsodeegree_impl.tools.Debug;
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
class GM_Surface_Impl<T extends GM_SurfacePatch> extends GM_OrientableSurface_Impl implements GM_Surface<T>, GM_GenericSurface, Serializable
{
  /** Use serialVersionUID for interoperability. */
  private final static long serialVersionUID = -2148069106391096842L;

  private final List<T> m_list;

  private final GM_SurfacePatch m_patch;

  private double area = 0;

  /**
   * initializes the surface with default orientation submitting one surface patch.
   * 
   * @param surfacePatch
   *            patches of the surface.
   */
  public GM_Surface_Impl( final T surfacePatch ) throws GM_Exception
  {
    this( '+', surfacePatch );
  }

  /**
   * initializes the surface submitting the orientation and one surface patch.
   * 
   * @param surfacePatch
   *            patches of the surface.
   */
  public GM_Surface_Impl( final char orientation, final T surfacePatch ) throws GM_Exception
  {
    super( surfacePatch.getCoordinateSystem(), orientation );

    m_list = Collections.singletonList( surfacePatch );
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

    m_list = Collections.emptyList();
    m_patch = null;

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
      if( !m_patch.equals( ((GM_Surface< ? >) other).get( 0 ) ) )
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

      return new GM_Surface_Impl<GM_SurfacePatch>( getOrientation(), myPatch );
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
   */
  @Override
  public String toString( )
  {
    String ret = getClass().getName() + ":\n";

    ret += ("envelope = " + getEnvelope() + "\n");
    ret += " CRS: " + getCoordinateSystem() + "\n";
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
    if( adapter == GM_SurfacePatch[].class )
    {
      return new GM_SurfacePatch[] { m_patch };
    }

    if( adapter == GM_Curve.class )
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

    return super.getAdapter( adapter );
  }

  /**
   * @param index
   * @param element
   * @see java.util.List#add(int, java.lang.Object)
   */
  public void add( final int index, final T element )
  {
    m_list.add( index, element );
  }

  /**
   * @param o
   * @return
   * @see java.util.List#add(java.lang.Object)
   */
  public boolean add( final T o )
  {
    return m_list.add( o );
  }

  /**
   * @param c
   * @return
   * @see java.util.List#addAll(java.util.Collection)
   */
  public boolean addAll( final Collection< ? extends T> c )
  {
    return m_list.addAll( c );
  }

  /**
   * @param index
   * @param c
   * @return
   * @see java.util.List#addAll(int, java.util.Collection)
   */
  public boolean addAll( final int index, final Collection< ? extends T> c )
  {
    return m_list.addAll( index, c );
  }

  /**
   * @see java.util.List#clear()
   */
  public void clear( )
  {
    m_list.clear();
  }

  /**
   * @param o
   * @return
   * @see java.util.List#contains(java.lang.Object)
   */
  public boolean contains( final Object o )
  {
    return m_list.contains( o );
  }

  /**
   * @param c
   * @return
   * @see java.util.List#containsAll(java.util.Collection)
   */
  public boolean containsAll( final Collection< ? > c )
  {
    return m_list.containsAll( c );
  }

  /**
   * @param index
   * @return
   * @see java.util.List#get(int)
   */
  public T get( final int index )
  {
    return m_list.get( index );
  }

  /**
   * @return
   * @see java.util.List#hashCode()
   */
  @Override
  public int hashCode( )
  {
    return m_list.hashCode();
  }

  /**
   * @param o
   * @return
   * @see java.util.List#indexOf(java.lang.Object)
   */
  public int indexOf( final Object o )
  {
    return m_list.indexOf( o );
  }

  /**
   * @return
   * @see java.util.List#isEmpty()
   */
  @Override
  public boolean isEmpty( )
  {
    return m_list.isEmpty();
  }

  /**
   * @return
   * @see java.util.List#iterator()
   */
  public Iterator<T> iterator( )
  {
    return m_list.iterator();
  }

  /**
   * @param o
   * @return
   * @see java.util.List#lastIndexOf(java.lang.Object)
   */
  public int lastIndexOf( final Object o )
  {
    return m_list.lastIndexOf( o );
  }

  /**
   * @return
   * @see java.util.List#listIterator()
   */
  public ListIterator<T> listIterator( )
  {
    return m_list.listIterator();
  }

  /**
   * @param index
   * @return
   * @see java.util.List#listIterator(int)
   */
  public ListIterator<T> listIterator( final int index )
  {
    return m_list.listIterator( index );
  }

  /**
   * @param index
   * @return
   * @see java.util.List#remove(int)
   */
  public T remove( final int index )
  {
    return m_list.remove( index );
  }

  /**
   * @param o
   * @return
   * @see java.util.List#remove(java.lang.Object)
   */
  public boolean remove( final Object o )
  {
    return m_list.remove( o );
  }

  /**
   * @param c
   * @return
   * @see java.util.List#removeAll(java.util.Collection)
   */
  public boolean removeAll( final Collection< ? > c )
  {
    return m_list.removeAll( c );
  }

  /**
   * @param c
   * @return
   * @see java.util.List#retainAll(java.util.Collection)
   */
  public boolean retainAll( final Collection< ? > c )
  {
    return m_list.retainAll( c );
  }

  /**
   * @param index
   * @param element
   * @return
   * @see java.util.List#set(int, java.lang.Object)
   */
  public T set( final int index, final T element )
  {
    return m_list.set( index, element );
  }

  /**
   * @return
   * @see java.util.List#size()
   */
  public int size( )
  {
    return m_list.size();
  }

  /**
   * @param fromIndex
   * @param toIndex
   * @return
   * @see java.util.List#subList(int, int)
   */
  public List<T> subList( final int fromIndex, final int toIndex )
  {
    return m_list.subList( fromIndex, toIndex );
  }

  /**
   * @return
   * @see java.util.List#toArray()
   */
  public Object[] toArray( )
  {
    return m_list.toArray();
  }

  /**
   * @param <T>
   * @param a
   * @return
   * @see java.util.List#toArray(T[])
   */
  public <S> S[] toArray( final S[] a )
  {
    return m_list.toArray( a );
  }

  /**
   * @see org.kalypsodeegree.model.geometry.ISurfacePatchVisitable#acceptSurfacePatches(org.kalypsodeegree.model.geometry.GM_Envelope,
   *      org.kalypsodeegree.model.geometry.ISurfacePatchVisitor, org.eclipse.core.runtime.IProgressMonitor)
   */
  public void acceptSurfacePatches( final GM_Envelope envToVisit, final ISurfacePatchVisitor<T> visitor, final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( "", m_list.size() );

    for( final T patch : m_list )
    {
      visitor.visit( patch, Double.NaN );
      ProgressUtilities.worked( monitor, 1 );
    }
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Object#transform(org.deegree.crs.transformations.CRSTransformation,
   *      java.lang.String)
   */
  public GM_Object transform( final CRSTransformation trans, final String targetOGCCS ) throws Exception
  {
    /* If the target is the same coordinate system, do not transform. */
    final String coordinateSystem = getCoordinateSystem();
    if( coordinateSystem == null || coordinateSystem.equalsIgnoreCase( targetOGCCS ) )
      return this;

    Debug.debugMethodBegin( this, "transformSurface" );

    final int cnt = size();
    final GM_SurfacePatch[] patches = new GM_SurfacePatch[cnt];

    for( int i = 0; i < cnt; i++ )
    {
      patches[i] = (GM_SurfacePatch) get( i ).transform( trans, targetOGCCS );
    }

    // at the moment only polygons made of one patch are supported
    // TODO: change it!
    Debug.debugMethodEnd();
    return GeometryFactory.createGM_Surface( patches[0] );
  }

  /**
   * @see org.kalypsodeegree_impl.model.geometry.GM_Object_Impl#setCoordinateSystem(java.lang.String)
   */
  @Override
  public void setCoordinateSystem( final String crs )
  {
    super.setCoordinateSystem( crs );

    if( m_patch instanceof GM_SurfacePatch_Impl )
      ((GM_SurfacePatch_Impl) m_patch).setCoordinateSystem( crs );
  }
}