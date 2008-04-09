/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.model.geometry;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;

import org.deegree.crs.transformations.coordinate.CRSTransformation;
import org.deegree.model.crs.UnknownCRSException;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.transformation.CRSHelper;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfaceBoundary;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree.model.geometry.ISurfacePatchVisitor;
import org.kalypsodeegree_impl.tools.Debug;

import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.index.ItemVisitor;
import com.vividsolutions.jts.index.SpatialIndex;
import com.vividsolutions.jts.index.quadtree.Quadtree;

/**
 * @author Gernot Belger
 */
public class GM_TriangulatedSurface_Impl extends GM_OrientableSurface_Impl implements GM_TriangulatedSurface
{
  private SpatialIndex m_index = new Quadtree();

  private final List<GM_Triangle> m_items;

  public GM_TriangulatedSurface_Impl( final String crs ) throws GM_Exception
  {
    this( new ArrayList<GM_Triangle>(), crs );
  }

  public GM_TriangulatedSurface_Impl( final List<GM_Triangle> items, final String crs ) throws GM_Exception
  {
    super( crs );

    setValid( true );

    m_items = items;

    for( final GM_Triangle triangle : items )
      insertToIndex( triangle );
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Object#getDimension()
   */
  public int getDimension( )
  {
    return 2;
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Object#isEmpty()
   */
  @Override
  public boolean isEmpty( )
  {
    return m_items.isEmpty();
  }

  /**
   * @see java.util.List#add(java.lang.Object)
   */
  public boolean add( final GM_Triangle o )
  {
    m_items.add( o );

    insertToIndex( o );

    return true;
  }

  /**
   * @see java.util.List#add(int, java.lang.Object)
   */
  public void add( final int index, final GM_Triangle element )
  {
    m_items.add( index, element );
    insertToIndex( element );
  }

  /**
   * @see java.util.List#addAll(java.util.Collection)
   */
  public boolean addAll( final Collection< ? extends GM_Triangle> c )
  {
    for( final GM_Triangle triangle : c )
      add( triangle );

    return !c.isEmpty();
  }

  /**
   * @see java.util.List#addAll(int, java.util.Collection)
   */
  public boolean addAll( final int index, final Collection< ? extends GM_Triangle> c )
  {
    m_items.addAll( index, c );

    for( final GM_Triangle triangle : c )
      insertToIndex( triangle );

    return !c.isEmpty();
  }

  /**
   * @see java.util.List#clear()
   */
  public void clear( )
  {
    m_items.clear();

    m_index = new Quadtree();

    invalidate();
  }

  /**
   * @see java.util.List#contains(java.lang.Object)
   */
  public boolean contains( final Object o )
  {
    return m_items.contains( o );
  }

  /**
   * @see java.util.List#containsAll(java.util.Collection)
   */
  public boolean containsAll( final Collection< ? > c )
  {
    return m_items.containsAll( c );
  }

  /**
   * @see java.util.List#get(int)
   */
  public GM_Triangle get( final int index )
  {
    return m_items.get( index );
  }

  /**
   * @see java.util.List#indexOf(java.lang.Object)
   */
  public int indexOf( final Object o )
  {
    return m_items.indexOf( o );
  }

  /**
   * TODO: if this surface is changed via this iterator, the index does not gets updated
   * 
   * @see java.util.List#iterator()
   */
  public Iterator<GM_Triangle> iterator( )
  {
    return m_items.iterator();
  }

  /**
   * @see java.util.List#lastIndexOf(java.lang.Object)
   */
  public int lastIndexOf( final Object o )
  {
    return m_items.lastIndexOf( o );
  }

  /**
   * TODO: if this surface is changed via this iterator, the index does not gets updated
   * 
   * @see java.util.List#listIterator()
   */
  public ListIterator<GM_Triangle> listIterator( )
  {
    return m_items.listIterator();
  }

  /**
   * TODO: if this surface is changed via this iterator, the index does not gets updated
   * 
   * @see java.util.List#listIterator(int)
   */
  public ListIterator<GM_Triangle> listIterator( final int index )
  {
    return m_items.listIterator( index );
  }

  /**
   * @see java.util.List#remove(java.lang.Object)
   */
  public boolean remove( final Object o )
  {
    removeTriangleFromIndex( o );

    return m_items.remove( o );
  }

  /**
   * @see java.util.List#remove(int)
   */
  public GM_Triangle remove( final int index )
  {
    final GM_Triangle triangle = get( index );
    removeTriangleFromIndex( triangle );

    return m_items.remove( index );
  }

  /**
   * @see java.util.List#removeAll(java.util.Collection)
   */
  public boolean removeAll( final Collection< ? > c )
  {
    boolean hasRemoved = false;
    for( final Object object : c )
      hasRemoved |= remove( object );

    return hasRemoved;
  }

  /**
   * @see java.util.List#retainAll(java.util.Collection)
   */
  public boolean retainAll( final Collection< ? > c )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.List#set(int, java.lang.Object)
   */
  public GM_Triangle set( final int index, final GM_Triangle element )
  {
    final GM_Triangle triangle = get( index );
    removeTriangleFromIndex( triangle );

    m_items.set( index, element );

    insertToIndex( element );
    return triangle;
  }

  /**
   * @see java.util.List#size()
   */
  public int size( )
  {
    return m_items.size();
  }

  /**
   * @see java.util.List#subList(int, int)
   */
  public List<GM_Triangle> subList( final int fromIndex, final int toIndex )
  {
    try
    {
      return new GM_TriangulatedSurface_Impl( m_items.subList( fromIndex, toIndex ), getCoordinateSystem() );
    }
    catch( final GM_Exception e )
    {
      // should never happen
      throw new IllegalStateException( e );
    }
  }

  /**
   * @see java.util.List#toArray()
   */
  public Object[] toArray( )
  {
    return m_items.toArray();
  }

  /**
   * @see java.util.List#toArray(T[])
   */
  public <T> T[] toArray( final T[] a )
  {
    return m_items.toArray( a );
  }

  /**
   * @see java.lang.Object#clone()
   */
  @Override
  public Object clone( ) throws CloneNotSupportedException
  {
    try
    {
      final GM_TriangulatedSurface_Impl clone = new GM_TriangulatedSurface_Impl( getCoordinateSystem() );

      for( final GM_Triangle triangle : this )
        clone.add( (GM_Triangle) triangle.clone() );

      return clone;
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();

      throw new CloneNotSupportedException( e.getLocalizedMessage() );
    }
  }

  /**
   * @see org.kalypsodeegree_impl.model.geometry.GM_Object_Impl#calculateParam()
   */
  @Override
  protected void calculateParam( )
  {
    setEnvelope( JTSAdapter.wrap( recalcEnvelope( m_items ) ) );
    // TODO:: other parameters: centroid, ...?
  }

  private static Envelope recalcEnvelope( final List<GM_Triangle> items )
  {
    if( items.isEmpty() )
      return new Envelope();

    Envelope bbox = null;
    for( final GM_Triangle gmTriangle : items )
    {
      final GM_Envelope env = gmTriangle.getEnvelope();
      final Envelope envelope = JTSAdapter.export( env );
      if( envelope.isNull() )
        continue;

      if( bbox == null )
        bbox = envelope;
      else
        bbox.expandToInclude( envelope );
    }

    if( bbox == null )
      return new Envelope();

    return bbox;
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Object#getCoordinateDimension()
   */
  public int getCoordinateDimension( )
  {
    try
    {
      return CRSHelper.getDimension( getCoordinateSystem() );
    }
    catch( UnknownCRSException e )
    {
      // TODO How to deal with this error? What to return?
      e.printStackTrace();
      return 0;
    }
  }

  /**
   * @see org.kalypsodeegree_impl.model.geometry.GM_Primitive_Impl#getAdapter(java.lang.Class)
   */
  @SuppressWarnings("unchecked")
  @Override
  public Object getAdapter( final Class adapter )
  {
    if( adapter == GM_SurfacePatch[].class || adapter == GM_Polygon[].class || adapter == GM_Triangle[].class )
    {
      return m_items.toArray( new GM_Triangle[m_items.size()] );
    }

    // for points: get centroids of the triangles
    if( adapter == GM_Point[].class )
    {
      final List<GM_Point> pointList = new LinkedList<GM_Point>();

      final GM_Triangle[] triangles = m_items.toArray( new GM_Triangle[m_items.size()] );
      for( final GM_Triangle triangle : triangles )
      {
        pointList.add( triangle.getCentroid() );
      }
      return pointList.toArray( new GM_Point[pointList.size()] );
    }

    // NO: behaviour assymmetric to GM_Surface
    if( adapter == GM_Curve[].class )
    {
      try
      {
        final GM_Curve[] curves = new GM_Curve[m_items.size()];
        for( int i = 0; i < curves.length; i++ )
        {
          final GM_Position[] triangle = m_items.get( i ).getExteriorRing();
          curves[i] = GeometryFactory.createGM_Curve( triangle, getCoordinateSystem() );
        }

        return curves;
      }
      catch( final GM_Exception e )
      {
        final IStatus statusFromThrowable = StatusUtilities.statusFromThrowable( e );
        KalypsoDeegreePlugin.getDefault().getLog().log( statusFromThrowable );
      }
    }

    // NO: behaviour assymmetric to GM_Surface
    if( adapter == GM_Surface[].class )
    {
      try
      {
        final GM_Surface[] surfaces = new GM_Surface[m_items.size()];
        for( int i = 0; i < surfaces.length; i++ )
        {
          surfaces[i] = GeometryFactory.createGM_Surface( m_items.get( i ) );
        }

        return surfaces;
      }
      catch( final GM_Exception e )
      {
        final IStatus statusFromThrowable = StatusUtilities.statusFromThrowable( e );
        KalypsoDeegreePlugin.getDefault().getLog().log( statusFromThrowable );
      }
    }

    return super.getAdapter( adapter );
  }

  private void insertToIndex( final GM_Triangle triangle )
  {
    final Envelope env = JTSAdapter.export( triangle.getEnvelope() );
    m_index.insert( env, triangle );

    if( isValid() )
    {
      final GM_Envelope triangleEnv = triangle.getEnvelope();

      final GM_Envelope envelope = getEnvelope();
      if( envelope == null )
        setEnvelope( triangleEnv );
      else
        setEnvelope( envelope.getMerged( triangleEnv ) );
    }
  }

  private void removeTriangleFromIndex( final Object o )
  {
    if( o instanceof GM_Triangle )
    {
      final GM_Triangle gmTri = (GM_Triangle) o;
      final Envelope env = JTSAdapter.export( gmTri.getEnvelope() );
      m_index.remove( env, o );
    }
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_OrientableSurface#getSurfaceBoundary()
   */
  public GM_SurfaceBoundary getSurfaceBoundary( )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_GenericSurface#getArea()
   */
  public double getArea( )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_GenericSurface#getPerimeter()
   */
  public double getPerimeter( )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypsodeegree.model.geometry.ISurfacePatchVisitable#acceptSurfacePatches(org.kalypsodeegree.model.geometry.GM_Envelope,
   *      org.kalypsodeegree.model.geometry.ISurfacePatchVisitor)
   */
  public void acceptSurfacePatches( final GM_Envelope envToVisit, final ISurfacePatchVisitor<GM_Triangle> surfacePatchVisitor )
  {
    final ItemVisitor visitor = new ItemVisitor()
    {
      public void visitItem( final Object item )
      {
        final GM_Triangle t = (GM_Triangle) item;
        try
        {
          surfacePatchVisitor.visit( t, Double.NaN );
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }
      }
    };

    final Envelope searchEnv = JTSAdapter.export( envToVisit );
    m_index.query( searchEnv, visitor );
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_TriangulatedSurface#getValue(org.kalypsodeegree.model.geometry.GM_Position)
   */
  @SuppressWarnings("unchecked")
  public double getValue( final GM_Point location )
  {
    // TODO: transform to my own crs

    final GM_Position position = location.getPosition();
    return getValue( position );
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_TriangulatedSurface#getValue(org.kalypsodeegree.model.geometry.GM_Position)
   */
  public double getValue( final GM_Position position )
  {
    final Envelope searchEnv = new Envelope( position.getX(), position.getX(), position.getY(), position.getY() );
    final List<GM_Triangle> query = m_index.query( searchEnv );
    for( final GM_Triangle triangle : query )
    {
      if( triangle.contains( position ) )
        return triangle.getValue( position );
    }

    return Double.NaN;
  }

  public GM_Triangle getTriangle( final GM_Position position )
  {
    final Envelope searchEnv = new Envelope( position.getX(), position.getX(), position.getY(), position.getY() );
    final List<GM_Triangle> query = m_index.query( searchEnv );
    for( final GM_Triangle triangle : query )
    {
      if( triangle.contains( position ) )
        return triangle;
    }

    return null;
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Object#transform(org.deegree.crs.transformations.CRSTransformation,
   *      java.lang.String)
   */
  public GM_Object transform( CRSTransformation trans, String targetOGCCS ) throws Exception
  {
    /* If the target is the same coordinate system, do not transform. */
    String coordinateSystem = getCoordinateSystem();
    if( coordinateSystem == null || coordinateSystem.equalsIgnoreCase( targetOGCCS ) )
      return this;

    Debug.debugMethodBegin( this, "transformTriangulatedSurface" );

    final int cnt = size();
    final GM_Triangle[] triangles = new GM_Triangle[cnt];

    for( int i = 0; i < cnt; i++ )
    {
      triangles[i] = (GM_Triangle) get( i ).transform( trans, targetOGCCS );
    }

    Debug.debugMethodEnd();
    return GeometryFactory.createGM_TriangulatedSurface( triangles, targetOGCCS );

  }
}
