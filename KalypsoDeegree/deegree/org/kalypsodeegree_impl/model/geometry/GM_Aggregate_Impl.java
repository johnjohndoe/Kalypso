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
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.kalypsodeegree.model.geometry.GM_Aggregate;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.tools.Debug;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * default implementierung of the GM_Aggregate interface ------------------------------------------------------------
 * 
 * @version 8.6.2001
 * @author Andreas Poth href="mailto:poth@lat-lon.de"
 */
abstract class GM_Aggregate_Impl extends GM_Object_Impl implements GM_Aggregate, Serializable
{
  /** Use serialVersionUID for interoperability. */
  private final static long serialVersionUID = 1161164609227432958L;

  protected List<GM_Object> m_aggregate = new ArrayList<GM_Object>( 500 );

  /**
   * Creates a new GM_Aggregate_Impl object.
   * 
   * @param crs
   */
  public GM_Aggregate_Impl( final CS_CoordinateSystem crs )
  {
    super( crs );
  }

  /**
   * returns the number of GM_Object within the aggregation
   */
  public int getSize( )
  {
    return m_aggregate.size();
  }

  /**
   * merges this aggregation with another one
   * 
   * @exception GM_Exception
   *                a GM_Exception will be thrown if the submitted isn't the same type as the recieving one.
   */
  public void merge( final GM_Aggregate aggregate ) throws GM_Exception
  {
    if( !this.getClass().getName().equals( aggregate.getClass().getName() ) )
    {
      throw new GM_Exception( "Aggregations are not of the same type!" );
    }

    for( int i = 0; i < this.getSize(); i++ )
    {
      this.add( aggregate.getObjectAt( i ) );
    }

    setValid( false );
  }

  /**
   * adds an GM_Object to the aggregation
   */
  public void add( final GM_Object gmo )
  {
    m_aggregate.add( gmo );

    setValid( false );
  }

  /**
   * inserts a GM_Object in the aggregation. all elements with an index equal or larger index will be moved. if index is
   * larger then getSize() - 1 or smaller then 0 or gmo equals null an exception will be thrown.
   * 
   * @param gmo
   *            GM_Object to insert.
   * @param index
   *            position where to insert the new GM_Object
   */
  public void insertObjectAt( final GM_Object gmo, final int index ) throws GM_Exception
  {
    if( (index < 0) || (index > this.getSize() - 1) )
    {
      throw new GM_Exception( "invalid index/position: " + index + " to insert a geometry!" );
    }

    if( gmo == null )
    {
      throw new GM_Exception( "gmo == null. it isn't possible to insert a value" + " that equals null!" );
    }

    m_aggregate.add( index, gmo );

    setValid( false );
  }

  /**
   * sets the submitted GM_Object at the submitted index. the element at the position <code>index</code> will be
   * removed. if index is larger then getSize() - 1 or smaller then 0 or gmo equals null an exception will be thrown.
   * 
   * @param gmo
   *            GM_Object to set.
   * @param index
   *            position where to set the new GM_Object
   */
  public void setObjectAt( final GM_Object gmo, final int index ) throws GM_Exception
  {
    if( (index < 0) || (index > this.getSize() - 1) )
    {
      throw new GM_Exception( "invalid index/position: " + index + " to set a geometry!" );
    }

    if( gmo == null )
    {
      throw new GM_Exception( "gmo == null. it isn't possible to set a value" + " that equals null!" );
    }

    m_aggregate.set( index, gmo );

    setValid( false );
  }

  /**
   * removes the submitted GM_Object from the aggregation
   * 
   * @return the removed GM_Object
   */
  public GM_Object removeObject( final GM_Object gmo )
  {
    if( gmo == null )
    {
      return null;
    }

    final int i = m_aggregate.indexOf( gmo );

    GM_Object gmo_ = null;

    try
    {
      gmo_ = removeObjectAt( i );
    }
    catch( final GM_Exception e )
    {
      Debug.debugException( e, "" );
    }

    setValid( false );

    return gmo_;
  }

  /**
   * removes the GM_Object at the submitted index from the aggregation. if index is larger then getSize() - 1 or smaller
   * then 0 an exception will be thrown.
   * 
   * @return the removed GM_Object
   */
  public GM_Object removeObjectAt( final int index ) throws GM_Exception
  {
    if( index < 0 )
    {
      return null;
    }

    if( index > (this.getSize() - 1) )
    {
      throw new GM_Exception( "invalid index/position: " + index + " to remove a geometry!" );
    }

    final GM_Object gmo = m_aggregate.remove( index );

    setValid( false );

    return gmo;
  }

  /**
   * removes all GM_Object from the aggregation.
   */
  public void removeAll( )
  {
    m_aggregate.clear();
    setEnvelope( null );
    setValid( false );
  }

  /**
   * returns the GM_Object at the submitted index. if index is larger then getSize() - 1 or smaller then 0 an exception
   * will be thrown.
   */
  public GM_Object getObjectAt( final int index )
  {
    return m_aggregate.get( index );
  }

  /**
   * returns all GM_Objects as array
   */
  public GM_Object[] getAll( )
  {
    final GM_Object[] gmos = new GM_Object[this.getSize()];

    return m_aggregate.toArray( gmos );
  }

  /**
   * returns true if the submitted GM_Object is within the aggregation
   */
  public boolean isMember( final GM_Object gmo )
  {
    return m_aggregate.contains( gmo );
  }

  /**
   * returns the aggregation as an iterator
   */
  public Iterator<GM_Object> getIterator( )
  {
    return m_aggregate.iterator();
  }

  /**
   * returns true if no geometry stored within the collection.
   */
  @Override
  public boolean isEmpty( )
  {
    return (getSize() == 0);
  }

  /**
   * sets the spatial reference system
   * 
   * @param crs
   *            new spatial reference system
   */
  @Override
  public void setCoordinateSystem( final CS_CoordinateSystem crs )
  {
    super.setCoordinateSystem( crs );

    if( m_aggregate != null )
    {
      for( int i = 0; i < m_aggregate.size(); i++ )
      {
        ((GM_Object_Impl) getObjectAt( i )).setCoordinateSystem( crs );
      }
      setValid( false );
    }
  }

  /**
   * translate the point by the submitted values. the <code>dz</code>- value will be ignored.
   */
  @Override
  public void translate( final double[] d )
  {
    try
    {
      for( int i = 0; i < getSize(); i++ )
      {
        final GM_Object gmo = getObjectAt( i );
        gmo.translate( d );
      }
      setValid( false );
    }
    catch( final Exception e )
    {
      Debug.debugException( e, "" );
    }
    setValid( false );
  }

  @Override
  public boolean equals( final Object other )
  {
    // envelope was not valid
    if( !super.equals( other ) || !(other instanceof GM_Aggregate_Impl) || !getEnvelope().equals( ((GM_Object) other).getEnvelope() ) || (getSize() != ((GM_Aggregate) other).getSize()) )
    {
      return false;
    }

    try
    {
      for( int i = 0; i < getSize(); i++ )
      {
        final Object o1 = getObjectAt( i );
        final Object o2 = ((GM_Aggregate) other).getObjectAt( i );

        if( !o1.equals( o2 ) )
        {
          return false;
        }
      }
    }
    catch( final Exception ex )
    {
      return false;
    }

    return true;
  }

  /**
   * The Boolean valued operation "intersects" shall return TRUE if this GM_Object intersects another GM_Object. Within
   * a GM_Complex, the GM_Primitives do not intersect one another. In general, topologically structured data uses shared
   * geometric objects to capture intersection information.
   */
  @Override
  public boolean intersects( final GM_Object gmo )
  {
    boolean inter = false;

    try
    {
      for( int i = 0; i < m_aggregate.size(); i++ )
      {
        if( this.getObjectAt( i ).intersects( gmo ) )
        {
          inter = true;
          break;
        }
      }
    }
    catch( final Exception e )
    {
    }

    return inter;
  }

  @Override
  public String toString( )
  {
    String ret = null;
    ret = "aggregate = " + m_aggregate + "\n";
    ret += ("envelope = " + getEnvelope() + "\n");
    return ret;
  }

  /**
   * @see org.kalypsodeegree_impl.model.geometry.GM_Object_Impl#invalidate()
   */
  @Override
  public void invalidate( )
  {
    for( final GM_Object gmobj : m_aggregate )
      gmobj.invalidate();

    super.invalidate();
  }

  /**
   * @see org.kalypsodeegree_impl.model.geometry.GM_Object_Impl#getAdapter(java.lang.Class)
   */
  @SuppressWarnings("unchecked")
  @Override
  public Object getAdapter( final Class adapter )
  {
    /* An array of GM_xxx adapts to the array of its adapters. */
    final Class< ? > componentType = adapter.getComponentType();
    if( componentType != null && GM_Object.class.isAssignableFrom( componentType ) )
    {
      final List<GM_Object> adaptedObjects = new ArrayList<GM_Object>();

      for( final GM_Object objectToAdapt : m_aggregate )
      {
        final GM_Object adaptedObject = (GM_Object) objectToAdapt.getAdapter( componentType );
        if( adaptedObject != null && componentType.isAssignableFrom( adaptedObject.getClass() ) )
          adaptedObjects.add( adaptedObject );
      }

      final Object adaptedArray = Array.newInstance( componentType, adaptedObjects.size() );
      return adaptedObjects.toArray( (GM_Object[]) adaptedArray );
    }

    return super.getAdapter( adapter );
  }
}