package org.deegree_impl.model.sort;

import java.awt.Graphics;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import org.deegree.graphics.displayelements.DisplayElement;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureList;
import org.deegree.model.feature.FeatureVisitor;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Position;
import org.deegree_impl.model.geometry.GeometryFactory;

public class SplitSort implements FeatureList
{
  public static boolean showIndexEnv = false;

  private SplitSortContainer myRootContainer = null;

  private List m_objects = new ArrayList();

  public SplitSort()
  {
    //
  }
  
  public SplitSort( final GM_Envelope env )
  {
    myRootContainer = new SplitSortContainer( null, env );
  }

  public boolean add( final Object object )
  {
    final GM_Envelope env = getEnvelope( object );
    add( env, object );

    return m_objects.add( object );
  }

  private void add( final GM_Envelope env, final Object object )
  {
    if( env == null )
      return;

    if( myRootContainer == null )
      myRootContainer = new SplitSortContainer( null, env );

    if( myRootContainer.getEnvelope().contains( env ) )
      myRootContainer.add( env, object );
    else
    {
      double maxX = env.getMax().getX();
      double maxY = env.getMax().getY();
      double minX = env.getMin().getX();
      double minY = env.getMin().getY();

      GM_Envelope envRoot = myRootContainer.getEnvelope();

      double maxXroot = envRoot.getMax().getX();
      double maxYroot = envRoot.getMax().getY();
      double minXroot = envRoot.getMin().getX();
      double minYroot = envRoot.getMin().getY();
      GM_Envelope newEnv = GeometryFactory.createGM_Envelope( minX < minXroot ? minX : minXroot,
          minY < minYroot ? minY : minYroot, maxX > maxXroot ? maxX : maxXroot,
          maxY > maxYroot ? maxY : maxYroot );

      SplitSortContainer newRootContainer = new SplitSortContainer( null, newEnv );
      myRootContainer.setParent( newRootContainer );
      newRootContainer.createSubContainers( myRootContainer );
      myRootContainer = newRootContainer;
      myRootContainer.add( env, object );
    }
  }

  public List query( GM_Envelope queryEnv, List result )
  {
    if( myRootContainer != null )
      myRootContainer.query( queryEnv, result );
    return result;
  }

  public List query( GM_Position pos, List result )
  {
    return query( GeometryFactory.createGM_Envelope( pos, pos ), result );
  }

  public List queryAll( List result )
  {
    if( result == null )
      result = new ArrayList();

    result.addAll( m_objects );
    return result;
  }

  private void remove( GM_Envelope env, Object object )
  {
    if( myRootContainer != null )
    {
      if( env != null )
        myRootContainer.remove( env, object );
      else
        myRootContainer.remove( object );
    }
  }

  public boolean remove( Object object )
  {
    GM_Envelope env = getEnvelope( object );
    if( env != null )
      remove( env, object );

    return m_objects.remove( object );
  }

  protected static GM_Envelope getEnvelope( Object object )
  {
    if( object instanceof DisplayContext )
    {
      return getEnvelope( ( (DisplayContext)object ).getFeature() );
    }
    if( object instanceof DisplayElement )
    {
      DisplayElement de = (DisplayElement)object;
      return getEnvelope( de.getFeature() );
    }
    else if( object instanceof Feature )
    {
      Feature fe = (Feature)object;
      GM_Object gmObject = fe.getDefaultGeometryProperty();
      if( gmObject == null )
        return null;
      GM_Envelope env = gmObject.getEnvelope();
      if( env == null )
      {
        GM_Position gmPos = fe.getDefaultGeometryProperty().getCentroid().getPosition();
        env = GeometryFactory.createGM_Envelope( gmPos, gmPos );
      }
      return env;
    }
    else
    {
      return null;
    }
  }

  public void paint( Graphics g, GeoTransform geoTransform )
  {
    if( myRootContainer != null )
      myRootContainer.paint( g, geoTransform );
  }

  public int rsize()
  {
    if( myRootContainer != null )
      return myRootContainer.rsize();
    return 0;
  }

  public GM_Envelope getBoundingBox()
  {
    // TODO es muss die boundingbox aus den Objecten innerhalb des
    // rootcontainers gebildet werden
    if( myRootContainer != null )
      return myRootContainer.getEnvelope();
    return null;
  }

  public void resort()
  {
    myRootContainer = null;

    GM_Envelope bbox = null;
    for( final Iterator iter = m_objects.iterator(); iter.hasNext(); )
    {
      final Feature f = (Feature)iter.next();

      final GM_Envelope envelope = f.getEnvelope();
      if( bbox == null )
        bbox = envelope;
      else
        bbox = bbox.merge( envelope );
    }
    
    myRootContainer = new SplitSortContainer( null, bbox );
    for( final Iterator iter = m_objects.iterator(); iter.hasNext(); )
    {
      final Object next = iter.next();
      add( getEnvelope( next ), next );
    }
  }

  /**
   * @see java.util.List#size()
   */
  public int size()
  {
    return m_objects.size();
  }

  /**
   * @see java.util.List#clear()
   */
  public void clear()
  {
    // TODO: dispose it?
    myRootContainer = null;

    m_objects.clear();
  }

  /**
   * @see java.util.List#isEmpty()
   */
  public boolean isEmpty()
  {
    return m_objects.isEmpty();
  }

  /**
   * @see java.util.List#toArray()
   */
  public Object[] toArray()
  {
    return m_objects.toArray();
  }

  /**
   * @see java.util.List#get(int)
   */
  public Object get( int index )
  {
    return m_objects.get( index );
  }

  /**
   * @see java.util.List#remove(int)
   */
  public Object remove( int index )
  {
    final Object object = get( index );
    remove( object );
    return object;
  }

  /**
   * @see java.util.List#add(int, java.lang.Object)
   */
  public void add( int index, Object element )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.List#indexOf(java.lang.Object)
   */
  public int indexOf( Object o )
  {
    return m_objects.indexOf( o );
  }

  /**
   * @see java.util.List#lastIndexOf(java.lang.Object)
   */
  public int lastIndexOf( Object o )
  {
    return m_objects.lastIndexOf( o );
  }

  /**
   * @see java.util.List#contains(java.lang.Object)
   */
  public boolean contains( Object o )
  {
    return m_objects.contains( o );
  }

  /**
   * @see java.util.List#addAll(int, java.util.Collection)
   */
  public boolean addAll( int index, Collection c )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.List#addAll(java.util.Collection)
   */
  public boolean addAll( Collection c )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.List#containsAll(java.util.Collection)
   */
  public boolean containsAll( Collection c )
  {
    return m_objects.containsAll( c );
  }

  /**
   * @see java.util.List#removeAll(java.util.Collection)
   */
  public boolean removeAll( Collection c )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.List#retainAll(java.util.Collection)
   */
  public boolean retainAll( Collection c )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.List#iterator()
   */
  public Iterator iterator()
  {
    return m_objects.iterator();
  }

  /**
   * @see java.util.List#subList(int, int)
   */
  public List subList( int fromIndex, int toIndex )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.List#listIterator()
   */
  public ListIterator listIterator()
  {
    return m_objects.listIterator();
  }

  /**
   * @see java.util.List#listIterator(int)
   */
  public ListIterator listIterator( int index )
  {
    return m_objects.listIterator( index );
  }

  /**
   * @see java.util.List#set(int, java.lang.Object)
   */
  public Object set( int index, Object element )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.List#toArray(java.lang.Object[])
   */
  public Object[] toArray( Object[] a )
  {
    return m_objects.toArray( a );
  }

  /**
   * @see org.deegree.model.feature.FeatureList#toFeatures()
   */
  public Feature[] toFeatures()
  {
    return (Feature[])m_objects.toArray( new Feature[m_objects.size()] );
  }

  /**
   * @see org.deegree.model.feature.FeatureList#accept(org.deegree.model.feature.FeatureVisitor)
   */
  public void accept( final FeatureVisitor visitor )
  {
    for( final Iterator iter = m_objects.iterator(); iter.hasNext(); )
    {
      final Object object = iter.next();
      if( object instanceof Feature )
        visitor.visit( (Feature)object );
    }
  }
}