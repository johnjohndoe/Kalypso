package org.kalypsodeegree_impl.model.sort;

import java.awt.Graphics;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.visitors.CollectorVisitor;
import org.kalypsodeegree_impl.model.feature.visitors.FeatureTypeVisitor;

/**
 * Eine gefilterte FeatureListe.
 * 
 * Die Liste zeigt nach aussen nur die Features, die einem bestimmten FeatureType entsprechen. Andererseits ist die
 * Liste aber durch die originale Liste gebackupd, d.h. alle �nderungen dieser Liste �ndern auch die Originalliste.
 * 
 * @author belger
 */
public class FilteredFeatureList implements FeatureList
{
  private final FeatureList m_original;

  private final FeatureTypeVisitor m_filterVisitor;

  private final CollectorVisitor m_collector = new CollectorVisitor();

  public FilteredFeatureList( final FeatureList original, final String typename, final boolean acceptIfSubstituting )
  {
    m_original = original;
    m_filterVisitor = new FeatureTypeVisitor( null, typename, acceptIfSubstituting );
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureList#toFeatures()
   */
  public Feature[] toFeatures()
  {
    m_filterVisitor.setVisitor( m_collector );
    m_original.accept( m_filterVisitor );
    return m_collector.getResults( true );
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureList#accept(org.kalypsodeegree.model.feature.FeatureVisitor)
   */
  public void accept( final FeatureVisitor visitor )
  {
    m_filterVisitor.setVisitor( visitor );
    m_original.accept( m_filterVisitor );
    m_filterVisitor.setVisitor( null );
  }

  /**
   * @see java.util.Collection#size()
   */
  public int size()
  {
    return toFeatures().length;
  }

  /**
   * @see java.util.Collection#clear()
   */
  public void clear()
  {
    final Feature[] features = toFeatures();
    for( int i = 0; i < features.length; i++ )
      m_original.remove( features[i] );
  }

  /**
   * @see java.util.Collection#isEmpty()
   */
  public boolean isEmpty()
  {
    return toFeatures().length == 0;
  }

  /**
   * @see java.util.Collection#toArray()
   */
  public Object[] toArray()
  {
    return toFeatures();
  }

  /**
   * @see java.util.List#get(int)
   */
  public Object get( int index )
  {
    return toFeatures()[index];
  }

  /**
   * @see java.util.List#remove(int)
   */
  public Object remove( int index )
  {
    final Object object = get( index );
    if( m_original.remove( object ) )
      return object;

    return null;
  }

  /**
   * @see java.util.List#add(int, java.lang.Object)
   */
  public void add( int index, Object element )
  {
    // geht nicht?
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.List#indexOf(java.lang.Object)
   */
  public int indexOf( Object o )
  {
    final Object[] objects = toArray();
    for( int i = 0; i < objects.length; i++ )
    {
      if( o == objects[i] )
        return i;
    }
    return -1;
  }

  /**
   * @see java.util.List#lastIndexOf(java.lang.Object)
   */
  public int lastIndexOf( Object o )
  {
    final Object[] objects = toArray();
    for( int i = objects.length - 1; i > -1; i-- )
    {
      if( o == objects[i] )
        return i;
    }

    return -1;
  }

  /**
   * @see java.util.Collection#add(java.lang.Object)
   */
  public boolean add( final Object o )
  {
    if( !m_filterVisitor.matchesType( (Feature)o ) )
      throw new IllegalArgumentException();

    return m_original.add( o );
  }

  /**
   * @see java.util.Collection#contains(java.lang.Object)
   */
  public boolean contains( Object o )
  {
    return m_original.contains( o );
  }

  /**
   * @see java.util.Collection#remove(java.lang.Object)
   */
  public boolean remove( final Object o )
  {
    if( m_filterVisitor.matchesType( (Feature)o ) )
      return m_original.remove( o );

    return false;
  }

  /**
   * @see java.util.List#addAll(int, java.util.Collection)
   */
  public boolean addAll( int index, Collection c )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.Collection#addAll(java.util.Collection)
   */
  public boolean addAll( final Collection c )
  {
    for( Iterator cIt = c.iterator(); cIt.hasNext(); )
      add( cIt.next() );

    return !c.isEmpty();
  }

  /**
   * @see java.util.Collection#containsAll(java.util.Collection)
   */
  public boolean containsAll( final Collection c )
  {
    for( Iterator cIt = c.iterator(); cIt.hasNext(); )
    {
      final Object f = cIt.next();
      if( !contains( f ) )
        return false;
    }

    return true;
  }

  /**
   * @see java.util.Collection#removeAll(java.util.Collection)
   */
  public boolean removeAll( final Collection c )
  {
    for( Iterator cIt = c.iterator(); cIt.hasNext(); )
    {
      final Object f = cIt.next();
      remove( f );
    }

    return !c.isEmpty();
  }

  /**
   * @see java.util.Collection#retainAll(java.util.Collection)
   */
  public boolean retainAll( final Collection c )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.Collection#iterator()
   */
  public Iterator iterator()
  {
    return listIterator();
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
    return listIterator( 0 );
  }

  /**
   * @see java.util.List#listIterator(int)
   */
  public ListIterator listIterator( final int index )
  {
    if( index < 0 || index > size() )
      throw new IndexOutOfBoundsException();

    return new ArrayIterator( index, toFeatures() );
  }

  /**
   * @see java.util.List#set(int, java.lang.Object)
   */
  public Object set( int index, Object element )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.Collection#toArray(java.lang.Object[])
   */
  public Object[] toArray( Object[] a )
  {
    final Feature[] toFeatures = toFeatures();
    if( a == null || a.length < toFeatures.length )
      return toFeatures;
    
    System.arraycopy( toFeatures, 0, a, 0, toFeatures.length );    
    
    return a;
  }

  /**
   * @see org.kalypsodeegree.model.sort.JMSpatialIndex#query(org.kalypsodeegree.model.geometry.GM_Envelope,
   *      java.util.List)
   */
  public List query( final GM_Envelope env, final List result )
  {
    return filterList( m_original.query( env, result ), result );
  }

  private List filterList( final List originalList, final List result )
  {
    final int oldlength = result == null ? 0 : result.size();

    // only remove new elements, which do not match type
    final List sublist = originalList.subList( oldlength, originalList.size() );
    for( Iterator sIt = sublist.iterator(); sIt.hasNext(); )
    {
      final Feature f = (Feature)sIt.next();
      if( !m_filterVisitor.matchesType( f ) )
        sIt.remove();
    }

    return originalList;
  }

  /**
   * @see org.kalypsodeegree.model.sort.JMSpatialIndex#query(org.kalypsodeegree.model.geometry.GM_Position,
   *      java.util.List)
   */
  public List query( final GM_Position env, final List result )
  {
    return filterList( m_original.query( env, result ), result );
  }

  /**
   * @see org.kalypsodeegree.model.sort.JMSpatialIndex#queryAll(java.util.List)
   */
  public List queryAll( final List result )
  {
    return filterList( m_original.queryAll( result ), result );
  }

  /**
   * @see org.kalypsodeegree.model.sort.JMSpatialIndex#resort()
   */
  public void resort()
  {
    m_original.resort();
  }

  /**
   * @see org.kalypsodeegree.model.sort.JMSpatialIndex#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform)
   */
  public void paint( final Graphics g, final GeoTransform geoTransform )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypsodeegree.model.sort.JMSpatialIndex#rsize()
   */
  public int rsize()
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypsodeegree.model.sort.JMSpatialIndex#getBoundingBox()
   */
  public GM_Envelope getBoundingBox()
  {
    // zu gross!
    return m_original.getBoundingBox();
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureList#getParentFeature()
   */
  public Feature getParentFeature()
  {
    return m_original.getParentFeature();
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureList#getParentFeatureTypeProperty()
   */
  public FeatureTypeProperty getParentFeatureTypeProperty()
  {

    return m_original.getParentFeatureTypeProperty();
  }
}
