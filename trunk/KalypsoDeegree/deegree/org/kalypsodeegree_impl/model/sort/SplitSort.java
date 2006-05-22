package org.kalypsodeegree_impl.model.sort;

import java.awt.Graphics;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

public class SplitSort implements FeatureList
{
  public static boolean showIndexEnv = false;

  private SplitSortContainer m_rootContainer = null;

  private List m_objects = new ArrayList();

  private final Feature m_parentFeature;

  private final IRelationType m_parentFeatureTypeProperty;

  public SplitSort( Feature parentFeature, IRelationType parentFTP )
  {
    m_parentFeature = parentFeature;
    m_parentFeatureTypeProperty = parentFTP;
  }

  public SplitSort( final Feature parentFeature, IRelationType parentFTP, final GM_Envelope env )
  {
    m_parentFeature = parentFeature;
    m_parentFeatureTypeProperty = parentFTP;
    m_rootContainer = new SplitSortContainer( null, env );
  }

  public boolean add( final Object object )
  {
    final GM_Envelope env = getEnvelope( object );
    spacialAdd( env, object );
    return m_objects.add( object );
  }

  private void spacialAdd( final GM_Envelope env, final Object object )
  {
    if( env == null )
      return;

    if( m_rootContainer == null )
      m_rootContainer = new SplitSortContainer( null, env );

    if( m_rootContainer.getEnvelope().contains( env ) )
      m_rootContainer.add( env, object );
    else
    {
      double maxX = env.getMax().getX();
      double maxY = env.getMax().getY();
      double minX = env.getMin().getX();
      double minY = env.getMin().getY();

      GM_Envelope envRoot = m_rootContainer.getEnvelope();

      double maxXroot = envRoot.getMax().getX();
      double maxYroot = envRoot.getMax().getY();
      double minXroot = envRoot.getMin().getX();
      double minYroot = envRoot.getMin().getY();
      GM_Envelope newEnv = GeometryFactory.createGM_Envelope( minX < minXroot ? minX : minXroot, minY < minYroot ? minY : minYroot, maxX > maxXroot ? maxX : maxXroot, maxY > maxYroot ? maxY
          : maxYroot );

      SplitSortContainer newRootContainer = new SplitSortContainer( null, newEnv );
      m_rootContainer.setParent( newRootContainer );
      newRootContainer.createSubContainers( m_rootContainer );
      m_rootContainer = newRootContainer;
      m_rootContainer.add( env, object );
    }
  }

  public List query( GM_Envelope queryEnv, List result )
  {
    if( result == null )
      result = new ArrayList();
    if( m_rootContainer != null )
      result = m_rootContainer.query( queryEnv, result );
    return result;
  }

  public List query( GM_Position pos, List result )
  {
    if( result == null )
      result = new ArrayList();
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
    if( m_rootContainer != null )
    {
      if( env != null )
        m_rootContainer.remove( env, object );
      else
        m_rootContainer.remove( object );
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
    if( object instanceof DisplayElement )
    {
      DisplayElement de = (DisplayElement) object;
      return getEnvelope( de.getFeature() );
    }
    else if( object instanceof Feature )
    {
      Feature fe = (Feature) object;
      GM_Envelope env = fe.getEnvelope();
      return env;
    }
    else
    {
      return null;
    }
  }

  public void paint( Graphics g, GeoTransform geoTransform )
  {
    if( m_rootContainer != null )
      m_rootContainer.paint( g, geoTransform );
  }

  public int rsize( )
  {
    if( m_rootContainer != null )
      return m_rootContainer.rsize();
    return 0;
  }

  public GM_Envelope getBoundingBox( )
  {
    // TODO es muss die boundingbox aus den Objecten innerhalb des
    // rootcontainers gebildet werden
    if( m_rootContainer != null )
      return m_rootContainer.getEnvelope();
    return null;
  }

  public void resort( Feature feature )
  {
    if( !contains( feature ) )
      return;
    if( m_rootContainer != null )
      m_rootContainer.remove( feature );
    final GM_Envelope envelope = getEnvelope( feature );
    if( envelope == null )
      System.out.println( "no envelope !" );
    spacialAdd( envelope, feature );
  }

  public void resort( )
  {
    m_rootContainer = null;

    GM_Envelope bbox = null;

    for( final Iterator iter = m_objects.iterator(); iter.hasNext(); )
    {
      final Object f = iter.next();

      final GM_Envelope envelope = getEnvelope( f );
      if( bbox == null )
        bbox = envelope;
      else
        bbox = bbox.getMerged( envelope );
    }
    if( bbox != null )
    {
      m_rootContainer = new SplitSortContainer( null, bbox );
      for( final Iterator iter = m_objects.iterator(); iter.hasNext(); )
      {
        final Object next = iter.next();
        GM_Envelope envelope = getEnvelope( next );
        spacialAdd( envelope, next );
      }
    }
    else
      m_rootContainer = null;
  }

  /**
   * @see java.util.List#size()
   */
  public int size( )
  {
    return m_objects.size();
  }

  /**
   * @see java.util.List#clear()
   */
  public void clear( )
  {
    // TODO: dispose it?
    m_rootContainer = null;

    m_objects.clear();
  }

  /**
   * @see java.util.List#isEmpty()
   */
  public boolean isEmpty( )
  {
    return m_objects.isEmpty();
  }

  /**
   * @see java.util.List#toArray()
   */
  public Object[] toArray( )
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
  public void add( int index, Object object )
  {
    final GM_Envelope env = getEnvelope( object );
    spacialAdd( env, object );
    m_objects.add( index, object );
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
    boolean changed = false;
    for( final Iterator iter = c.iterator(); iter.hasNext(); )
    {
      add( iter.next() );

      changed = true;
    }

    return changed;
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
  public Iterator iterator( )
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
  public ListIterator listIterator( )
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
   * @deprecated use toArray() cause in a splitsort can be also featureIds (String), if feature is linked from the list
   * @see org.kalypsodeegree.model.feature.FeatureList#toFeatures()
   */
  public Feature[] toFeatures( )
  {

    return (Feature[]) m_objects.toArray( new Feature[m_objects.size()] );
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureList#accept(org.kalypsodeegree.model.feature.FeatureVisitor)
   */
  public void accept( final FeatureVisitor visitor )
  {
    for( final Iterator iter = m_objects.iterator(); iter.hasNext(); )
    {
      final Object object = iter.next();
      if( object instanceof Feature )
        visitor.visit( (Feature) object );
    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureList#getParentFeature()
   */
  public Feature getParentFeature( )
  {
    return m_parentFeature;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureList#getParentFeatureTypeProperty()
   */
  public IRelationType getParentFeatureTypeProperty( )
  {
    return m_parentFeatureTypeProperty;
  }
}