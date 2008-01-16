package org.kalypsodeegree_impl.model.sort;

import java.awt.Graphics;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;

import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Envelope;

public class SplitSort implements FeatureList
{
  private final IEnvelopeProvider DEFAULT_ENV_PROVIDER = new IEnvelopeProvider()
  {
    public GM_Envelope getEnvelope( final Object object )
    {
      if( object instanceof DisplayElement )
      {
        final DisplayElement de = (DisplayElement) object;
        return getEnvelope( de.getFeature() );
      }
      else if( object instanceof Feature )
      {
        final Feature fe = (Feature) object;

        // HACK: if the workspace is null, we are probably still loading
        // so do not access the envelope, which may cause problems now
        final GMLWorkspace workspace = fe.getWorkspace();
        if( workspace == null )
          return null;

        return fe.getEnvelope();
      }
      else if( object instanceof String )
      {
        final GMLWorkspace workspace = getParentFeature().getWorkspace();
        final Feature fe = workspace == null ? null : workspace.getFeature( (String) object );
        if( fe != null )
          return fe.getEnvelope();
      }

      return null;
    }
  };

  /**
   * Used to synchronize access to m_items and m_index
   */
  private final Object m_lock = new Object();

  private SpatialIndexExt m_index;

  private final List<Object> m_items = new ArrayList<Object>();

  private final Feature m_parentFeature;

  private final IRelationType m_parentFeatureTypeProperty;

  private final IEnvelopeProvider m_envelopeProvider;

  public SplitSort( final Feature parentFeature, final IRelationType parentFTP )
  {
    this( parentFeature, parentFTP, (IEnvelopeProvider) null );
  }

  public SplitSort( final Feature parentFeature, final IRelationType parentFTP, final IEnvelopeProvider envelopeProvider )
  {
    this( parentFeature, parentFTP, null, envelopeProvider );
  }

  public SplitSort( final Feature parentFeature, final IRelationType parentFTP, final GM_Envelope env )
  {
    this( parentFeature, parentFTP, env, null );
  }

  public SplitSort( final Feature parentFeature, final IRelationType parentFTP, final GM_Envelope env, final IEnvelopeProvider envelopeProvider )
  {
    m_parentFeature = parentFeature;
    m_parentFeatureTypeProperty = parentFTP;
    m_envelopeProvider = envelopeProvider == null ? DEFAULT_ENV_PROVIDER : envelopeProvider;

    // Index is initially index. This is necessary, as loading the features ads them to this list,
    // but the features often have no envelope; so we rather wait for the first query.
    m_index = null;
  }

  private SpatialIndexExt createIndex( final Envelope env )
  {
    return new SplitSortSpatialIndex( env );
    // m_index = new QuadTreeIndex( env );
  }

  /**
   * Recreate the index, if it is <code>null</code>.
   */
  private void checkIndex( )
  {
    if( m_index == null )
    {
      // Recalculate the bounding box
      Envelope bbox = null;
      for( final Object item : m_items )
      {
        final Envelope env = getEnvelope( item );
        if( env != null )
        {
          if( bbox == null )
            bbox = new Envelope( env );
          else
            bbox.expandToInclude( env );
        }
      }

      // create index
      m_index = createIndex( bbox );

      // insert all elements
      for( final Object item : m_items )
      {
        final Envelope env = getEnvelope( item );
        m_index.insert( env, item );
      }
    }
  }

  /**
   * @see java.util.List#add(java.lang.Object)
   */
  public boolean add( final Object object )
  {
    final Envelope env = getEnvelope( object );

    synchronized( m_lock )
    {
      if( m_index != null )
        m_index.insert( env, object );
      return m_items.add( object );
    }
  }

  /**
   * @see org.kalypsodeegree.model.sort.JMSpatialIndex#query(org.kalypsodeegree.model.geometry.GM_Envelope,
   *      java.util.List)
   */
  public List query( final GM_Envelope queryEnv, List result )
  {
    checkIndex();

    synchronized( m_lock )
    {
      if( result == null )
        result = new ArrayList();
      final Envelope env = JTSAdapter.export( queryEnv );
      final List list = m_index.query( env );
      for( final Object object : list )
        result.add( object );
      return result;
    }
  }

  /**
   * @see org.kalypsodeegree.model.sort.JMSpatialIndex#query(org.kalypsodeegree.model.geometry.GM_Position,
   *      java.util.List)
   */
  public List query( final GM_Position pos, List result )
  {
    if( result == null )
      result = new ArrayList();
    return query( GeometryFactory.createGM_Envelope( pos, pos ), result );
  }

  /**
   * @deprecated This is slow: TODO: better comment TODO: deprecate in parent interface
   * @see java.util.List#remove(java.lang.Object)
   */
  @Deprecated
  public boolean remove( final Object object )
  {
    final Envelope env = getEnvelope( object );

    synchronized( m_lock )
    {
      // TODO: slow!
      final boolean removed = m_items.remove( object );
      if( m_index != null )
        m_index.remove( env, object );
      return removed;
    }
  }

  private Envelope getEnvelope( final Object object )
  {
    final GM_Envelope envelope = m_envelopeProvider.getEnvelope( object );
    return JTSAdapter.export( envelope );
  }

  /**
   * @see org.kalypsodeegree.model.sort.JMSpatialIndex#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform)
   */
  public void paint( final Graphics g, final GeoTransform geoTransform )
  {
    checkIndex();

    synchronized( m_lock )
    {
      m_index.paint( g, geoTransform );
    }
  }

  /**
   * @see org.kalypsodeegree.model.sort.JMSpatialIndex#getBoundingBox()
   */
  // TODO: slow; check if we can improve it by maintaining the bbox in this class
  public GM_Envelope getBoundingBox( )
  {
    checkIndex();

    synchronized( m_lock )
    {
      final Envelope bbox = m_index.getBoundingBox();
      if( bbox == null )
        return null;
      return JTSAdapter.wrap( bbox );
    }
  }

  /**
   * @see java.util.List#size()
   */
  public int size( )
  {
    synchronized( m_lock )
    {
      return m_items.size();
    }
  }

  /**
   * @see java.util.List#clear()
   */
  public void clear( )
  {
    synchronized( m_lock )
    {
      m_items.clear();
      m_index = null;
    }
  }

  /**
   * @see java.util.List#isEmpty()
   */
  public boolean isEmpty( )
  {
    return size() == 0;
  }

  /**
   * @see java.util.List#toArray()
   */
  public Object[] toArray( )
  {
    synchronized( m_lock )
    {
      return m_items.toArray( new Object[m_items.size()] );
    }
  }

  /**
   * @see java.util.List#get(int)
   */
  public Object get( final int index )
  {
    synchronized( m_lock )
    {
      return m_items.get( index );
    }
  }

  /**
   * @see java.util.List#remove(int)
   */
  public Object remove( final int index )
  {
    synchronized( m_lock )
    {
      final Object removedItem = m_items.remove( index );
      if( m_index != null )
      {
        // We remove with null envelope here, else we would break the synchronized code by calling getEnvelope() here
        m_index.remove( null, removedItem );
      }
      return removedItem;
    }
  }

  /**
   * @deprecated SLOW (as we are using ArrayList internally) TODO: better comment, make deprecated in interface
   * @see java.util.List#add(int, java.lang.Object)
   */
  @Deprecated
  public void add( final int index, final Object item )
  {
    final Envelope env = getEnvelope( item );

    synchronized( m_lock )
    {
      m_items.add( index, item );
      if( m_index != null )
        m_index.insert( env, item );
    }
  }

  /**
   * @deprecated SLOW TODO: better comment, make deprecated in interface
   * @see java.util.List#indexOf(java.lang.Object)
   */
  @Deprecated
  public int indexOf( final Object item )
  {
    synchronized( m_lock )
    {
      return m_items.indexOf( item );
    }
  }

  /**
   * @deprecated SLOW TODO: better comment, make deprecated in interface
   * @see java.util.List#lastIndexOf(java.lang.Object)
   */
  @Deprecated
  public int lastIndexOf( final Object item )
  {
    synchronized( m_lock )
    {
      return m_items.lastIndexOf( item );
    }
  }

  /**
   * @see java.util.List#contains(java.lang.Object)
   */
  public boolean contains( final Object item )
  {
    final Envelope env = getEnvelope( item );
    checkIndex();

    synchronized( m_lock )
    {
      // TODO: slow, as the index may be recalculated, check if possible to avoid this
      return m_index.contains( env, item );
    }
  }

  /**
   * @see java.util.List#addAll(int, java.util.Collection)
   */
  public boolean addAll( final int index, final Collection c )
  {
    // First get all envelope, in order no to break the synchronize code by calling getEnvelope inside
    final Map<Object, Envelope> newItems = new HashMap<Object, Envelope>();
    for( final Object item : c )
    {
      final Envelope envelope = getEnvelope( item );
      newItems.put( item, envelope );
    }

    synchronized( m_lock )
    {
      final boolean added = m_items.addAll( index, c );
      if( m_index != null )
      {
        for( final Entry<Object, Envelope> entry : newItems.entrySet() )
          m_index.insert( entry.getValue(), entry.getKey() );
      }
      return added;
    }
  }

  /**
   * @see java.util.List#addAll(java.util.Collection)
   */
  public boolean addAll( final Collection c )
  {
    // First get all envelope, in order no to break the synchronize code by calling getEnvelope inside
    final Map<Object, Envelope> newItems = new HashMap<Object, Envelope>();
    for( final Object item : c )
    {
      final Envelope envelope = getEnvelope( item );
      newItems.put( item, envelope );
    }

    synchronized( m_lock )
    {
      final boolean added = m_items.addAll( c );
      if( m_index != null )
      {
        for( final Entry<Object, Envelope> entry : newItems.entrySet() )
          m_index.insert( entry.getValue(), entry.getKey() );
      }
      return added;
    }
  }

  /**
   * @see java.util.List#containsAll(java.util.Collection)
   */
  public boolean containsAll( final Collection c )
  {
    // First get all envelope, in order no to break the synchronize code by calling getEnvelope inside
    final Map<Object, Envelope> newItems = new HashMap<Object, Envelope>();
    for( final Object item : c )
    {
      final Envelope envelope = getEnvelope( item );
      newItems.put( item, envelope );
    }

    checkIndex();

    synchronized( m_lock )
    {
      // TODO: see contains()
      for( final Entry<Object, Envelope> entry : newItems.entrySet() )
      {
        if( !m_index.contains( entry.getValue(), entry.getKey() ) )
          return false;
      }
      return true;
    }
  }

  /**
   * @deprecated SLOW: TODO: better comment, make deprecated in interface
   * @see java.util.List#removeAll(java.util.Collection)
   */
  @Deprecated
  public boolean removeAll( final Collection c )
  {
    boolean result = false;
    for( final Object object : c )
      result |= remove( object );

    return result;
  }

  /**
   * NOT IMPLEMENTED
   * 
   * @see java.util.List#retainAll(java.util.Collection)
   */
  public boolean retainAll( final Collection c )
  {
    // TODO: implement
    throw new UnsupportedOperationException();
  }

  /**
   * ATTENTION: do not remove object via this iterator, it will break the geo-index<br>
   * TODO: wrap iterator in order to maintain the index's consistency
   * 
   * @see java.util.List#iterator()
   */
  public Iterator iterator( )
  {
    // TODO: what about synchronization?

    return m_items.iterator();
  }

  /**
   * NOT IMPLEMENTED
   * 
   * @see java.util.List#subList(int, int)
   */
  public List subList( final int fromIndex, final int toIndex )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * ATTENTION: do not remove object via this iterator, it will break the geo-index<br>
   * TODO: wrap iterator in order to maintain the index's consistency
   * 
   * @see java.util.List#listIterator()
   */
  public ListIterator listIterator( )
  {
    // TODO: what about synchronization?
    return m_items.listIterator();
  }

  /**
   * ATTENTION: do not remove object via this iterator, it will break the geo-index<br>
   * TODO: wrap iterator in order to maintain the index's consistency
   * 
   * @see java.util.List#listIterator(int)
   */
  public ListIterator listIterator( final int index )
  {
    // TODO: what about synchronization?
    return m_items.listIterator( index );
  }

  /**
   * @see java.util.List#set(int, java.lang.Object)
   */
  public Object set( final int index, final Object newItem )
  {
    final Envelope newEnv = getEnvelope( newItem );

    synchronized( m_lock )
    {
      final Object oldItem = m_items.set( index, newItem );
      if( m_index != null )
      {
        // remove with null envelope, in order not to break the synchronization code by calling getEnvelope
        m_index.remove( null, oldItem );
        m_index.insert( newEnv, newItem );
      }
      return oldItem;
    }
  }

  /**
   * @see java.util.List#toArray(java.lang.Object[])
   */
  public Object[] toArray( final Object[] a )
  {
    synchronized( m_lock )
    {
      return m_items.toArray( a );
    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureList#toFeatures()
   */
  @Deprecated
  public Feature[] toFeatures( )
  {
    synchronized( m_lock )
    {
      // TODO: this will probably not work, as the list may contain non-features
      return m_items.toArray( new Feature[m_items.size()] );
    }
  }

  /**
   * TODO: add flags for visiting links/depth
   * 
   * @see org.kalypsodeegree.model.feature.FeatureList#accept(org.kalypsodeegree.model.feature.FeatureVisitor)
   */
  public void accept( final FeatureVisitor visitor )
  {
    // TODO: not synchronized: Problem?

    for( final Object object : m_items )
    {
      // TODO: also visit links?
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

  /**
   * @see org.kalypsodeegree.model.sort.JMSpatialIndex#invalidate()
   */
  public void invalidate( )
  {
    synchronized( m_lock )
    {
      m_index = null;
    }
  }

  /**
   * @see org.kalypsodeegree.model.sort.JMSpatialIndex#invalidate(java.lang.Object)
   */
  public void invalidate( final Object o )
  {
    final Envelope envelope = getEnvelope( o );

    synchronized( m_lock )
    {
      if( m_index != null )
      {
        m_index.remove( null, o );

        m_index.insert( envelope, o );
      }
    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureList#first()
   */
  public Object first( )
  {
    synchronized( m_lock )
    {
      if( size() == 0 )
        return null;
      return m_items.get( 0 );
    }
  }

}