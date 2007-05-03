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
        // so do not access the envelope, whichs may cause problems now
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

  public static boolean showIndexEnv = false;

  private final SpatialIndexExt m_index;

  private final List<Object> m_objects = new ArrayList<Object>();

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

//     m_index = new SplitSortSpatialIndex( m_envelopeProvider, env );
    m_index = new QuadTreeIndex( m_envelopeProvider );
  }

  public boolean add( final Object object )
  {
    final GM_Envelope env = getEnvelope( object );
    spacialAdd( env, object );

    return m_objects.add( object );
  }

  private void spacialAdd( final GM_Envelope env, final Object object )
  {
    final Envelope itemEnv = env == null ? new Envelope() : JTSAdapter.export( env );
    m_index.insert( itemEnv, object );
  }

  public List query( final GM_Envelope queryEnv, List result )
  {
    if( result == null )
      result = new ArrayList();

    final Envelope env = JTSAdapter.export( queryEnv );

    final List list = m_index.query( env );
    for( final Object object : list )
      result.add( object );

    return result;
  }

  public List query( final GM_Position pos, List result )
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

  public boolean remove( final Object object )
  {
    final GM_Envelope env = getEnvelope( object );
    remove(  env, object );

    return m_objects.remove( object );
  }

  private void remove( final GM_Envelope env, final Object object )
  {
    final Envelope itemEnv = JTSAdapter.export( env );
    m_index.remove( itemEnv, object );
  }

  protected GM_Envelope getEnvelope( final Object object )
  {
    return m_envelopeProvider.getEnvelope( object );
  }

  public void paint( final Graphics g, final GeoTransform geoTransform )
  {
    m_index.paint( g, geoTransform );
  }

  public int rsize( )
  {
    return 0;
  }

  public GM_Envelope getBoundingBox( )
  {
    final Envelope bbox = m_index.getBoundingBox();
    if( bbox.isNull() )
      return null;
    return JTSAdapter.wrap( bbox );
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
    m_index.clear();

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
  public Object get( final int index )
  {
    return m_objects.get( index );
  }

  /**
   * @see java.util.List#remove(int)
   */
  public Object remove( final int index )
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
   * IMPORTANT: this operation is slow (as in ArrayList).
   * 
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
  public boolean removeAll( final Collection c )
  {
    boolean result = false;
    for( final Object object : c )
      result |= remove( object );

    return result;
  }

  /**
   * @see java.util.List#retainAll(java.util.Collection)
   */
  public boolean retainAll( Collection c )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * ATTENTION: do not remove object via this iterator, it will break the geo-index
   * 
   * @see java.util.List#iterator()
   */
  public Iterator iterator( )
  {
    return m_objects.iterator();
  }

  /**
   * @see java.util.List#subList(int, int)
   */
  public List subList( final int fromIndex, final int toIndex )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * ATTENTION: do not remove object via this iterator, it will break the geo-index
   * 
   * @see java.util.List#listIterator()
   */
  public ListIterator listIterator( )
  {
    return m_objects.listIterator();
  }

  /**
   * ATTENTION: do not remove object via this iterator, it will break the geo-index
   * 
   * @see java.util.List#listIterator(int)
   */
  public ListIterator listIterator( final int index )
  {
    return m_objects.listIterator( index );
  }

  /**
   * @see java.util.List#set(int, java.lang.Object)
   */
  public Object set( final int index, final Object element )
  {
    final GM_Envelope env = getEnvelope( element );
    remove( env, element );

    /* Only update index if we are valid. Else we do not need to because we get a resort at the next query. */
    spacialAdd( env, element );

    return m_objects.set( index, element );
  }

  /**
   * @see java.util.List#toArray(java.lang.Object[])
   */
  @SuppressWarnings("unchecked")
  public Object[] toArray( Object[] a )
  {
    return m_objects.toArray( a );
  }

  /**
   * @deprecated use toArray() cause in a splitsort can be also featureIds (String), if feature is linked from the list
   * @see org.kalypsodeegree.model.feature.FeatureList#toFeatures()
   */
  @Deprecated
  public Feature[] toFeatures( )
  {
    return m_objects.toArray( new Feature[m_objects.size()] );
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureList#accept(org.kalypsodeegree.model.feature.FeatureVisitor)
   */
  public void accept( final FeatureVisitor visitor )
  {
    for( final Object object : m_objects )
    {
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
    m_index.invalidate();
  }

  /**
   * @see org.kalypsodeegree.model.sort.JMSpatialIndex#invalidate(java.lang.Object)
   */
  public void invalidate( final Object o )
  {
    m_index.invalidate( o );
  }

}