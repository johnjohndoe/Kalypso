package org.kalypsodeegree.model.feature.binding;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;

/**
 * Class representing the wbGml:RangeSetFeature element. The feature contained in the range set need to be adaptable
 * into a {@link RangeSetCls} object
 * 
 * @author Patrice Congo
 */
@SuppressWarnings( { "unchecked" })
public class FeatureWrapperCollection<FWCls extends IFeatureWrapper2> extends AbstractFeatureBinder implements IFeatureWrapperCollection<FWCls>
{
  /**
   * TODO: remove and use getWrappedFeatur everywhere
   * <p>
   * The feature wrapped by this object
   */
  private final Feature m_featureCollection;

  /**
   * the list of the feature properties
   */
  private final FeatureList m_featureList;

  /**
   * The {@link QName} of the list property of the feature(-collection)
   */
  private final QName m_featureMemberProp;

  /**
   * The class of the DEFAULT feature wrapper in this collection
   */
  private final Class<FWCls> m_defaultWrapperClass;

  private final List<Class<FWCls>> m_secondaryWrapperClasses = new ArrayList<Class<FWCls>>();

  /**
   * Creates a new {@link FeatureWrapperCollection} wrapping the provided feature
   * 
   * @param featureCol
   *            the feature or feature collection with a list property to wrapp
   * @param fwClass
   *            the base class representing the property feature in the list
   * @param featureMemberProp
   *            the list property linking the feature and its properties
   */
  public FeatureWrapperCollection( final Feature featureCol, final Class<FWCls> fwClass, final QName featureMemberProp )
  {
    super( featureCol, featureCol.getFeatureType().getQName() );
    m_defaultWrapperClass = fwClass;
    m_featureCollection = featureCol;
    m_featureMemberProp = featureMemberProp;
    m_featureList = (FeatureList) m_featureCollection.getProperty( featureMemberProp );
  }

  public FeatureWrapperCollection( final Feature parentFeature, final QName childQName, final QName featureMemberProp, final Class<FWCls> fwClass ) throws IllegalArgumentException
  {
    super( createSubfeature( parentFeature, childQName, featureMemberProp ), childQName );
    try
    {
      this.m_featureCollection = FeatureHelper.addFeature( parentFeature, featureMemberProp, childQName );
    }
    catch( final GMLSchemaException ex )
    {
      throw new IllegalArgumentException( "Parent does not accept property of the specified type. Parent: " + parentFeature + ", PropertyType: " + featureMemberProp + ", ChildType: " + childQName, ex );
    }
    m_featureList = (FeatureList) m_featureCollection.getProperty( featureMemberProp );
    m_featureMemberProp = featureMemberProp;
    m_defaultWrapperClass = fwClass;
  }

  private static Feature createSubfeature( final Feature parentFeature, final QName childQName, final QName featureMemberProp )
  {
    try
    {
      return FeatureHelper.addFeature( parentFeature, featureMemberProp, childQName );
    }
    catch( final GMLSchemaException ex )
    {
      throw new IllegalArgumentException( "Parent does not accept property of the specified type. Parent: " + parentFeature + ", PropertyType: " + featureMemberProp + ", ChildType: " + childQName, ex );
    }
  }

  public void add( final int index, final FWCls element )
  {
    final Feature f = element.getWrappedFeature();
    m_featureList.add( index, f );
  }

  public boolean add( final FWCls o )
  {
    final Feature f = o.getWrappedFeature();
    return m_featureList.add( f );
  }

  public FWCls addNew( final QName newChildType )
  {
    return addNew( newChildType, m_defaultWrapperClass );
  }

  public <T extends FWCls> T addNew( final QName newChildType, final Class<T> classToAdapt )
  {
    Feature feature = null;
    try
    {
      feature = FeatureHelper.createFeatureForListProp( m_featureList, m_featureMemberProp, newChildType );
      final T wrapper = (T) feature.getAdapter( classToAdapt );
      if( wrapper == null )
      {
        throw new IllegalArgumentException( "Feature not adaptable. FeatureType: " + newChildType + ", TypeToAdapt: " + m_defaultWrapperClass + ", Feature: " + feature );
      }
      // Feature was already added by Util.create..., so dont add it again
      // featureList.add(feature);
      return wrapper;
    }
    catch( final GMLSchemaException e )
    {
      throw new IllegalArgumentException( "feature:" + feature + " class=" + m_defaultWrapperClass + " featureQName=" + newChildType, e );
    }
  }

  public FWCls addNew( final QName newChildType, final String newFeatureId )
  {
    return addNew( newChildType, newFeatureId, m_defaultWrapperClass );
  }

  public <T extends FWCls> T addNew( final QName newChildType, final String newFeatureId, final Class<T> classToAdapt )
  {
    Feature feature = null;
    try
    {
      feature = FeatureHelper.createFeatureWithId( newChildType, m_featureCollection, m_featureMemberProp, newFeatureId );
      final T wrapper = (T) feature.getAdapter( classToAdapt );
      if( wrapper == null )
      {
        throw new IllegalArgumentException( "Feature not adaptable. FeatureType: " + newChildType + ", TypeToAdapt: " + m_defaultWrapperClass + ", Feature: " + feature );
      }
      return wrapper;
    }
    catch( final Exception e )
    {
      throw new IllegalArgumentException( "Feature=" + feature + " class=" + m_defaultWrapperClass, e );
    }
  }

  public FWCls addNew( final int index, final QName newChildType )
  {
    return addNew( index, newChildType, m_defaultWrapperClass );
  }

  public <T extends FWCls> T addNew( final int index, final QName newChildType, final Class<T> classToAdapt )
  {
    try
    {
      final Feature feature = FeatureHelper.createFeatureForListProp( m_featureList, m_featureMemberProp, newChildType );
      final T wrapper = (T) feature.getAdapter( classToAdapt );
      if( wrapper == null )
      {
        throw new IllegalArgumentException( "Feature not adaptable. FeatureType: " + newChildType + ", TypeToAdapt: " + m_defaultWrapperClass + ", Feature: " + feature );
      }
      m_featureList.add( index, feature );
      return wrapper;
    }
    catch( final GMLSchemaException e )
    {
      throw new IllegalArgumentException( e );
    }
  }

  public boolean addAll( final Collection< ? extends FWCls> c )
  {
    return m_featureList.addAll( FeatureHelper.toFeatureList( c ) );
  }

  public boolean addAll( final int index, final Collection< ? extends FWCls> c )
  {
    return m_featureList.addAll( index, FeatureHelper.toFeatureList( c ) );
  }

  public void clear( )
  {
    m_featureList.clear();
  }

  public boolean contains( final Object o )
  {
    return indexOf( o ) != -1;
  }

  public boolean containsAll( final Collection< ? > c )
  {
    throw new UnsupportedOperationException();
    /*
     * TODO: see comment at 'contains' /* return featureList.containsAll(c);
     */
    /*
     * TOASK i do not understand that one. and featureList.containsAll(c) will only work if featureList contains real
     * features or references
     */
  }

  public FWCls get( final int index )
  {
    final Object property = m_featureList.get( index );
    final Feature f = FeatureHelper.getFeature( m_featureCollection.getWorkspace(), property );
    if( f == null )
      return null;
    FWCls adapted = (FWCls) f.getAdapter( m_defaultWrapperClass );
    if( adapted == null )
    {
      for( int i = 0; i < m_secondaryWrapperClasses.size(); i++ )
      {
        adapted = (FWCls) f.getAdapter( m_secondaryWrapperClasses.get( i ) );
        if( adapted != null )
          break;
      }
    }
    return adapted;
  }

  public int indexOf( final Object o )
  {
    if( o instanceof IFeatureWrapper2 )
    {
      // The following line does not work, because the feature list
      // may
      // contain strings (i.e. references to features)
      // return featureList.indexOf(((IFeatureWrapper) o)
      // .getWrappedFeature());

      // We do trust in the equals implementation of
      // AbstractFeatureBinder
      /*
       * TODO may be we should put equals() into the iwrapper interface because all ifeaturewrapper must not extends
       * AbtractFeatureBinder, and this will therefore work, i guess, only for AbstractFeatureBinder childs. i will have
       * a look during the move IfeatureWrapper to kalypso deegree
       */
      for( int i = size() - 1; i >= 0; i-- )
      {
        final FWCls cls = get( i );
        if( cls == null )
        {
          // bad link removing it
          System.out.println( "removing bad link:" + m_featureList.get( i ) );
          remove( i );
          continue;
        }

        if( cls.equals( o ) )
          return i;
      }
    }

    return -1;
  }

  public boolean isEmpty( )
  {
    return m_featureList.isEmpty();
  }

  public Iterator<FWCls> iterator( )
  {
    return new Iterator<FWCls>()
    {
      private final Iterator it = m_featureList.iterator();

      private final GMLWorkspace workspace = m_featureCollection.getWorkspace();

      synchronized public boolean hasNext( )
      {
        return it.hasNext();
      }

      synchronized public FWCls next( )
      {
        final Object next = it.next();
        final Feature f = FeatureHelper.getFeature( workspace, next );

        FWCls wrapper = (FWCls) f.getAdapter( m_defaultWrapperClass );
        if( wrapper == null )
        {
          for( final Class<FWCls> clazz : m_secondaryWrapperClasses )
          {
            wrapper = (FWCls) f.getAdapter( clazz );
            if( wrapper != null )
              return wrapper;
          }
          throw new RuntimeException( "Feature " + f + " could not be adapted to " + m_defaultWrapperClass );
        }
        return wrapper;
      }

      public void remove( )
      {
        it.remove();
      }

    };
  }

  public int lastIndexOf( final Object o )
  {
    if( o instanceof IFeatureWrapper2 )
    {
      return m_featureList.lastIndexOf( ((IFeatureWrapper2) o).getWrappedFeature() );
    }
    else
    {
      return -1;
    }
  }

  public ListIterator<FWCls> listIterator( )
  {
    return listIterator( 0 );
  }

  public ListIterator<FWCls> listIterator( final int index )
  {
    return new ListIterator<FWCls>()
    {
      private final ListIterator lit = m_featureList.listIterator( index );

      @SuppressWarnings("unchecked")
      public void add( final FWCls o )
      {
        lit.add( o.getWrappedFeature() );
      }

      public boolean hasNext( )
      {
        return lit.hasNext();
      }

      public boolean hasPrevious( )
      {
        return lit.hasPrevious();
      }

      public FWCls next( )
      {
        final Feature f = FeatureHelper.getFeature( m_featureCollection.getWorkspace(), lit.next() );
        final Object wrapper = f.getAdapter( m_defaultWrapperClass );
        return (FWCls) wrapper;
      }

      public int nextIndex( )
      {
        return lit.nextIndex();
      }

      public FWCls previous( )
      {
        final Feature f = (Feature) lit.previous();
        final Object wrapper = f.getAdapter( m_defaultWrapperClass );
        return (FWCls) wrapper;
      }

      public int previousIndex( )
      {
        return lit.previousIndex();
      }

      public void remove( )
      {
        lit.remove();
      }

      public void set( final FWCls o )
      {
        lit.set( o.getWrappedFeature() );
      }

    };
  }

  public FWCls remove( final int index )
  {
    final FWCls wrapper = FeatureHelper.getFeature( m_featureCollection.getWorkspace(), m_featureList.remove( index ), m_defaultWrapperClass );
    return wrapper;
  }

  public boolean remove( final Object o )
  {
    if( o instanceof IFeatureWrapper2 )
    {
      boolean removed = m_featureList.remove( ((IFeatureWrapper2) o).getWrappedFeature() );
      if( !removed )
        removed = m_featureList.remove( ((IFeatureWrapper2) o).getWrappedFeature().getId() );
      return removed;
    }
    else if( o instanceof String )
    {
      return m_featureList.remove( o );
    }
    else
    {
      return m_featureList.remove( o );
    }
  }

  public boolean removeAll( final Collection< ? > c )
  {
    boolean ret = false;
    for( final Object o : c )
      ret = ret || remove( o );
    return ret;
  }

  public boolean retainAll( final Collection< ? > c )
  {
    throw new UnsupportedOperationException();
  }

  public FWCls set( final int index, final FWCls element )
  {
    final FWCls r = get( index );
    final Feature f = element.getWrappedFeature();

    m_featureList.set( index, f );
    return r;
  }

  public int size( )
  {
    return m_featureList.size();
  }

  public List<FWCls> subList( final int fromIndex, final int toIndex )
  {
    return null;
  }

  public Object[] toArray( )
  {
    final Object objs[] = new Object[size()];
    for( int i = 0; i < objs.length; i++ )
    {
      final Object fObj = m_featureList.get( i );

      final Feature feature = FeatureHelper.getFeature( m_featureList.getParentFeature().getWorkspace(), fObj );
      if( feature == null )
        throw new RuntimeException( "Type not known:" + fObj );

      Object object = feature.getAdapter( m_defaultWrapperClass );
      if( object == null )
      {
        for( final Class<FWCls> clazz : m_secondaryWrapperClasses )
        {
          object = (FWCls) feature.getAdapter( clazz );
          if( object != null )
            break;
        }
        if( object == null )
          throw new RuntimeException( String.format( "Unable to adapt object %s to %s.", feature, m_defaultWrapperClass ) );
      }
      objs[i] = object;
    }
    return objs;
  }

  public <T> T[] toArray( T[] a )
  {
    final int SIZE = size();
    final Class compType = a.getClass().getComponentType();
    if( !compType.isAssignableFrom( m_defaultWrapperClass ) )
    {
      throw new ArrayStoreException();
    }

    if( a.length < SIZE )
    {
      a = (T[]) Array.newInstance( compType, SIZE );
    }
    for( int i = SIZE - 1; i >= 0; i-- )
    {
      a[i] = (T) get( i );
    }

    if( a.length > SIZE )
    {
      a[SIZE] = null;
    }
    return a;
  }

  @Override
  public boolean equals( final Object obj )
  {
    if( obj instanceof FeatureWrapperCollection )
    {
      return m_featureList.equals( ((FeatureWrapperCollection) obj).m_featureList );
    }
    else if( obj instanceof IFeatureWrapperCollection )
    {
      final IFeatureWrapperCollection frs = (IFeatureWrapperCollection) obj;
      final int SIZE = size();
      if( SIZE != frs.size() )
      {
        return false;
      }
      for( int i = SIZE - 1; i >= 0; i-- )
      {
        if( !get( i ).equals( frs.get( 0 ) ) )
        {
          return false;
        }
      }
      return true;
    }
    else
    {
      return super.equals( obj );
    }
  }

  @Override
  public Feature getWrappedFeature( )
  {
    return m_featureCollection;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.IFeatureWrapper#getGmlID()
   */
  @Override
  public String getGmlID( )
  {
    return m_featureCollection.getId();
  }

  public FeatureList getWrappedList( )
  {
    return m_featureList;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection#removeAllRefs(org.kalypsodeegree.model.feature.binding.IFeatureWrapper)
   */
  public void removeAllRefs( final FWCls toRemove ) throws IllegalArgumentException
  {
    if( toRemove == null )
      return;
    final String gmlID = toRemove.getGmlID();
    m_featureList.remove( gmlID );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection#addRef(org.kalypsodeegree.model.feature.binding.IFeatureWrapper)
   */
  public boolean addRef( final FWCls toAdd ) throws IllegalArgumentException
  {
    final String gmlID = toAdd.getGmlID();
    // TODO: this can cause major performance leaks
    if( m_featureList.contains( gmlID ) )
    {
      return false;
    }
    return m_featureList.add( gmlID );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection#query(org.kalypsodeegree.model.geometry.GM_Surface,
   *      boolean, javax.xml.namespace.QName)
   */
  public List<FWCls> query( final GM_Surface selectionSurface, final boolean containedOnly )
  {
    final List selectedFeature = m_featureList.query( selectionSurface.getEnvelope(), null );
    final List<FWCls> selFW = new ArrayList<FWCls>( selectedFeature.size() );
    final GMLWorkspace workspace = m_featureCollection.getWorkspace();

    for( final Object linkOrFeature : selectedFeature )
    {
      final FWCls feature = FeatureHelper.getFeature( workspace, linkOrFeature, m_defaultWrapperClass );
      if( feature != null )
      {
        final GM_Object prop = feature.getWrappedFeature().getDefaultGeometryProperty();
        if( containedOnly )
        {
          if( selectionSurface.contains( prop ) )
            selFW.add( feature );
        }
        else
        {
          if( selectionSurface.intersects( prop ) )
            selFW.add( feature );
        }
      }
    }

    return selFW;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection#query(org.kalypsodeegree.model.geometry.GM_Envelope)
   */

  public List<FWCls> query( final GM_Envelope envelope )
  {
    final List selectedFeature = m_featureList.query( envelope, null );
    final List<FWCls> selFW = new ArrayList<FWCls>( selectedFeature.size() );
    final GMLWorkspace workspace = m_featureCollection.getWorkspace();

    for( final Object linkOrFeature : selectedFeature )
    {
      final FWCls feature = FeatureHelper.getFeature( workspace, linkOrFeature, m_defaultWrapperClass );
      if( feature != null )
      {
        selFW.add( feature );
      }
    }
    return selFW;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection#query(org.kalypsodeegree.model.geometry.GM_Position)
   */
  public List<FWCls> query( final GM_Position position )
  {
    final List selectedFeature = m_featureList.query( position, null );
    final List<FWCls> selFW = new ArrayList<FWCls>( selectedFeature.size() );
    final GMLWorkspace workspace = m_featureCollection.getWorkspace();

    for( final Object linkOrFeature : selectedFeature )
    {
      final FWCls feature = FeatureHelper.getFeature( workspace, linkOrFeature, m_defaultWrapperClass );
      if( feature != null )
      {
        selFW.add( feature );
      }
    }
    return selFW;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection#countFeatureWrappers(java.lang.Class)
   */
  public int countFeatureWrappers( final Class wrapperClass )
  {
    int num = 0;
    for( final FWCls ele : this )
      if( wrapperClass.isInstance( ele ) )
        num++;
    return num;
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection#cloneInto(org.kalypsodeegree.model.feature.binding.IFeatureWrapper2)
   */
  public void cloneInto( final FWCls toClone ) throws Exception
  {
    final IRelationType relationType = m_featureList.getParentFeatureTypeProperty();
    FeatureHelper.cloneFeature( getWrappedFeature(), relationType, toClone.getWrappedFeature() );
  }

  public void addSecondaryWrapper( final Class<FWCls> secondaryWrapper )
  {
    if( !m_secondaryWrapperClasses.contains( secondaryWrapper ) )
      m_secondaryWrapperClasses.add( secondaryWrapper );
  }

}
