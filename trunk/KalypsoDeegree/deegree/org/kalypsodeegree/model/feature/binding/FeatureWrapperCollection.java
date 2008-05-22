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
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

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
   *            the feature or feature collection with a list property to wrap
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

  /**
   * @deprecated THIS IS RUBBISH!
   */
  @Deprecated
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
    final Feature f = element.getFeature();
    m_featureList.add( index, f );
  }

  public boolean add( final FWCls o )
  {
    final Feature f = o.getFeature();
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
      feature = FeatureHelper.createFeatureForListProp( m_featureList, newChildType, -1 );
      final T wrapper = (T) feature.getAdapter( classToAdapt );
      if( wrapper == null )
      {
        throw new IllegalArgumentException( "Feature not adaptable. FeatureType: " + newChildType + ", TypeToAdapt: " + m_defaultWrapperClass + ", Feature: " + feature );
      }

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
      final Feature feature = FeatureHelper.createFeatureForListProp( m_featureList, newChildType, index );
      final T wrapper = (T) feature.getAdapter( classToAdapt );
      if( wrapper == null )
      {
        throw new IllegalArgumentException( "Feature not adaptable. FeatureType: " + newChildType + ", TypeToAdapt: " + m_defaultWrapperClass + ", Feature: " + feature );
      }

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
    return getAdaptedFeature( f );
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

      // Why backwards??: because of remove below...
      for( int i = size() - 1; i >= 0; i-- )
      {
        final FWCls cls = get( i );
        if( cls == null )
        {
          // FIXME: this is no good! This fixing of a bad model should not happen here, this is too dangerous!
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
        if( f == null )
          throw new RuntimeException( "Feature does not exists: " + next.toString() );
        final FWCls wrapper = getAdaptedFeature( f );
        if( wrapper == null )
          throw new RuntimeException( "Feature " + f + " could not be adapted: " + f.getId() );
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
      return m_featureList.lastIndexOf( ((IFeatureWrapper2) o).getFeature() );
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
        lit.add( o.getFeature() );
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
        lit.set( o.getFeature() );
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
      boolean removed = m_featureList.remove( ((IFeatureWrapper2) o).getFeature() );
      if( !removed )
        removed = m_featureList.remove( ((IFeatureWrapper2) o).getFeature().getId() );
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
    final Feature f = element.getFeature();

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
      final Object object = getAdaptedFeature( feature );
      if( object != null )
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
  public Feature getFeature( )
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
        final GM_Object prop = feature.getFeature().getDefaultGeometryProperty();
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
    for( final Object linkOrFeature : selectedFeature )
    {
      final Feature feature = FeatureHelper.getFeature( m_featureCollection.getWorkspace(), linkOrFeature );
      final FWCls adaptedFeature = getAdaptedFeature( feature );
      if( adaptedFeature != null )
        selFW.add( adaptedFeature );
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
    FeatureHelper.cloneFeature( getFeature(), relationType, toClone.getFeature() );
  }

  public void addSecondaryWrapper( final Class<FWCls> secondaryWrapper )
  {
    if( !m_secondaryWrapperClasses.contains( secondaryWrapper ) )
      m_secondaryWrapperClasses.add( secondaryWrapper );
  }

  private FWCls getAdaptedFeature( final Feature feature )
  {
    if( feature == null )
      return null;
    FWCls adapted = (FWCls) feature.getAdapter( m_defaultWrapperClass );
    if( adapted == null )
    {
      for( final Class<FWCls> clazz : m_secondaryWrapperClasses )
      {
        adapted = (FWCls) feature.getAdapter( clazz );
        if( adapted != null )
          return adapted;
      }
    }
    return adapted;

  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection#getBoundingBox()
   */
  public GM_Envelope getBoundingBox( )
  {
    return getWrappedList().getBoundingBox();
  }
}
