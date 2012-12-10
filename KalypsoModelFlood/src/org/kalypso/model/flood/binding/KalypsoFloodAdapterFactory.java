/**
 *
 */
package org.kalypso.model.flood.binding;

import java.util.Collections;
import java.util.Hashtable;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IAdapterFactory;
import org.kalypsodeegree.model.feature.Feature;

public class KalypsoFloodAdapterFactory implements IAdapterFactory
{
  interface AdapterConstructor
  {
    public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException;
  }

  private final Map<Class< ? >, AdapterConstructor> m_constructors = createConstructorMap();

  /**
   * @see org.eclipse.core.runtime.IAdapterFactory#getAdapter(java.lang.Object, java.lang.Class)
   */
  @Override
  public Object getAdapter( final Object adaptableObject, @SuppressWarnings("rawtypes") final Class adapterType )
  {
    final AdapterConstructor ctor = m_constructors.get( adapterType );
    if( ctor != null )
    {
      return ctor.constructAdapter( (Feature) adaptableObject, adapterType );
    }
    return null;
  }

  /**
   * @see org.eclipse.core.runtime.IAdapterFactory#getAdapterList()
   */
  @Override
  public Class< ? >[] getAdapterList( )
  {
    return m_constructors.keySet().toArray( new Class[m_constructors.size()] );
  }

  private final Map<Class< ? >, AdapterConstructor> createConstructorMap( )
  {
    final Map<Class< ? >, AdapterConstructor> cMap = new Hashtable<Class< ? >, AdapterConstructor>();

    AdapterConstructor cTor;

    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();
        if( featureQName.equals( IFloodModel.QNAME ) )
          return new FloodModel( feature );
        else
          return null;
      }
    };
    cMap.put( IFloodModel.class, cTor );

    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();
        if( featureQName.equals( IFloodExtrapolationPolygon.QNAME ) )
          return new FloodExtrapolationPolygon( feature );
        else if( featureQName.equals( IFloodClipPolygon.QNAME ) )
          return new FloodClipPolygon( feature );
        else if( featureQName.equals( IFloodVolumePolygon.QNAME ) )
          return new FloodVolumePolygon( feature );
        else
          return null;
      }
    };
    cMap.put( IFloodExtrapolationPolygon.class, cTor );
    cMap.put( IFloodClipPolygon.class, cTor );
    cMap.put( IFloodPolygon.class, cTor );

    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();
        if( featureQName.equals( IRunoffEvent.QNAME ) )
          return new RunoffEvent( feature );
        else
          return null;
      }
    };
    cMap.put( IRunoffEvent.class, cTor );

    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();
        if( featureQName.equals( ITinReference.QNAME ) )
          return new TinReference( feature );
        else
          return null;
      }
    };
    cMap.put( ITinReference.class, cTor );

    return Collections.unmodifiableMap( cMap );
  }

}
