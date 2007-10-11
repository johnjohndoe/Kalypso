/**
 *
 */
package org.kalypso.risk.model.schema.binding;

import java.util.Collections;
import java.util.Hashtable;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IAdapterFactory;
import org.kalypsodeegree.model.feature.Feature;

public class KalypsoRiskFeatureFactory implements IAdapterFactory
{
  public final void warnUnableToAdapt( final Feature featureToAdapt, final QName featureQName, final Class targetClass )
  {
    // System.out.println("Unable to adapt "+featureToAdapt.getFeatureType().getQName()+" to "+targetClass.getName());
  }

  interface AdapterConstructor
  {
    public Object constructAdapter( Feature feature, Class cls ) throws IllegalArgumentException;
  }

  private final Map<Class, AdapterConstructor> m_constructors = createConstructorMap();

  public KalypsoRiskFeatureFactory( )
  {
  }

  /**
   * @see org.eclipse.core.runtime.IAdapterFactory#getAdapter(java.lang.Object, java.lang.Class)
   */
  public Object getAdapter( final Object adaptableObject, final Class adapterType )
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
  public Class[] getAdapterList( )
  {
    return m_constructors.keySet().toArray( new Class[m_constructors.size()] );
  }

  private final Map<Class, AdapterConstructor> createConstructorMap( )
  {
    final Map<Class, AdapterConstructor> cMap = new Hashtable<Class, AdapterConstructor>();

    AdapterConstructor cTor;

    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();
        if( featureQName.equals( ILandusePolygonCollection.QNAME ) )
        {
          return new LandusePolygonCollection( feature );
        }
        else
        {
          warnUnableToAdapt( feature, featureQName, ILandusePolygonCollection.class );
          return null;
        }
      }
    };
    cMap.put( LandusePolygonCollection.class, cTor );

    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();
        if( featureQName.equals( ILandusePolygon.QNAME ) )
        {
          return new LandusePolygon( feature );
        }
        else
        {
          warnUnableToAdapt( feature, featureQName, ILandusePolygon.class );
          return null;
        }
      }
    };
    cMap.put( LandusePolygon.class, cTor );

    return Collections.unmodifiableMap( cMap );
  }

}
