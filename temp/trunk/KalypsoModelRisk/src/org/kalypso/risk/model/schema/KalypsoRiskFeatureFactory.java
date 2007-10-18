/**
 *
 */
package org.kalypso.risk.model.schema;

import java.util.Collections;
import java.util.Hashtable;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IAdapterFactory;
import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;
import org.kalypso.risk.model.schema.binding.ILanduseClass;
import org.kalypso.risk.model.schema.binding.ILanduseClassCollection;
import org.kalypso.risk.model.schema.binding.ILanduseModel;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.schema.binding.ILandusePolygonCollection;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.schema.binding.IWaterdepthCoverage;
import org.kalypso.risk.model.schema.binding.IWaterdepthCoverageCollection;
import org.kalypso.risk.model.schema.binding.IWaterdepthCoverageModel;
import org.kalypso.risk.model.schema.binding.LanduseClass;
import org.kalypso.risk.model.schema.binding.LanduseClassCollection;
import org.kalypso.risk.model.schema.binding.LanduseModel;
import org.kalypso.risk.model.schema.binding.LandusePolygon;
import org.kalypso.risk.model.schema.binding.LandusePolygonCollection;
import org.kalypso.risk.model.schema.binding.RasterizationControlModel;
import org.kalypso.risk.model.schema.binding.WaterdepthCoverage;
import org.kalypso.risk.model.schema.binding.WaterdepthCoverageCollection;
import org.kalypso.risk.model.schema.binding.WaterdepthCoverageModel;
import org.kalypsodeegree.model.feature.Feature;

public class KalypsoRiskFeatureFactory implements IAdapterFactory
{
  interface AdapterConstructor
  {
    public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException;
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
        if( featureQName.equals( IRasterizationControlModel.QNAME ) )
          return new RasterizationControlModel( feature );
        else if( featureQName.equals( ILanduseModel.QNAME ) )
          return new LanduseModel( feature );
        else if( featureQName.equals( IWaterdepthCoverageModel.QNAME ) )
          return new WaterdepthCoverageModel( feature );
        else
          return null;
      }
    };
    cMap.put( IRasterizationControlModel.class, cTor );
    cMap.put( ILanduseModel.class, cTor );
    cMap.put( IWaterdepthCoverageModel.class, cTor );
    cMap.put( IModel.class, cTor );

    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();
        if( featureQName.equals( ILanduseClass.QNAME ) )
          return new LanduseClass( feature );
        else
          return null;
      }
    };
    cMap.put( ILanduseClass.class, cTor );

    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();
        if( featureQName.equals( ILanduseClassCollection.QNAME ) )
          return new LanduseClassCollection( feature );
        else
          return null;
      }
    };
    cMap.put( ILanduseClassCollection.class, cTor );

    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();
        if( featureQName.equals( ILandusePolygonCollection.QNAME ) )
          return new LandusePolygonCollection( feature );
        else
          return null;
      }
    };
    cMap.put( ILandusePolygonCollection.class, cTor );

    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();
        if( featureQName.equals( ILandusePolygon.QNAME ) )
          return new LandusePolygon( feature );
        else
          return null;
      }
    };
    cMap.put( ILandusePolygon.class, cTor );

    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();
        if( featureQName.equals( IWaterdepthCoverageCollection.QNAME ) )
          return new WaterdepthCoverageCollection( feature );
        else
          return null;
      }
    };
    cMap.put( IWaterdepthCoverageCollection.class, cTor );

    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();
        if( featureQName.equals( IWaterdepthCoverage.QNAME ) )
          return new WaterdepthCoverage( feature );
        else
          return null;
      }
    };
    cMap.put( IWaterdepthCoverage.class, cTor );

    return Collections.unmodifiableMap( cMap );
  }

}
