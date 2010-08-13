/**
 *
 */
package org.kalypso.risk.model.schema;

import java.util.Collections;
import java.util.Hashtable;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IAdapterFactory;
import org.kalypso.afgui.model.IModel;
import org.kalypso.risk.model.schema.binding.AdministrationUnit;
import org.kalypso.risk.model.schema.binding.AssetValueClass;
import org.kalypso.risk.model.schema.binding.DamageFunction;
import org.kalypso.risk.model.schema.binding.IAdministrationUnit;
import org.kalypso.risk.model.schema.binding.IAssetValueClass;
import org.kalypso.risk.model.schema.binding.IDamageFunction;
import org.kalypso.risk.model.schema.binding.ILanduseClass;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.schema.binding.ILandusePolygonCollection;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.schema.binding.IRiskLanduseStatistic;
import org.kalypso.risk.model.schema.binding.IRiskZoneDefinition;
import org.kalypso.risk.model.schema.binding.IVectorDataModel;
import org.kalypso.risk.model.schema.binding.LanduseClass;
import org.kalypso.risk.model.schema.binding.LandusePolygon;
import org.kalypso.risk.model.schema.binding.LandusePolygonCollection;
import org.kalypso.risk.model.schema.binding.RasterDataModel;
import org.kalypso.risk.model.schema.binding.RasterizationControlModel;
import org.kalypso.risk.model.schema.binding.RiskLanduseStatistic;
import org.kalypso.risk.model.schema.binding.RiskZoneDefinition;
import org.kalypso.risk.model.schema.binding.VectorDataModel;
import org.kalypsodeegree.model.feature.Feature;

public class KalypsoRiskFeatureFactory implements IAdapterFactory
{
  interface AdapterConstructor
  {
    public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException;
  }

  private final Map<Class< ? >, AdapterConstructor> m_constructors = createConstructorMap();

  public KalypsoRiskFeatureFactory( )
  {
  }

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
        if( featureQName.equals( IRasterizationControlModel.QNAME ) )
          return new RasterizationControlModel( feature );
        else if( featureQName.equals( IVectorDataModel.QNAME ) )
          return new VectorDataModel( feature );
        else if( featureQName.equals( IRasterDataModel.QNAME ) )
          return new RasterDataModel( feature );
        else
          return null;
      }
    };
    cMap.put( IRasterizationControlModel.class, cTor );
    cMap.put( IVectorDataModel.class, cTor );
    cMap.put( IRasterDataModel.class, cTor );
    cMap.put( IModel.class, cTor );

    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException
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
      @Override
      public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();
        if( featureQName.equals( IAssetValueClass.QNAME ) )
          return new AssetValueClass( feature );
        else
          return null;
      }
    };
    cMap.put( IAssetValueClass.class, cTor );

    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();
        if( featureQName.equals( IDamageFunction.QNAME ) )
          return new DamageFunction( feature );
        else
          return null;
      }
    };
    cMap.put( IDamageFunction.class, cTor );

    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();
        if( featureQName.equals( IAdministrationUnit.QNAME ) )
          return new AdministrationUnit( feature );
        else
          return null;
      }
    };
    cMap.put( IAdministrationUnit.class, cTor );

    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();
        if( featureQName.equals( IRiskZoneDefinition.QNAME ) )
          return new RiskZoneDefinition( feature );
        else
          return null;
      }
    };
    cMap.put( IRiskZoneDefinition.class, cTor );

    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException
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
      @Override
      public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException
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
      @Override
      public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();
        if( featureQName.equals( IRiskLanduseStatistic.QNAME ) )
          return new RiskLanduseStatistic( feature );
        else
          return null;
      }
    };
    cMap.put( IRiskLanduseStatistic.class, cTor );

    return Collections.unmodifiableMap( cMap );
  }

}
