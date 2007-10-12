/**
 *
 */
package org.kalypso.risk.model.schema.binding;

import java.util.Collections;
import java.util.Hashtable;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IAdapterFactory;
import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;
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
        if( ILanduseModel.QNAME.equals( featureQName ) )
          return new LanduseModel( feature );
        // else if( ITerrainModel.QNAME_TERRAIN_MODEL.equals( featureType.getQName() ) )
        // return new TerrainModel( feature );
        // else if( KalypsoModelRoughnessConsts.WBR_F_ROUGHNESS_CLS_COLLECTION.equals( featureType.getQName() ) )
        // return new RoughnessClsCollection( feature );
        else
          return null;
      }
    };
    cMap.put( IModel.class, cTor );

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

    return Collections.unmodifiableMap( cMap );
  }

}
