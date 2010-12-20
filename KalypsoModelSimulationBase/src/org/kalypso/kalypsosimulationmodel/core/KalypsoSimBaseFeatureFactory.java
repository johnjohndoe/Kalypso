/**
 * 
 */
package org.kalypso.kalypsosimulationmodel.core;

import java.util.Collections;
import java.util.Hashtable;
import java.util.Map;

import org.eclipse.core.runtime.IAdapterFactory;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.kalypsosimulationmodel.core.flowrel.FlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection;
import org.kalypso.kalypsosimulationmodel.core.roughness.RoughnessCls;
import org.kalypso.kalypsosimulationmodel.core.roughness.RoughnessClsCollection;
import org.kalypso.kalypsosimulationmodel.core.roughness.RoughnessClsCorrection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetwork;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetworkCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessLayer;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygonCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModelSystem;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.NativeTerrainElevationModelWrapper;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.RiverProfileNetwork;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.RiverProfileNetworkCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.RoughnessLayer;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.RoughnessPolygon;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.RoughnessPolygonCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.TerrainElevationModelSystem;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.TerrainModel;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindDataModel;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindDataModelSystem;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindModel;
import org.kalypso.kalypsosimulationmodel.core.wind.NativeWindDataModelWrapper;
import org.kalypso.kalypsosimulationmodel.core.wind.WindDataModelSystem;
import org.kalypso.kalypsosimulationmodel.core.wind.WindModel;
import org.kalypso.kalypsosimulationmodel.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Adapter Factory for feature in the simBase namespace
 * 
 * @author Patrice Congo
 * 
 */
@SuppressWarnings("unchecked")
public class KalypsoSimBaseFeatureFactory implements IAdapterFactory
{
  interface AdapterConstructor
  {
    /**
     * Construct the Adapter of the specified class for the given feature
     * 
     * @param <T>
     * @param feature
     * @param cls
     * @return
     * @throws IllegalArgumentException
     *             if
     *             <ul>
     *             <li/>feature or cls is null <li/>feature cannot be converted
     *             </ul>
     */
    public Object constructAdapter( Feature feature, Class< ? > cls ) throws IllegalArgumentException;
  }

  private final Map<Class< ? >, AdapterConstructor> constructors = createConstructorMap();

  public KalypsoSimBaseFeatureFactory( )
  {
    // Empty
  }

  /**
   * @see org.eclipse.core.runtime.IAdapterFactory#getAdapter(java.lang.Object, java.lang.Class)
   */
  @Override
  public Object getAdapter( final Object adaptableObject, @SuppressWarnings("rawtypes") final Class adapterType )
  {
    if( !(adaptableObject instanceof Feature) )
    {
      throw new IllegalArgumentException( Messages.getString("org.kalypso.kalypsosimulationmodel.core.KalypsoSimBaseFeatureFactory.3") + Messages.getString("org.kalypso.kalypsosimulationmodel.core.KalypsoSimBaseFeatureFactory.4") + adaptableObject ); //$NON-NLS-1$ //$NON-NLS-2$
    }

    final AdapterConstructor ctor = constructors.get( adapterType );
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
    return constructors.keySet().toArray( new Class[constructors.size()] );
  }

  private final Map<Class< ? >, AdapterConstructor> createConstructorMap( )
  {
    final Map<Class< ? >, AdapterConstructor> cMap = new Hashtable<Class< ? >, AdapterConstructor>();

    // terrain model
    AdapterConstructor cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException
      {

        return new TerrainModel( feature );
      }
    };
    cMap.put( ITerrainModel.class, cTor );

    // wind model
    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( Feature feature, Class cls ) throws IllegalArgumentException
      {
        
        return new WindModel( feature );
      }
    };
    cMap.put( IWindModel.class, cTor );

    // IRoughnessCls
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException
      {
        return new RoughnessCls( feature );
      }
    };
    cMap.put( IRoughnessCls.class, cTor );

    // IRoughnessCls
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException
      {
        try
        {
          return new RoughnessClsCollection( feature );
        }
        catch( final Throwable th )
        {
          th.printStackTrace();
          return null;
        }
      }
    };
    cMap.put( IRoughnessClsCollection.class, cTor );

    // IRoughnessClsCorrection
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException
      {
        return new RoughnessClsCorrection( feature );
      }
    };
    cMap.put( IRoughnessClsCorrection.class, cTor );

    // IRoughnessPolygon
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException
      {
        return new RoughnessPolygon( feature );
      }
    };
    cMap.put( IRoughnessPolygon.class, cTor );

    // IRoughnessLayer
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException
      {
        return new RoughnessLayer( feature );
      }
    };
    cMap.put( IRoughnessLayer.class, cTor );

    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException
      {
        return new RoughnessPolygonCollection( feature );
      }
    };
    cMap.put( IRoughnessPolygonCollection.class, cTor );

    // IRiverProfileNetworkCollection
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException
      {
        return new RiverProfileNetworkCollection( feature );
      }
    };
    cMap.put( IRiverProfileNetworkCollection.class, cTor );

    // IRiverProfileNetwork
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException
      {
        return new RiverProfileNetwork( feature );
      }
    };
    cMap.put( IRiverProfileNetwork.class, cTor );

    // ITerrainElevationModel
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException
      {
        // TODO provide adapaterfac method for the elevation model
        if( GMLSchemaUtilities.substitutes( feature.getFeatureType(), NativeTerrainElevationModelWrapper.SIM_BASE_F_NATIVE_TERRAIN_ELE_WRAPPER ) )
        {
          try
          {
            return new NativeTerrainElevationModelWrapper( feature );
          }
          catch( final Throwable th )
          {
            throw new IllegalArgumentException( Messages.getString( "org.kalypso.kalypsosimulationmodel.core.KalypsoSimBaseFeatureFactory.6" ) + Messages.getString( "org.kalypso.kalypsosimulationmodel.core.KalypsoSimBaseFeatureFactory.7" ) + feature + Messages.getString( "org.kalypso.kalypsosimulationmodel.core.KalypsoSimBaseFeatureFactory.10" ) + cls, th ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
          }

        }
        else if( GMLSchemaUtilities.substitutes( feature.getFeatureType(), TerrainElevationModelSystem.SIM_BASE_F_TERRAIN_ELE_SYS ) )
        {
          try
          {
            return new TerrainElevationModelSystem( feature );
          }
          catch( final Throwable th )
          {
            throw new IllegalArgumentException( Messages.getString( "org.kalypso.kalypsosimulationmodel.core.KalypsoSimBaseFeatureFactory.11" ) + Messages.getString( "org.kalypso.kalypsosimulationmodel.core.KalypsoSimBaseFeatureFactory.12" ) + feature + Messages.getString( "org.kalypso.kalypsosimulationmodel.core.KalypsoSimBaseFeatureFactory.13" ) + cls, th ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
          }

        }
        else
        {
          System.out.println( "Could not adapt=" + feature + " to" + cls ); //$NON-NLS-1$ //$NON-NLS-2$
          return null;// new TerrainElevationModel(feature);
        }
      }
    };
    cMap.put( ITerrainElevationModel.class, cTor );
    cMap.put( ITerrainElevationModelSystem.class, cTor );

    // ITerrainModel
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException
      {
        return new TerrainModel( feature );
      }
    };
    cMap.put( ITerrainModel.class, cTor );
    
    // IWindModel
    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( Feature feature, Class cls ) throws IllegalArgumentException
      {
        if( GMLSchemaUtilities.substitutes( feature.getFeatureType(), KalypsoModelSimulationBaseConsts.SIM_BASE_F_NATIVE_WIND_ELE_WRAPPER ) )
        {
          try
          {
            return new NativeWindDataModelWrapper( feature );
          }
          catch( Throwable th )
          {
            throw new IllegalArgumentException( Messages.getString("org.kalypso.kalypsosimulationmodel.core.KalypsoSimBaseFeatureFactory.6") + Messages.getString("org.kalypso.kalypsosimulationmodel.core.KalypsoSimBaseFeatureFactory.7") + feature + Messages.getString("org.kalypso.kalypsosimulationmodel.core.KalypsoSimBaseFeatureFactory.10") + cls, th ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
          }
          
        }
//        else if( GMLSchemaUtilities.substitutes( feature.getFeatureType(), KalypsoModelSimulationBaseConsts.SIM_BASE_F_WIND_ELE_SYS ) )
//        {
//          try
//          {
//            return new WindDataModelSystem( feature );
//          }
//          catch( Throwable th )
//          {
//            throw new IllegalArgumentException( Messages.getString("org.kalypso.kalypsosimulationmodel.core.KalypsoSimBaseFeatureFactory.11") + Messages.getString("org.kalypso.kalypsosimulationmodel.core.KalypsoSimBaseFeatureFactory.12") + feature + Messages.getString("org.kalypso.kalypsosimulationmodel.core.KalypsoSimBaseFeatureFactory.13") + cls, th ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
//          }
//          
//        }
        else
        {
          System.out.println( "Could not adapt=" + feature + " to" + cls ); //$NON-NLS-1$ //$NON-NLS-2$
          return null;
        }
      }
    };
    cMap.put( IWindDataModel.class, cTor );
    
    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( Feature feature, Class cls ) throws IllegalArgumentException
      {
        if( GMLSchemaUtilities.substitutes( feature.getFeatureType(), KalypsoModelSimulationBaseConsts.SIM_BASE_F_WIND_ELE_SYS ) )
        {
          try
          {
            return new WindDataModelSystem( feature );
          }
          catch( Throwable th )
          {
            throw new IllegalArgumentException( Messages.getString("org.kalypso.kalypsosimulationmodel.core.KalypsoSimBaseFeatureFactory.11") + Messages.getString("org.kalypso.kalypsosimulationmodel.core.KalypsoSimBaseFeatureFactory.12") + feature + Messages.getString("org.kalypso.kalypsosimulationmodel.core.KalypsoSimBaseFeatureFactory.13") + cls, th ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
          }
          
        }
        else
        {
          System.out.println( "Could not adapt=" + feature + " to" + cls ); //$NON-NLS-1$ //$NON-NLS-2$
          return null;
        }
      }
    };
    cMap.put( IWindDataModelSystem.class, cTor );
    
    // IWindModel
    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( Feature feature, Class cls ) throws IllegalArgumentException
      {
        return new WindModel( feature );
      }
    };
    cMap.put( IWindModel.class, cTor );

    // IFlowRelationshipModel
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException
      {
        return new FlowRelationshipModel( feature );
      }
    };
    cMap.put( IFlowRelationshipModel.class, cTor );

    return Collections.unmodifiableMap( cMap );
  }
}
