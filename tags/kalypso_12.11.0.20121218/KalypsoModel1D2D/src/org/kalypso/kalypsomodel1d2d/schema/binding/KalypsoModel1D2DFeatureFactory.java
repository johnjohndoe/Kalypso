/**
 *
 */
package org.kalypso.kalypsomodel1d2d.schema.binding;

import java.util.Collections;
import java.util.Hashtable;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IAdapterFactory;
import org.kalypso.kalypsomodel1d2d.ui.map.temsys.viz.ElevationModelDisplayElementFactory;
import org.kalypso.kalypsomodel1d2d.ui.map.temsys.viz.WindModelDisplayElementFactory;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.HMOTerrainElevationModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.NativeTerrainElevationModelWrapper;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindDataModelSystem;
import org.kalypsodeegree.graphics.displayelements.DisplayElementDecorator;
import org.kalypsodeegree.model.elevation.IElevationModel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.graphics.displayelements.IElevationColorModel;
import org.kalypsodeegree_impl.graphics.displayelements.TriangulatedSurfacePolygonDisplayElement;

/**
 * Adapter Factory for feature in the simBase namespace
 * 
 * @author Patrice Congo
 */
@SuppressWarnings( "rawtypes" )
public class KalypsoModel1D2DFeatureFactory implements IAdapterFactory
{
  private interface AdapterConstructor
  {
    /**
     * Construct the Adapter of the specified class for the given feature
     * 
     * @param <T>
     * @param feature
     * @param cls
     * @return
     * @throws IllegalArgumentException
     *           if
     *           <ul>
     *           <li/>feature or cls is null
     *           <li/>feature cannnot be converted
     *           </ul>
     */
    public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException;
  }

  private final Map<Class, AdapterConstructor> constructors = createConstructorMap();

  public KalypsoModel1D2DFeatureFactory( )
  {
    // Empty
  }

  @Override
  public Object getAdapter( final Object adaptableObject, final Class adapterType )
  {
    if( !(adaptableObject instanceof Feature) )
      throw new IllegalArgumentException( "Adapter Factory for feature only but get to adapt:" + adaptableObject ); //$NON-NLS-1$

    final AdapterConstructor ctor = constructors.get( adapterType );
    if( ctor == null )
      return null;

    return ctor.constructAdapter( (Feature)adaptableObject, adapterType );
  }

  @Override
  public Class< ? >[] getAdapterList( )
  {
    return constructors.keySet().toArray( new Class[constructors.size()] );
  }

  private final Map<Class, AdapterConstructor> createConstructorMap( )
  {
    final Map<Class, AdapterConstructor> cMap = new Hashtable<>();

    AdapterConstructor cTor;

    // IDisplayElement
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName name = feature.getFeatureType().getQName();
        if( NativeTerrainElevationModelWrapper.SIM_BASE_F_NATIVE_TERRAIN_ELE_WRAPPER.equals( name ) )
        {
          final ITerrainElevationModel terrainElevationModel = (ITerrainElevationModel)feature.getAdapter( ITerrainElevationModel.class );
          final IElevationModel elevationProvider = ((NativeTerrainElevationModelWrapper)terrainElevationModel).getElevationProvider();

          if( elevationProvider instanceof HMOTerrainElevationModel )
          {
            final IElevationColorModel colorModel = ElevationModelDisplayElementFactory.createColorModel( terrainElevationModel, null );
            return new TriangulatedSurfacePolygonDisplayElement( feature, terrainElevationModel, colorModel );
          }

          return ElevationModelDisplayElementFactory.createDisplayElement( feature );
        }
        // FIXME: constant does not exist any more -> does wind work??
        // else if( KalypsoModelSimulationBaseConsts.SIM_BASE_F_NATIVE_WIND_ELE_WRAPPER.equals( name ) )
        // {
        // return WindModelDisplayElementFactory.createDisplayElement( feature );
        // }
        else if( IWindDataModelSystem.SIM_BASE_F_WIND_ELE_SYS.equals( name ) )
        {
          return WindModelDisplayElementFactory.createDisplayElement( feature );
        }
        else
        {
          return null;
        }
      }
    };
    cMap.put( DisplayElementDecorator.class, cTor );

    return Collections.unmodifiableMap( cMap );
  }
}