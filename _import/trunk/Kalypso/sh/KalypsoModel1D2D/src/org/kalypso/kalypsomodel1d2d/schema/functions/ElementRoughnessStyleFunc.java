package org.kalypso.kalypsomodel1d2d.schema.functions;

import java.util.Map;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IStaticModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.StaticModel1D2D;
import org.kalypso.kalypsosimulationmodel.core.modeling.ISimulationModel;
import org.kalypso.kalypsosimulationmodel.core.modeling.IStaticModel;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessEstimateSpec;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygonCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.ui.wizards.imports.ISzenarioSourceProvider;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;

import de.renew.workflow.cases.ICaseDataProvider;

/**
 * @author Patrice Congo
 */
public class ElementRoughnessStyleFunc extends FeaturePropertyFunction
{
  
  /**
   * @see org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction#init(java.util.Map)
   */
  @Override
  public void init( Map<String, String> properties )
  {
    // nothing to do
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#getValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public Object getValue( 
            final Feature feature, 
            final IPropertyType pt, 
            final Object currentValue )
  {
    try
    {
      IPolyElement polyElement = 
          (IPolyElement) feature.getAdapter( IPolyElement.class );
      if( polyElement == null )
      {
        return null;
      }
      
      ITerrainModel terrainModel = getModel( ITerrainModel.class );
      if( terrainModel == null )
      {
        return null;
      }
      IRoughnessPolygonCollection roughnessPolygonCollection = 
                              terrainModel.getRoughnessPolygonCollection();
        IRoughnessEstimateSpec roughnessEstimateSpec = 
          roughnessPolygonCollection.getRoughnessEstimateSpec( 
                                  polyElement.recalculateElementGeometry() );
        IRoughnessCls[] classes = roughnessEstimateSpec.mostSpreadRoughness();
        if( classes.length >0 )
        {
          IRoughnessCls roughnessCls = classes[0];
          if( roughnessCls == null )
          {
            System.out.println("StyleName=_DEFAULT_STYLE_");
            return "_DEFAULT_STYLE_";
          }
          else
          {
            String name = roughnessCls.getName( );
            System.out.println( "StyleName=" + name );
            return name;
            
          }
        }
        else
        {
          return null;
        }
    }
    catch( GM_Exception e )
    {
      e.printStackTrace();
      return null;
    }
    
//    ISimulationModel simulationModel = getModel( ISimulationModel.class );
//    if( simulationModel == null )
//    {
//      return null;
//    }
//    IStaticModel staticModel = simulationModel.getStaticModel();
//    if( !(staticModel instanceof IStaticModel1D2D ) )
//    {
//      ((IStaticModel1D2D)staticModel).getTerrainModel()
//    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#setValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public Object setValue( 
                final Feature feature, 
                final IPropertyType pt, 
                final Object valueToSet )
  {
    return null;
  }

  private static final  <T extends IFeatureWrapper2> T getModel(Class<T> modelClass)
  {
    try
    {
      IWorkbench workbench = PlatformUI.getWorkbench();
      IHandlerService  service = 
          (IHandlerService) workbench.getService( IHandlerService.class );
      IEvaluationContext currentState = service.getCurrentState();
      ICaseDataProvider<IFeatureWrapper2> caseDataProvider =
          (ICaseDataProvider<IFeatureWrapper2>) 
            currentState.getVariable( ISzenarioSourceProvider.ACTIVE_SZENARIO_DATA_PROVIDER_NAME );
      T model =
        caseDataProvider.getModel( modelClass );
      
      return model;
    }
    catch ( Throwable th ) 
    {
      th.printStackTrace();
      return null;
    }
  }
}
