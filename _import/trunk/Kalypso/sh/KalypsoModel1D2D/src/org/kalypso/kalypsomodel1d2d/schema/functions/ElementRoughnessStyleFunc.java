package org.kalypso.kalypsomodel1d2d.schema.functions;

import java.util.HashMap;
import java.util.Map;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsosimulationmodel.core.Util;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessEstimateSpec;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygonCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;

/**
 * @author Patrice Congo
 */
public class ElementRoughnessStyleFunc extends FeaturePropertyFunction
{
  private static boolean activated = true;
  
  private static final Map<String, IRoughnessCls> roughnessMap = 
                                    new HashMap<String, IRoughnessCls>();
  private static final String DEFAULT_STYLE = "_DEFAULT_STYLE_";
  
  private static IRoughnessPolygonCollection roughnessPolygonCollection;
  
  
  
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
    IPolyElement polyElement = 
      (IPolyElement) feature.getAdapter( IPolyElement.class );
    return getElementRoughnessCls( polyElement );
  }
  
  private IRoughnessCls getElementRoughnessCls( IPolyElement polyElement )
  {
    if( !isActivated() )
    {
      return null;
    }
    
    reinitRoughnessModel();
    if( roughnessPolygonCollection == null )
    {
      System.out.println( "roughness Collection is null" );
      return null;
    }
    
    try
    {
      if( polyElement == null )
      {
        System.out.println( "not a polyelement" );
        return null;
      }
      final String polyElementID = polyElement.getGmlID();
      IRoughnessCls clsName = roughnessMap.get( polyElementID ); 
      if ( clsName != null )
      {
        //already computed
        System.out.println("From Cache "+ polyElementID + clsName);
        return clsName;
      }
      else
      {
        clsName = null;
        if( !roughnessPolygonCollection.isEmpty() )
        {
          IRoughnessEstimateSpec roughnessEstimateSpec = 
                    roughnessPolygonCollection.getRoughnessEstimateSpec( 
                                            polyElement.recalculateElementGeometry() );
          IRoughnessCls[] classes = roughnessEstimateSpec.mostSpreadRoughness();
          if( classes.length >0 )
          {
            clsName = classes[0];
                        
          }
          
        }
        System.out.println( "StyleName=" + clsName );
        roughnessMap.put( polyElementID, clsName );
        return clsName;  
      }
    }
    catch( GM_Exception e )
    {
      e.printStackTrace();
      return null;
    }
    
  }
  

//  private String getElementRoughnessStyle( IPolyElement polyElement )
//  {
//    if( !isActivated() )
//    {
//      return DEFAULT_STYLE;
//    }
//    
//    reinitRoughnessModel();
//    if( roughnessPolygonCollection == null )
//    {
//      System.out.println( "roughness Collection is null" );
//      return DEFAULT_STYLE;
//    }
//    
//    try
//    {
//      if( polyElement == null )
//      {
//        System.out.println( "not a polyelement" );
//        return null;
//      }
//      final String polyElementID = polyElement.getGmlID();
//      String clsName = roughnessMap.get( polyElementID );
//      if ( clsName != null )
//      {
//        //already computed
//        System.out.println("From Cache "+ polyElementID + clsName);
//        return clsName;
//      }
//      else
//      {
//        clsName = DEFAULT_STYLE;
//        if( !roughnessPolygonCollection.isEmpty() )
//        {
//          IRoughnessEstimateSpec roughnessEstimateSpec = 
//                    roughnessPolygonCollection.getRoughnessEstimateSpec( 
//                                            polyElement.recalculateElementGeometry() );
//          IRoughnessCls[] classes = roughnessEstimateSpec.mostSpreadRoughness();
//          if( classes.length >0 )
//          {
//            IRoughnessCls roughnessCls = classes[0];
//            if( roughnessCls != null )
//            {
//              clsName = roughnessCls.getName( );
//            }            
//          }
//          
//        }
//        System.out.println( "StyleName=" + clsName );
//        roughnessMap.put( polyElementID, clsName );
//        return clsName;  
//      }
//    }
//    catch( GM_Exception e )
//    {
//      e.printStackTrace();
//      return "_DEFAULT_STYLE_";
//    }
//    
//  }
  
  public static final void  reinitRoughnessModel()
  {
     if( roughnessPolygonCollection == null )
     {
       
      // reinit the roughness polygon layer 
      ITerrainModel terrainModel = Util.getModel( ITerrainModel.class );
      if( terrainModel == null )
      {
        roughnessPolygonCollection = null;
      }
      else
      {
        roughnessPolygonCollection = 
            terrainModel.getRoughnessPolygonCollection();        
      }
      
     }
      
  }
  
  public static final void clear(  )
  {
    System.out.println("Clear the roughness mapping");
    synchronized( roughnessMap )
    {
      roughnessMap.clear();
    }
  }
  
  public static final void removeRoughnessClass( Feature polyElementFeature )
  {
    final String id = polyElementFeature.getId();
    System.out.println( "Removing " + id );
    synchronized( roughnessMap )
    {
      roughnessMap.remove( id );
    }
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

//  public static final  <T extends IFeatureWrapper2> T getModel(Class<T> modelClass)
//  {
//    try
//    {
//      IWorkbench workbench = PlatformUI.getWorkbench();
//      IHandlerService  service = 
//          (IHandlerService) workbench.getService( IHandlerService.class );
//      IEvaluationContext currentState = service.getCurrentState();
//      ICaseDataProvider<IFeatureWrapper2> caseDataProvider =
//          (ICaseDataProvider<IFeatureWrapper2>) 
//            currentState.getVariable( ISzenarioSourceProvider.ACTIVE_SZENARIO_DATA_PROVIDER_NAME );
//      T model =
//        caseDataProvider.getModel( modelClass );
//      
//      return model;
//    }
//    catch ( Throwable th ) 
//    {
//      th.printStackTrace();
//      return null;
//    }
//  }
  
  public static final boolean isActivated()
  {
    
    synchronized( roughnessMap )
    {
      return activated;
    }
  }
  
  public static final void setActivated(boolean newActivationState)
  {
    
    synchronized( roughnessMap )
    {
      activated = newActivationState;
    }
  }
}
