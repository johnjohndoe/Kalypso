/**
 *
 */
package org.kalypso.kalypsomodel1d2d.schema.binding;

import java.util.Collections;
import java.util.Hashtable;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IAdapterFactory;
import org.kalypso.afgui.model.IModel;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.kalypsomodel1d2d.conv.results.IRestartInfo;
import org.kalypso.kalypsomodel1d2d.conv.results.RestartInfo;
import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.CalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.CalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.CalculationUnit2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ContinuityLine1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ContinuityLine2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.Element1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DDiscretisationModel;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IAbstractJunction;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IContinuityLine1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IContinuityLine2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IJunctionElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ITransitionElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.JunctionElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.PolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.TransitionElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.BoundaryCondition;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.BridgeFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBridgeFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IKingFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.ITeschkeFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IWeirFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IWeirFlowRelation2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.KingFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.TeschkeFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.WeirFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.WeirFlowRelation2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.ControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.ControlModel1D2DCollection;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.ControlModelGroup;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2DCollection;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModelGroup;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IStaticModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.StaticModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.CalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.DocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.StepResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.GMLNodeResult;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.Hydrograph;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.HydrographCollection;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.IHydrograph;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.IHydrographCollection;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResultCollection;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.NodeResultCollection;
import org.kalypso.kalypsomodel1d2d.ui.map.temsys.viz.ElevationModelDisplayElementFactory;
import org.kalypso.kalypsomodel1d2d.ui.map.temsys.viz.WindModelDisplayElementFactory;
import org.kalypso.kalypsosimulationmodel.core.flowrel.FlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.FlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;
import org.kalypso.kalypsosimulationmodel.core.roughness.RoughnessCls;
import org.kalypso.kalypsosimulationmodel.core.roughness.RoughnessClsCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.NativeTerrainElevationModelWrapper;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.TerrainModel;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindModel;
import org.kalypso.kalypsosimulationmodel.core.wind.WindModel;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelRoughnessConsts;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypsodeegree.graphics.displayelements.DisplayElementDecorator;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Adapter Factory for feature in the simBase namespace
 *
 * @author Patrice Congo
 *
 */
@SuppressWarnings("unchecked")
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
    public Object constructAdapter( Feature feature, Class cls ) throws IllegalArgumentException;
  }

  private final Map<Class, AdapterConstructor> constructors = createConstructorMap();

  public KalypsoModel1D2DFeatureFactory( )
  {
    // Empty
  }

  /**
   * @see org.eclipse.core.runtime.IAdapterFactory#getAdapter(java.lang.Object, java.lang.Class)
   */
  @Override
  public Object getAdapter( final Object adaptableObject, final Class adapterType )
  {
    if( !(adaptableObject instanceof Feature) )
    {
      throw new IllegalArgumentException( Messages.getString("org.kalypso.kalypsomodel1d2d.schema.binding.KalypsoModel1D2DFeatureFactory.0") + Messages.getString("org.kalypso.kalypsomodel1d2d.schema.binding.KalypsoModel1D2DFeatureFactory.1") + adaptableObject ); //$NON-NLS-1$ //$NON-NLS-2$
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
  public Class[] getAdapterList( )
  {
    return constructors.keySet().toArray( new Class[constructors.size()] );
  }

  private final Map<Class, AdapterConstructor> createConstructorMap( )
  {
    final Map<Class, AdapterConstructor> cMap = new Hashtable<Class, AdapterConstructor>();

    AdapterConstructor cTor;

    // IModel
    // Adapt all models to IModel
    // TODO: adapt other models than discretisation model and terrain model when needed
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final IFeatureType featureType = feature.getFeatureType();
        if( IFEDiscretisationModel1d2d.QNAME.equals( featureType.getQName() ) )
        {
          return new FE1D2DDiscretisationModel( feature );
        }
        else if( ITerrainModel.QNAME_TERRAIN_MODEL.equals( featureType.getQName() ) )
        {
          return new TerrainModel( feature );
        }
        else if( KalypsoModelRoughnessConsts.WBR_F_ROUGHNESS_CLS_COLLECTION.equals( featureType.getQName() ) )
        {
          return new RoughnessClsCollection( feature );
        }
        else
        {
          return null;
        }
      }
    };
    cMap.put( ITerrainModel.class, cTor );
    cMap.put( IModel.class, cTor );
    
    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final IFeatureType featureType = feature.getFeatureType();
       
        if( IWindModel.QNAME_WIND_MODEL.equals( featureType.getQName() ) )
        {
          return new WindModel( feature );
        }
        else
        {
          return null;
        }
      }
    };
    cMap.put( IWindModel.class, cTor );

    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final IFeatureType featureType = feature.getFeatureType();
        if( IRoughnessCls.QNAME.equals( featureType.getQName() ) )
          return new RoughnessCls( feature );
        else
          return null;
      }
    };
    cMap.put( IRoughnessCls.class, cTor );

    // IFE1D2DNode
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final IFeatureType featureType = feature.getFeatureType();
        if( Kalypso1D2DSchemaConstants.WB1D2D_F_NODE.equals( featureType.getQName() ) )
          return new FE1D2DNode( feature );

        return null;
      }
    };
    cMap.put( IFE1D2DNode.class, cTor );

    // IFE1D2DEdge
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( IFE1D2DEdge.QNAME ) )
        {
          return new FE1D2DEdge( feature );
        }
        return null;
      }
    };
    cMap.put( IFE1D2DEdge.class, cTor );

    // 1d2d element
    // registered for IFE1D2DElement.class but generates the most specific type
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        // TODO: also always check the class, else this can happen:
        // feature.getAdapter( IElement1D.class ) instanceof ILineElement
        // This is against the Adapter mechanism!

        if( featureQName.equals( IPolyElement.QNAME ) && cls.isAssignableFrom( IPolyElement.class ) )
          return new PolyElement( feature );
        else if( featureQName.equals( IElement1D.QNAME ) && cls.isAssignableFrom( IElement1D.class ) )
          return new Element1D( feature );
        else
        {
          return null;
        }
      }
    };
    // REMARK: do NOT register for the other classes as well
    // it is better to register them separate in order to make sure
    // that only the specific types are generated and null is returned if the types
    // does not fit (this is according to the adapter-contract)
    cMap.put( IFE1D2DElement.class, cTor );
    cMap.put( IPolyElement.class, cTor );
    cMap.put( IElement1D.class, cTor );

    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();
        if( featureQName.equals( IContinuityLine1D.QNAME ) && cls.isAssignableFrom( IContinuityLine1D.class ) )
        {
          return new ContinuityLine1D( feature );
        }
        else if( featureQName.equals( IContinuityLine2D.QNAME ) && cls.isAssignableFrom( IContinuityLine2D.class ) )
        {
          return new ContinuityLine2D( feature );
        }
        else
        {
          return null;
        }
      }
    };
    cMap.put( IFELine.class, cTor );
    cMap.put( IContinuityLine1D.class, cTor );
    cMap.put( IContinuityLine2D.class, cTor );

    // 1d2d complex element
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();
        if( featureQName.equals( IJunctionElement.QNAME ) )
        {
          return new JunctionElement( feature );
        }
        else if( featureQName.equals( ITransitionElement.QNAME ) )
        {
          return new TransitionElement( feature );
        }
        else if( featureQName.equals( ICalculationUnit1D.QNAME ) )
        {
          return new CalculationUnit1D( feature );
        }
        else if( featureQName.equals( ICalculationUnit2D.QNAME ) )
        {
          return new CalculationUnit2D( feature );
        }
        else if( featureQName.equals( ICalculationUnit1D2D.QNAME ) )
        {
          return new CalculationUnit1D2D( feature );
        }
        else
        {
          return null;
        }
      }
    };
    cMap.put( IFE1D2DComplexElement.class, cTor );
    cMap.put( IAbstractJunction.class, cTor );
    cMap.put( IJunctionElement.class, cTor );
    cMap.put( ITransitionElement.class, cTor );
    cMap.put( ICalculationUnit.class, cTor );
    cMap.put( ICalculationUnit1D.class, cTor );
    cMap.put( ICalculationUnit2D.class, cTor );
    cMap.put( ICalculationUnit1D2D.class, cTor );

    // DiscretisationModel
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( ControlModelGroup.WB1D2DCONTROL_F_MODEL_GROUP ) )
        {
          return new ControlModelGroup( feature );
        }
        else
        {
          return null;
        }
      }
    };
    cMap.put( IControlModelGroup.class, cTor );

    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( ControlModel1D2DCollection.WB1D2DCONTROL_F_MODEL_COLLECTION ) )
        {
          return new ControlModel1D2DCollection( feature );
        }
        else
        {
          return null;
        }
      }
    };
    cMap.put( IControlModel1D2DCollection.class, cTor );

    // DiscretisationModel
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( IFEDiscretisationModel1d2d.QNAME ) )
        {
          return new FE1D2DDiscretisationModel( feature );
        }
        else
        {
          return null;
        }
      }
    };
    cMap.put( IFEDiscretisationModel1d2d.class, cTor );

    // StaticModel1D2D
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( StaticModel1D2D.WB1D2D_F_STATIC_MODEL ) )
        {
          return new StaticModel1D2D( feature );
        }
        else
        {
          return null;
        }
      }
    };
    cMap.put( IStaticModel1D2D.class, cTor );

    // Flow relationship model
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( FlowRelationship.OP1D2D_F_FLOWRELATIONSHIPS_MODEL ) )
        {
          return new FlowRelationshipModel( feature );
        }
        else
        {
          return null;
        }
      }
    };
    cMap.put( IFlowRelationshipModel.class, cTor );

    // ControlModel
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();
        if( featureQName.equals( ControlModel1D2D.WB1D2DCONTROL_F_MODEL ) )
          return new ControlModel1D2D( feature );
        else
        {
          return null;
        }
      }
    };
    cMap.put( IControlModel1D2D.class, cTor );

    // IDisplayElement
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName name = feature.getFeatureType().getQName();
        if( NativeTerrainElevationModelWrapper.SIM_BASE_F_NATIVE_TERRAIN_ELE_WRAPPER.equals( name ) )
        {
          return ElevationModelDisplayElementFactory.createDisplayElement( feature );
        }
        else if( KalypsoModelSimulationBaseConsts.SIM_BASE_F_NATIVE_WIND_ELE_WRAPPER.equals( name ) )
        {
          return WindModelDisplayElementFactory.createDisplayElement( feature );
        }
        else if( KalypsoModelSimulationBaseConsts.SIM_BASE_F_WIND_ELE_SYS.equals( name ) )
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

    // IFlowRelation
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        // If a generel flow relation is to be adapted, return the konkrete type instead
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( IKingFlowRelation.QNAME ) )
        {
          return new KingFlowRelation( feature );
        }
        else if( featureQName.equals( ITeschkeFlowRelation.QNAME ) )
        {
          return new TeschkeFlowRelation( feature );
        }
        else if( featureQName.equals( IWeirFlowRelation.QNAME ) )
        {
          return new WeirFlowRelation( feature );
        }
        else if( featureQName.equals( IWeirFlowRelation2D.QNAME ) )
        {
          return new WeirFlowRelation2D( feature );
        }
        else if( featureQName.equals( IBridgeFlowRelation.QNAME ) )
        {
          return new BridgeFlowRelation( feature );
        }
        else if( featureQName.equals( IBoundaryCondition.QNAME ) )
        {
          return new BoundaryCondition( feature );
        }
        else
        {
          return null;
        }
      }
    };
    cMap.put( IFlowRelationship.class, cTor );

    // IBuildingFlowRelation
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        // If a generel flow relation is to be adapted, return the konkrete type instead
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( IWeirFlowRelation.QNAME ) )
        {
          return new WeirFlowRelation( feature );
        }
        else if( featureQName.equals( IBridgeFlowRelation.QNAME ) )
        {
          return new BridgeFlowRelation( feature );
        }
        else
        {
          return null;
        }
      }
    };
    cMap.put( IBuildingFlowRelation.class, cTor );
    
    // IBuildingFlowRelation2D
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        // If a generel flow relation is to be adapted, return the konkrete type instead
        final QName featureQName = feature.getFeatureType().getQName();
        
        
        if( featureQName.equals( IWeirFlowRelation2D.QNAME ) )
        {
          return new WeirFlowRelation2D( feature );
        }
        else
        {
          return null;
        }
      }
    };
    cMap.put( IBuildingFlowRelation2D.class, cTor );

    // KingFlowRelation
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( IKingFlowRelation.QNAME ) )
        {
          return new KingFlowRelation( feature );
        }
        else
        {
          return null;
        }
      }
    };
    cMap.put( IKingFlowRelation.class, cTor );

    // TeschkeFlowRelation
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( ITeschkeFlowRelation.QNAME ) )
        {
          return new TeschkeFlowRelation( feature );
        }
        else
        {
          return null;
        }
      }
    };
    cMap.put( ITeschkeFlowRelation.class, cTor );

    // WeirFlowRelation
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( IWeirFlowRelation.QNAME ) )
        {
          return new WeirFlowRelation( feature );
        }
        else
        {
          return null;
        }
      }
    };
    cMap.put( IWeirFlowRelation.class, cTor );
    
    // WeirFlowRelation2D
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();
        
        if( featureQName.equals( IWeirFlowRelation2D.QNAME ) )
        {
          return new WeirFlowRelation2D( feature );
        }
        else
        {
          return null;
        }
      }
    };
    cMap.put( IWeirFlowRelation2D.class, cTor );

    // BridgeFlowRelation
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( IBridgeFlowRelation.QNAME ) )
        {
          return new BridgeFlowRelation( feature );
        }
        else
        {
          return null;
        }
      }
    };
    cMap.put( IBridgeFlowRelation.class, cTor );

    // IResultMeta
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        // If a general result meta is to be adapted, return the concrete type instead
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( IScenarioResultMeta.QNAME ) )
        {
          return new ScenarioResultMeta( feature );
        }
        else if( featureQName.equals( ICalcUnitResultMeta.QNAME ) )
        {
          return new CalcUnitResultMeta( feature );
        }
        else if( featureQName.equals( IStepResultMeta.QNAME ) )
        {
          return new StepResultMeta( feature );
        }
        else if( featureQName.equals( IDocumentResultMeta.QNAME ) )
        {
          return new DocumentResultMeta( feature );
        }
        else
        {
          return null;
        }
      }
    };
    cMap.put( IResultMeta.class, cTor );

    // ScenarioResultMeta
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( IScenarioResultMeta.QNAME ) )
        {
          return new ScenarioResultMeta( feature );
        }
        else
        {
          return null;
        }
      }
    };
    cMap.put( IScenarioResultMeta.class, cTor );

    // CalcUnitResultMeta
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( ICalcUnitResultMeta.QNAME ) )
        {
          return new CalcUnitResultMeta( feature );
        }
        else
        {
          return null;
        }
      }
    };
    cMap.put( ICalcUnitResultMeta.class, cTor );

    // StepResultMeta
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( IStepResultMeta.QNAME ) )
        {
          return new StepResultMeta( feature );
        }
        else
        {
          return null;
        }
      }
    };
    cMap.put( IStepResultMeta.class, cTor );

    // DocumentResultMeta
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( IDocumentResultMeta.QNAME ) )
        {
          return new DocumentResultMeta( feature );
        }
        else
        {
          return null;
        }
      }
    };
    cMap.put( IDocumentResultMeta.class, cTor );

    // RestartInfo
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( IRestartInfo.QNAME ) )
        {
          return new RestartInfo( feature );
        }
        else
        {
          return null;
        }
      }
    };
    cMap.put( IRestartInfo.class, cTor );

    // BoundaryCondition
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( IBoundaryCondition.QNAME ) )
        {
          return new BoundaryCondition( feature );
        }
        else
        {
          return null;
        }
      }
    };
    cMap.put( IBoundaryCondition.class, cTor );

    // hydrograph collection
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( IHydrographCollection.QNAME ) )
        {
          return new HydrographCollection( feature );
        }
        else
        {
          return null;
        }
      }
    };
    cMap.put( IHydrographCollection.class, cTor );

    // hydrograph
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( IHydrograph.QNAME ) )
        {
          return new Hydrograph( feature );
        }
        else
        {
          return null;
        }
      }
    };
    cMap.put( IHydrograph.class, cTor );

    // Node Results
    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( INodeResult.QNAME ) )
        {
          return new GMLNodeResult( feature );
        }
        else
        {
          return null;
        }
      }
    };
    cMap.put( INodeResult.class, cTor );

    cTor = new AdapterConstructor()
    {
      @Override
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( INodeResultCollection.QNAME ) )
        {
          return new NodeResultCollection( feature );
        }
        else
        {
          return null;
        }
      }
    };
    cMap.put( INodeResultCollection.class, cTor );

    return Collections.unmodifiableMap( cMap );
  }
}
