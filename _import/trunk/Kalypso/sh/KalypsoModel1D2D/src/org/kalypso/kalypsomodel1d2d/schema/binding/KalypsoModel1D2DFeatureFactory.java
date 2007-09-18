/**
 *
 */
package org.kalypso.kalypsomodel1d2d.schema.binding;

import java.util.Collections;
import java.util.Hashtable;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IAdapterFactory;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.kalypsomodel1d2d.conv.results.IRestartInfo;
import org.kalypso.kalypsomodel1d2d.conv.results.RestartInfo;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.CalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.CalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.CalculationUnit2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ContinuityLine1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ContinuityLine2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.EdgeInv;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.Element1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DDiscretisationModel;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FEMiddleNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IContinuityLine1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IContinuityLine2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IEdgeInv;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEMiddleNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IRiverChannel1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ITransitionElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.PolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.RiverChannel1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.TransitionElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.BoundaryCondition;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.BridgeFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBridgeFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IKingFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.ITeschkeFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IWeirFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.KingFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.TeschkeFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.WeirFlowRelation;
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
import org.kalypso.kalypsomodel1d2d.schema.binding.results.Hydrograph;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.HydrographCollection;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.IHydrograph;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.IHydrographCollection;
import org.kalypso.kalypsomodel1d2d.ui.map.temsys.viz.ElevationModelDisplayElementFactory;
import org.kalypso.kalypsosimulationmodel.core.flowrel.FlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.kalypsosimulationmodel.core.roughness.RoughnessClsCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.TerrainModel;
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
  // TODO: use the Debug helper class instead
  // private static final Logger logger = Logger.getLogger( KalypsoModel1D2DFeatureFactory.class.toString() );

  public static final void warnUnableToAdapt( final Feature featureToAdapt, final QName featureQName, final Class targetClass )
  {
    // System.out.println("Unable to adapt "+featureToAdapt.getFeatureType().getQName()+" to "+targetClass.getName());
  }

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
     *             <li/>feature or cls is null <li/>feature cannnot be converted
     *             </ul>
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
  public Object getAdapter( final Object adaptableObject, final Class adapterType )
  {
    if( !(adaptableObject instanceof Feature) )
    {
      throw new IllegalArgumentException( "Adapter Factory for feature only but" + " get to adapt:" + adaptableObject );
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
    // TODO: adapt other models than discretisation modell and terrain modell when needed
    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final IFeatureType featureType = feature.getFeatureType();
        if( Kalypso1D2DSchemaConstants.WB1D2D_F_DiscretisationModel.equals( featureType.getQName() ) )
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
    cMap.put( IModel.class, cTor );

    // IFE1D2DNode
    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final IFeatureType featureType = feature.getFeatureType();
        if( Kalypso1D2DSchemaConstants.WB1D2D_F_MIDDLE_NODE.equals( featureType.getQName() ) )
        {
          return new FEMiddleNode( feature );
        }
        else if( Kalypso1D2DSchemaConstants.WB1D2D_F_NODE.equals( featureType.getQName() ) )
        {
          return new FE1D2DNode( feature );
        }
        else
        {
          return null;
        }
      }
    };
    cMap.put( IFE1D2DNode.class, cTor );
    cMap.put( IFEMiddleNode.class, cTor );

    // IFE1D2DEdge
    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE_INV ) )
        {
          return new EdgeInv( feature );
        }
        else if( featureQName.equals( IFE1D2DEdge.QNAME ) )
        {
          return new FE1D2DEdge( feature );
        }
        else
        {
          warnUnableToAdapt( feature, featureQName, IFE1D2DEdge.class );
          return null;
        }
      }
    };
    cMap.put( IFE1D2DEdge.class, cTor );
    cMap.put( IEdgeInv.class, cTor );

    // 1d2d element
    // registered for IFE1D2DElement.class but generates the most specific type
    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        // TODO: also always check the class, else this can happen:
        // feature.getAdapter( IElement1D.class ) instanceof ILineElement
        // This is against the Adapter mechanism!

        if( featureQName.equals( Kalypso1D2DSchemaConstants.WB1D2D_F_POLY_ELEMENT ) && cls.isAssignableFrom( IPolyElement.class ) )
          return new PolyElement( feature );
        else if( featureQName.equals( Kalypso1D2DSchemaConstants.WB1D2D_F_ELEMENT1D ) && cls.isAssignableFrom( IElement1D.class ) )
          return new Element1D( feature );
        else
        {
          warnUnableToAdapt( feature, featureQName, IFE1D2DElement.class );
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
          warnUnableToAdapt( feature, featureQName, IContinuityLine1D.class );
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
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( Kalypso1D2DSchemaConstants.WB1D2D_F_COMPLEX_ELE_2D ) )
        {
          return new FE1D2DComplexElement<IFE1D2DElement>( feature, Kalypso1D2DSchemaConstants.WB1D2D_F_COMPLEX_ELE_2D, Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELE_2D,// elements
          IFE1D2DElement.class );
        }
        else if( featureQName.equals( Kalypso1D2DSchemaConstants.WB1D2D_F_RIVER_CHANNEL1D ) )
        {
          return new FE1D2DComplexElement<Element1D>( feature, Kalypso1D2DSchemaConstants.WB1D2D_F_RIVER_CHANNEL1D, Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENT1D,// elements
          Element1D.class );
        }
        else if( featureQName.equals( ITransitionElement.QNAME ) )
        {
          return new TransitionElement( feature );
        }
        else if( featureQName.equals( Kalypso1D2DSchemaConstants.WB1D2D_F_CALC_UNIT_1D ) )
        {
          return new CalculationUnit1D( feature );
        }
        else if( featureQName.equals( Kalypso1D2DSchemaConstants.WB1D2D_F_CALC_UNIT_2D ) )
        {
          return new CalculationUnit2D( feature );
        }
        else if( featureQName.equals( Kalypso1D2DSchemaConstants.WB1D2D_F_CALC_UNIT_1D2D ) )
        {
          return new CalculationUnit1D2D( feature );
        }
        else
        {
          warnUnableToAdapt( feature, featureQName, IFE1D2DComplexElement.class );
          return null;
        }
      }
    };
    cMap.put( IFE1D2DComplexElement.class, cTor );
    cMap.put( ITransitionElement.class, cTor );
    cMap.put( ICalculationUnit.class, cTor );
    cMap.put( ICalculationUnit1D.class, cTor );
    cMap.put( ICalculationUnit2D.class, cTor );
    cMap.put( ICalculationUnit1D2D.class, cTor );

    // RiverChannel1D
    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        return new RiverChannel1D( feature );
      }
    };
    cMap.put( IRiverChannel1D.class, cTor );

    // DiscretisationModel
    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_F_MODEL_GROUP ) )
        {
          return new ControlModelGroup( feature );
        }
        else
        {
          warnUnableToAdapt( feature, featureQName, IFEDiscretisationModel1d2d.class );
          return null;
        }
      }
    };
    cMap.put( IControlModelGroup.class, cTor );

    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_F_MODEL_COLLECTION ) )
        {
          return new ControlModel1D2DCollection( feature );
        }
        else
        {
          warnUnableToAdapt( feature, featureQName, IFEDiscretisationModel1d2d.class );
          return null;
        }
      }
    };
    cMap.put( IControlModel1D2DCollection.class, cTor );

    // DiscretisationModel
    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( Kalypso1D2DSchemaConstants.WB1D2D_F_DiscretisationModel ) )
        {
          return new FE1D2DDiscretisationModel( feature );
        }
        else
        {
          warnUnableToAdapt( feature, featureQName, IFEDiscretisationModel1d2d.class );
          return null;
        }
      }
    };
    cMap.put( IFEDiscretisationModel1d2d.class, cTor );

    // StaticModel1D2D
    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( Kalypso1D2DSchemaConstants.WB1D2D_F_STATIC_MODEL ) )
        {
          return new StaticModel1D2D( feature );
        }
        else
        {
          warnUnableToAdapt( feature, featureQName, IStaticModel1D2D.class );
          return null;
        }
      }
    };
    cMap.put( IStaticModel1D2D.class, cTor );

    // Flow relationship model
    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( Kalypso1D2DSchemaConstants.OP1D2D_F_FLOWRELATIONSHIPS_MODEL ) )
        {
          return new FlowRelationshipModel( feature );
        }
        else
        {
          warnUnableToAdapt( feature, featureQName, IStaticModel1D2D.class );
          return null;
        }
      }
    };
    cMap.put( IFlowRelationshipModel.class, cTor );

    // ControlModel
    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();
        if( featureQName.equals( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_F_MODEL ) )
          return new ControlModel1D2D( feature );
        else
        {
          warnUnableToAdapt( feature, featureQName, IControlModel1D2D.class );
          return null;
        }
      }
    };
    cMap.put( IControlModel1D2D.class, cTor );

    // IDisplayElement
    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName name = feature.getFeatureType().getQName();
        // if(GMLSchemaUtilities.substitutes( feature.getFeatureType(),
        // KalypsoModelSimulationBaseConsts.SIM_BASE_F_NATIVE_TERRAIN_ELE_WRAPPER ))
        if( KalypsoModelSimulationBaseConsts.SIM_BASE_F_NATIVE_TERRAIN_ELE_WRAPPER.equals( name ) )
        {
          return ElevationModelDisplayElementFactory.createDisplayElement( feature );
        }
        else
        {
          warnUnableToAdapt( feature, name, DisplayElementDecorator.class );
          return null;
        }
      }
    };
    cMap.put( DisplayElementDecorator.class, cTor );

    // IFlowRelation
    cTor = new AdapterConstructor()
    {
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
          warnUnableToAdapt( feature, featureQName, IFlowRelationship.class );
          return null;
        }
      }
    };
    cMap.put( IFlowRelationship.class, cTor );

    // IBuildingFlowRelation
    cTor = new AdapterConstructor()
    {
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
          warnUnableToAdapt( feature, featureQName, IFlowRelationship.class );
          return null;
        }
      }
    };
    cMap.put( IBuildingFlowRelation.class, cTor );

    // KingFlowRelation
    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( IKingFlowRelation.QNAME ) )
        {
          return new KingFlowRelation( feature );
        }
        else
        {
          warnUnableToAdapt( feature, featureQName, IKingFlowRelation.class );
          return null;
        }
      }
    };
    cMap.put( IKingFlowRelation.class, cTor );

    // TeschkeFlowRelation
    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( ITeschkeFlowRelation.QNAME ) )
        {
          return new TeschkeFlowRelation( feature );
        }
        else
        {
          warnUnableToAdapt( feature, featureQName, ITeschkeFlowRelation.class );
          return null;
        }
      }
    };
    cMap.put( ITeschkeFlowRelation.class, cTor );

    // WeirFlowRelation
    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( IWeirFlowRelation.QNAME ) )
        {
          return new WeirFlowRelation( feature );
        }
        else
        {
          warnUnableToAdapt( feature, featureQName, ITeschkeFlowRelation.class );
          return null;
        }
      }
    };
    cMap.put( IWeirFlowRelation.class, cTor );

    // BridgeFlowRelation
    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( IBridgeFlowRelation.QNAME ) )
        {
          return new BridgeFlowRelation( feature );
        }
        else
        {
          warnUnableToAdapt( feature, featureQName, ITeschkeFlowRelation.class );
          return null;
        }
      }
    };
    cMap.put( IBridgeFlowRelation.class, cTor );

    // IResultMeta
    cTor = new AdapterConstructor()
    {
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
          warnUnableToAdapt( feature, featureQName, IResultMeta.class );
          return null;
        }
      }
    };
    cMap.put( IResultMeta.class, cTor );

    // ScenarioResultMeta
    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( IScenarioResultMeta.QNAME ) )
        {
          return new ScenarioResultMeta( feature );
        }
        else
        {
          warnUnableToAdapt( feature, featureQName, IScenarioResultMeta.class );
          return null;
        }
      }
    };
    cMap.put( IScenarioResultMeta.class, cTor );

    // CalcUnitResultMeta
    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( ICalcUnitResultMeta.QNAME ) )
        {
          return new CalcUnitResultMeta( feature );
        }
        else
        {
          warnUnableToAdapt( feature, featureQName, ICalcUnitResultMeta.class );
          return null;
        }
      }
    };
    cMap.put( ICalcUnitResultMeta.class, cTor );

    // StepResultMeta
    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( IStepResultMeta.QNAME ) )
        {
          return new StepResultMeta( feature );
        }
        else
        {
          warnUnableToAdapt( feature, featureQName, IStepResultMeta.class );
          return null;
        }
      }
    };
    cMap.put( IStepResultMeta.class, cTor );

    // DocumentResultMeta
    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( IDocumentResultMeta.QNAME ) )
        {
          return new DocumentResultMeta( feature );
        }
        else
        {
          warnUnableToAdapt( feature, featureQName, IDocumentResultMeta.class );
          return null;
        }
      }
    };
    cMap.put( IDocumentResultMeta.class, cTor );

    // RestartInfo
    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( IRestartInfo.QNAME ) )
        {
          return new RestartInfo( feature );
        }
        else
        {
          warnUnableToAdapt( feature, featureQName, IRestartInfo.class );
          return null;
        }
      }
    };
    cMap.put( IRestartInfo.class, cTor );

    // BoundaryCondition
    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( IBoundaryCondition.QNAME ) )
        {
          return new BoundaryCondition( feature );
        }
        else
        {
          warnUnableToAdapt( feature, featureQName, IBoundaryCondition.class );
          return null;
        }
      }
    };
    cMap.put( IBoundaryCondition.class, cTor );

    // hydrograph collection
    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( IHydrographCollection.QNAME ) )
        {
          return new HydrographCollection( feature );
        }
        else
        {
          warnUnableToAdapt( feature, featureQName, IBoundaryCondition.class );
          return null;
        }
      }
    };
    cMap.put( IHydrographCollection.class, cTor );

    // hydrograph
    cTor = new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException
      {
        final QName featureQName = feature.getFeatureType().getQName();

        if( featureQName.equals( IHydrograph.QNAME ) )
        {
          return new Hydrograph( feature );
        }
        else
        {
          warnUnableToAdapt( feature, featureQName, IBoundaryCondition.class );
          return null;
        }
      }
    };
    cMap.put( IHydrographCollection.class, cTor );

    return Collections.unmodifiableMap( cMap );
  }

}
