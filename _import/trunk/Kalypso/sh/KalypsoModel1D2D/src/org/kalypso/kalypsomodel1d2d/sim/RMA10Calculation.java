/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.kalypsomodel1d2d.sim;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.BoundaryConditionInfo;
import org.kalypso.kalypsomodel1d2d.conv.BoundaryLineInfo;
import org.kalypso.kalypsomodel1d2d.conv.INativeIDProvider;
import org.kalypso.kalypsomodel1d2d.conv.ITimeStepinfo;
import org.kalypso.kalypsomodel1d2d.ops.CalcUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.DiscretisationModelUtils;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IBoundaryLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IBoundaryLine1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.schema.binding.metadata.IResultModelDescriptor;
import org.kalypso.kalypsomodel1d2d.schema.binding.metadata.ISimulationDescriptor;
import org.kalypso.kalypsomodel1d2d.schema.binding.metadata.ResultDB;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IResultModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelRoughnessConsts;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.observation.result.TupleResultUtilities;
import org.kalypso.ogc.gml.serialize.AbstractFeatureProviderFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.GmlSerializerXlinkFeatureProvider;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureProvider;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.IFeatureProviderFactory;

/**
 * @author huebsch <a href="mailto:j.huebsch@tuhh.de">Jessica Huebsch</a>
 */
@SuppressWarnings( { "unchecked", "hiding" })
public class RMA10Calculation implements INativeIDProvider
{
  private GMLWorkspace m_disModelWorkspace = null;

  private GMLWorkspace m_terrainModelWorkspace = null;

  private final GMLWorkspace m_operationalModelWorkspace;

  private GMLWorkspace m_flowRelWorkspace = null;

  private final GMLWorkspace m_flowResistanceWorkspace = null;

  private Feature m_controlModelRoot = null;

  private Feature m_roughnessRootWorkspace = null;

  private String m_kalypso1D2DKernelPath;

  private final List<ITimeStepinfo> m_timeStepInfos;

  private final LinkedHashMap<IBoundaryLine, Integer> m_boundaryLineIDProvider = new LinkedHashMap<IBoundaryLine, Integer>( 32 );

  private ISimulationDescriptor m_simulationDesciptor;

  private ICalculationUnit m_calculationUnit;

  public RMA10Calculation( final ISimulationDataProvider inputProvider ) throws SimulationException, Exception
  {
    final Map<String, String> specialUrls = new HashMap<String, String>();
    specialUrls.put( "project:/.metadata/roughness.gml", "Roughness" );

    final IUrlResolver resolver = new UrlResolver()
    {
      /**
       * @see org.kalypso.contribs.java.net.UrlResolver#resolveURL(java.net.URL, java.lang.String)
       */
      @Override
      public URL resolveURL( URL baseURL, String relativeURL ) throws MalformedURLException
      {
        if( specialUrls.containsKey( relativeURL ) )
        {
          final String specialUrl = specialUrls.get( relativeURL );
          try
          {
            return (URL) inputProvider.getInputForID( specialUrl );
          }
          catch( final SimulationException e )
          {
            e.printStackTrace();
            throw new MalformedURLException( "Missing input for ID: " + specialUrl );
          }
        }

        return super.resolveURL( baseURL, relativeURL );
      }
    };

    final IFeatureProviderFactory factory = new AbstractFeatureProviderFactory()
    {
      @Override
      protected IFeatureProvider createProvider( Feature context, String uri, String role, String arcrole, String title, String show, String actuate )
      {
        return new GmlSerializerXlinkFeatureProvider( context, uri, role, arcrole, title, show, actuate, this, resolver );
      }
    };

    // final GMLWorkspace simResWorkspace = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID(
    // RMA10SimModelConstants.SIMULATIONRESULTMODEL_ID ), null );
    m_disModelWorkspace = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( RMA10SimModelConstants.DISCRETISATIOMODEL_ID ), factory );
    m_terrainModelWorkspace = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( RMA10SimModelConstants.TERRAINMODEL_ID ), factory );
    m_operationalModelWorkspace = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( RMA10SimModelConstants.OPERATIONALMODEL_ID ), factory );
    m_flowRelWorkspace = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( RMA10SimModelConstants.FLOWRELATIONSHIPMODEL_ID ), factory );
    // m_flowResWS = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID(
    // RMA10SimModelConstants.FLOWRESISTANCEMODEL_ID ),
    // factory );

    m_controlModelRoot = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( RMA10SimModelConstants.CONTROL_ID ), factory ).getRootFeature();
    m_roughnessRootWorkspace = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( RMA10SimModelConstants.ROUGHNESS_ID ), factory ).getRootFeature();

    m_timeStepInfos = calculateBoundaryConditionInfos();
  }

  public GMLWorkspace getOperationalModelWorkspace( )
  {
    return m_operationalModelWorkspace;
  }

  public ITerrainModel getTerrainModel( )
  {
    return (ITerrainModel) m_terrainModelWorkspace.getRootFeature().getAdapter( ITerrainModel.class );
  }

  public IFlowRelationshipModel getFlowModel( )
  {
    return (IFlowRelationshipModel) m_flowRelWorkspace.getRootFeature().getAdapter( IFlowRelationshipModel.class );
  }

  public IFEDiscretisationModel1d2d getDiscModel( )
  {
    return (IFEDiscretisationModel1d2d) m_disModelWorkspace.getRootFeature().getAdapter( IFEDiscretisationModel1d2d.class );
  }

  public GMLWorkspace getFlowResistanceWorkspace( )
  {
    return m_flowResistanceWorkspace;
  }

  public String getKalypso1D2DKernelPath( )
  {
    final IControlModel1D2D controlModel = getControlModel();
    final String kalypso1D2DVersion = controlModel.getVersion();

    if( kalypso1D2DVersion.equals( "test" ) )
      m_kalypso1D2DKernelPath = RMA10SimModelConstants.SIM_EXE_FILE_TEST;
    else if( kalypso1D2DVersion.equals( "NEW" ) )
      m_kalypso1D2DKernelPath = RMA10SimModelConstants.SIM_EXE_FILE_3_5;
    else if( kalypso1D2DVersion.equals( "Version3.5" ) )
      m_kalypso1D2DKernelPath = RMA10SimModelConstants.SIM_EXE_FILE_3_5;
    else
      m_kalypso1D2DKernelPath = RMA10SimModelConstants.SIM_EXE_FILE_3_5;

    return m_kalypso1D2DKernelPath;
  }

  public Double[] getViskosity( final Feature roughnessFE )
  {
    final Double[] result = new Double[4];
    final int iedsw = getControlModel().getIEDSW();

    // turbulence combo (iedsw):
    if( iedsw == 0 || iedsw == 10 || iedsw == 13 )
    {
      final double defaultValue = 2900.0;
      result[0] = defaultValue;
      result[1] = defaultValue;
      result[2] = defaultValue;
      result[3] = defaultValue;
      // result[0] = getEddyXX( roughnessFE );
      // result[1] = getEddyYX( roughnessFE );
      // result[2] = getEddyXY( roughnessFE );
      // result[3] = getEddyYY( roughnessFE );
    }
    else
    {
      // TODO: strange values, check again with Nico
      // final double defaultValue = 0.4;
      final double defaultValue = 2900.0;
      result[0] = defaultValue;
      result[1] = defaultValue;
      result[2] = defaultValue;
      result[3] = defaultValue;
    }
    return result;
    // return getcharactV( roughnessFE ); 0.4
  }

  public Double getEddyXX( final Feature roughnessFE )
  {
    return FeatureHelper.getAsDouble( roughnessFE, KalypsoModelRoughnessConsts.WBR_PROP_EDDY_XX, 500.0 );
  }

  public Double getEddyYX( final Feature roughnessFE )
  {
    return FeatureHelper.getAsDouble( roughnessFE, KalypsoModelRoughnessConsts.WBR_PROP_EDDY_YX, 500.0 );
  }

  public Double getEddyXY( final Feature roughnessFE )
  {
    return FeatureHelper.getAsDouble( roughnessFE, KalypsoModelRoughnessConsts.WBR_PROP_EDDY_XY, 500.0 );
  }

  public Double getEddyYY( final Feature roughnessFE )
  {
    return FeatureHelper.getAsDouble( roughnessFE, KalypsoModelRoughnessConsts.WBR_PROP_EDDY_YY, 500.0 );
  }

  public Double getcharactV( final Feature roughnessFE )
  {
    return (Double) roughnessFE.getProperty( KalypsoModelRoughnessConsts.WBR_PROP_CHARACTV );
  }

  public String getName( final Feature feature )
  {
    return (String) feature.getProperty( KalypsoModelRoughnessConsts.GML_PROP_NAME );
  }

  public List getRoughnessClassList( )
  {
    return (List) m_roughnessRootWorkspace.getProperty( KalypsoModelRoughnessConsts.WBR_PROP_ROUGHNESS_CLS_MEMBER );
  }

  public Double getKs( final Feature roughnessFE )
  {
    return (Double) roughnessFE.getProperty( KalypsoModelRoughnessConsts.WBR_PROP_KS );
  }

  public Double getAxAy( final Feature roughnessFE )
  {
    return (Double) roughnessFE.getProperty( KalypsoModelRoughnessConsts.WBR_PROP_AXAY );
  }

  public Double getDp( final Feature roughnessFE )
  {
    return (Double) roughnessFE.getProperty( KalypsoModelRoughnessConsts.WBR_PROP_DP );
  }

  public List<IBoundaryLine> getContinuityLineList( )
  {
    // TODO: at the moment, we write ALL boundary lines again...

    // implemented like this, or search for BoundaryConditions (operational model) which fits to ContinuityLines
    // (discretisation model)
    final IFEDiscretisationModel1d2d discModel = getDiscModel();
    final List<IBoundaryLine> list = new ArrayList<IBoundaryLine>();
    final IFeatureWrapperCollection<IFE1D2DElement> elements = discModel.getElements();
    for( final IFE1D2DElement element : elements )
    {
      if( element instanceof IBoundaryLine )
        list.add( (IBoundaryLine) element );
    }
    return list;

  }

  public BoundaryLineInfo[] getContinuityLineInfo( )
  {
    final List<BoundaryLineInfo> continuityLineInfos = new ArrayList<BoundaryLineInfo>();

    for( final ITimeStepinfo info : m_timeStepInfos )
    {
      if( info instanceof BoundaryLineInfo )
        // TODO: This is a discrace, what else should be there??
        // Also: for every BC we need a conti line!
        continuityLineInfos.add( (BoundaryLineInfo) info );
    }

    return continuityLineInfos.toArray( new BoundaryLineInfo[continuityLineInfos.size()] );
  }

  public ITimeStepinfo[] getTimeStepInfos( )
  {
    return m_timeStepInfos.toArray( new ITimeStepinfo[m_timeStepInfos.size()] );
  }

  private List<ITimeStepinfo> calculateBoundaryConditionInfos( ) throws SimulationException
  {
    // TOFILTER
    final List<ITimeStepinfo> result = new ArrayList<ITimeStepinfo>();

    /* Take all conti lines which are defined in the discretisation model. */
    final Map<IBoundaryLine, BoundaryLineInfo> contiMap = new HashMap<IBoundaryLine, BoundaryLineInfo>();
    final List<IBoundaryLine> continuityLineList = getContinuityLineList();
    int contiCount = 0;
    for( final IBoundaryLine<IFE1D2DComplexElement, IFE1D2DEdge> line : continuityLineList )
    {
      final IFE1D2DNode[] nodeArray;
      if( line instanceof IBoundaryLine1D )
      {
        nodeArray = new IFE1D2DNode[] { ((IBoundaryLine1D) line).getTargetNode() };
      }
      else
      {
        final List<IFE1D2DNode> nodes = line.getNodes();
        nodeArray = nodes.toArray( new IFE1D2DNode[nodes.size()] );
      }
      final BoundaryLineInfo info = new BoundaryLineInfo( ++contiCount, nodeArray );
      result.add( info );
      contiMap.put( line, info );
      m_boundaryLineIDProvider.put( line, new Integer( contiCount ) );
    }

    // TODO: instead of this stupit code above, we should iterate through all boundary conditions below as before
    // and check for each of them if it is contained in a calc-unit

    /* Add all boundary conditions. */
    final IFlowRelationshipModel model = (IFlowRelationshipModel) m_operationalModelWorkspace.getRootFeature().getAdapter( IFlowRelationshipModel.class );
    final ICalculationUnit unit = getCalculationUnit();
    // TODO: this is very bad! a grab distance of 11 is much too big!!!
    final double grabDistance = 11;
    for( final IFlowRelationship relationship : model )
    {
      if( relationship instanceof IBoundaryCondition )
      {
        final IBoundaryCondition bc = (IBoundaryCondition) relationship;
        final IObservation<TupleResult> obs = bc.getObservation();
        final TupleResult obsResult = obs.getResult();
        // CalUnitOps.getAssignedBoundaryConditionLine( unit, bCondition, grabDistance );
        // HACK: 0.5 as grab distance?? normally 0.0 should be enough, but then the contilines are not found, why?
        // TODO: at least find everything in this distance, if mroe than one element is found, take nearest...
        // final boolean boundaryConditionOf = CalUnitOps.isBoundaryConditionOf( unit, bc, grabDistance );
        final IFeatureWrapper2 wrapper2 = CalcUnitOps.getAssignedBoundaryConditionLine( unit, bc, grabDistance );// DiscretisationModelUtils.findModelElementForBC(
        // discModel,
        // bc.getPosition(),
        // 0.001
        // );

        if( wrapper2 == null )
        {
          System.out.println( "Skiping boundary condition since it is not part of calcUnit:" + relationship.getGmlID() );
        }
        else if( wrapper2 instanceof IBoundaryLine )
        {
          final IBoundaryLine<IFE1D2DComplexElement, IFE1D2DEdge> contiLine = (IBoundaryLine<IFE1D2DComplexElement, IFE1D2DEdge>) wrapper2;
          final BoundaryLineInfo info = contiMap.get( contiLine );

          if( info == null )
            continue;

          final IComponent timeComponent = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
          final IComponent qComponent = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE );
          final IComponent hComponent = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL );
          info.setSteadyValue( bc.getStationaryCondition() );
          if( qComponent != null )
          {
            info.setObservation( obs, timeComponent, qComponent, ITimeStepinfo.TYPE.CONTI_BC_Q );
            info.setTheta( bc.getDirection() );
          }
          else if( hComponent != null )
            info.setObservation( obs, timeComponent, hComponent, ITimeStepinfo.TYPE.CONTI_BC_H );
          else
            throw new SimulationException( "Falsche Parameter an Kontinuit‰tslinien-Randbedingung: " + bc.getName(), null );

          // TODO: at the moment, we should comment this line out;
          // Sadly, the calc unit concept does not apply at all to non-boundary.line connected boundary conditions.
          // result.add( info );
        }
        // TODO: the following elses never get called any more
        else if( wrapper2 instanceof IFE1D2DNode && DiscretisationModelUtils.is1DNode( (IFE1D2DNode<IFE1D2DEdge>) wrapper2 ) )
        {
          // create new contiline
          final IFE1D2DNode[] nodeArray = new IFE1D2DNode[] { (IFE1D2DNode) wrapper2 };
          final BoundaryLineInfo info = new BoundaryLineInfo( ++contiCount, nodeArray );

          final IComponent timeComponent = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
          final IComponent qComponent = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE );
          final IComponent hComponent = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL );
          info.setSteadyValue( bc.getStationaryCondition() );
          if( qComponent != null )
          {
            info.setObservation( obs, timeComponent, qComponent, ITimeStepinfo.TYPE.CONTI_BC_Q );
            info.setTheta( bc.getDirection() );
          }
          else if( hComponent != null )
            info.setObservation( obs, timeComponent, hComponent, ITimeStepinfo.TYPE.CONTI_BC_H );
          else
            throw new SimulationException( "Falsche Parameter an Kontinuit‰tslinien-Randbedingung: " + bc.getName(), null );

          result.add( info );
        }
        else if( wrapper2 instanceof IElement2D || wrapper2 instanceof IElement1D )
        {
          final IElement2D<IFE1D2DComplexElement, IFE1D2DEdge> ele2d = (IElement2D<IFE1D2DComplexElement, IFE1D2DEdge>) wrapper2;

          // final String gmlID = ele2d.getGmlID();
          final int id = 0; // TODO: get ascii element id for gmlid

          final BoundaryConditionInfo info = new BoundaryConditionInfo( id, ITimeStepinfo.TYPE.ELE_BCE_2D );

          final IComponent timeComponent = TupleResultUtilities.findComponentById( obsResult, "time" );
          final IComponent qComponent = TupleResultUtilities.findComponentById( obsResult, "q" );
          final IComponent hComponent = TupleResultUtilities.findComponentById( obsResult, "h" );
          final IComponent valueComponent = qComponent == null ? hComponent : qComponent;

          info.setObservation( obs, timeComponent, valueComponent );
          info.setSteadyValue( bc.getStationaryCondition() );

          result.add( info );
        }
        else
          throw new SimulationException( "Nicht zugeordnete Randbedingung: " + bc.getName(), null );
      }
    }

    return result;
  }

  public ICalculationUnit getCalculationUnit( )
  {
    if( m_calculationUnit == null )
    {
      final IControlModel1D2D controlModel = getControlModel();

      m_calculationUnit = controlModel.getCalculationUnit();
      /*
       * final ICalculationUnit linkedCalculationUnit = controlModel.getCalculationUnit(); final
       * IFEDiscretisationModel1d2d discModel = getDiscModel(); final Feature feature =
       * discModel.getWrappedFeature().getWorkspace().getFeature( linkedCalculationUnit.getGmlID() ); m_calculationUnit =
       * (ICalculationUnit) feature.getAdapter( ICalculationUnit.class );
       */
    }
    return m_calculationUnit;
  }

  public IControlModel1D2D getControlModel( )
  {
    final Feature controlModelCollection = (Feature) m_controlModelRoot.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_FP_MODEL_COLLECTION );

    final Object link = controlModelCollection.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_XP_ACTIVE_MODEL );
    final Feature controlModelFeature = FeatureHelper.getFeature( m_controlModelRoot.getWorkspace(), link );

    return (IControlModel1D2D) controlModelFeature.getAdapter( IControlModel1D2D.class );
  }

  /**
   * Adds a simulation descriptor for this rma10 simulation to the result db
   */
  public void addToResultDB( )
  {
    /** Descriptor for this simulation */
    final ResultDB resultDB = KalypsoModel1D2DPlugin.getDefault().getResultDB();
    m_simulationDesciptor = resultDB.addRMACalculation( this );
  }

  /**
   * Adds a descriptor for this result model to the simulation descriptor of this rma10s simulation
   * 
   * @param resultModel
   *            the result model to add to the rma10 simulation
   * @return the added result descriptor
   */
  public IResultModelDescriptor addToSimulationDescriptor( final IResultModel1d2d resultModel )
  {
    Assert.throwIAEOnNullParam( resultModel, "resultModel" );
    final ResultDB resultDB = KalypsoModel1D2DPlugin.getDefault().getResultDB();

    final IResultModelDescriptor resDescriptor = (IResultModelDescriptor) resultDB.addModelDescriptor( resultModel );
    final IFeatureWrapperCollection<IResultModelDescriptor> resultModels = m_simulationDesciptor.getResultModel();
    resultModels.addRef( resDescriptor );

    return resDescriptor;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.INativeIDProvider#getID(org.kalypsodeegree.model.feature.binding.IFeatureWrapper2)
   */
  public int getBoundaryLineID( final IFeatureWrapper2 object )
  {
    if( object instanceof IBoundaryLine )
    {
      final Integer id = m_boundaryLineIDProvider.get( object );
      if( id == null )
        return -1;

      return id;
    }
    else
    {
      throw new RuntimeException( "Type not supported" );
    }
  }

  public boolean containsID( final IFeatureWrapper2 i1d2dObject )
  {
    if( i1d2dObject instanceof IBoundaryLine )
    {
      return m_boundaryLineIDProvider.containsKey( i1d2dObject );
    }
    else
    {
      throw new RuntimeException( "Type not supported" );
    }
  }
}
