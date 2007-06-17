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
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.kalypsomodel1d2d.conv.BoundaryConditionInfo;
import org.kalypso.kalypsomodel1d2d.conv.BoundaryLineInfo;
import org.kalypso.kalypsomodel1d2d.conv.ITimeStepinfo;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.DiscretisationModelUtils;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IBoundaryLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
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
public class RMA10Calculation
{
  private GMLWorkspace m_disModelWorkspace = null;

  private GMLWorkspace m_terrainModelWorkspace = null;

  private GMLWorkspace m_operationalModelWorkspace;

  private GMLWorkspace m_flowRelWorkspace = null;

  private final GMLWorkspace m_flowResistanceWorkspace = null;

  private Feature m_controlModelRoot = null;

  private Feature m_roughnessRootWorkspace = null;

  private String m_kalypso1D2DKernelPath;

  private List<ITimeStepinfo> m_timeStepInfos;

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

    m_timeStepInfos = calculationBoundaryConditionInfos();
  }

  public RMA10Calculation( final GMLWorkspace disModelWorkspace, final Feature controlRoot, final Feature roughnessRoot )
  {
    m_disModelWorkspace = disModelWorkspace;
    m_controlModelRoot = controlRoot;
    m_roughnessRootWorkspace = roughnessRoot;
  }

  public GMLWorkspace getDisModelWorkspace( )
  {
    return m_disModelWorkspace;
  }

  public GMLWorkspace getTerrainModelWorkspace( )
  {
    return m_terrainModelWorkspace;
  }

  public GMLWorkspace getOperationalModelWorkspace( )
  {
    return m_operationalModelWorkspace;
  }

  public GMLWorkspace getFlowRelWorkspace( )
  {
    return m_flowRelWorkspace;
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

  public Double getViskosity( final Feature roughnessFE )
  {
    final int iedsw = getControlModel().getIEDSW();
    if( iedsw == 0 )
    {
      return getEddy( roughnessFE );
    }
    return getcharactV( roughnessFE );
  }

  public Double getEddy( final Feature roughnessFE )
  {
    return (Double) roughnessFE.getProperty( KalypsoModelRoughnessConsts.WBR_PROP_EDDY );
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
    // return (List) m_roughnessRoot.getProperty( KalypsoModelRoughnessConsts.WBR_F_ROUGHNESS );
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
    // implemented like this, or search for BoundaryConditions (operational model) which fits to ContinuityLines
    // (discretisation model)
    final IFEDiscretisationModel1d2d adapter = (IFEDiscretisationModel1d2d) m_disModelWorkspace.getRootFeature().getAdapter( IFEDiscretisationModel1d2d.class );
    final IFeatureWrapperCollection<IFE1D2DElement> elements = adapter.getElements();
    final List<IBoundaryLine> list = new ArrayList<IBoundaryLine>();
    final Iterator<IFE1D2DElement> iterator = elements.iterator();
    while( iterator.hasNext() )
    {
      final IFE1D2DElement element = iterator.next();
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
        continuityLineInfos.add( (BoundaryLineInfo) info );
    }

    return continuityLineInfos.toArray( new BoundaryLineInfo[continuityLineInfos.size()] );
  }

  public ITimeStepinfo[] getTimeStepInfos( )
  {
    return m_timeStepInfos.toArray( new ITimeStepinfo[m_timeStepInfos.size()] );
  }

  private List<ITimeStepinfo> calculationBoundaryConditionInfos( ) throws SimulationException
  {
    final List<ITimeStepinfo> result = new ArrayList<ITimeStepinfo>();

    /* Take all conti lines whihc are defined in the discretisation model. */
    final Map<IBoundaryLine, BoundaryLineInfo> contiMap = new HashMap<IBoundaryLine, BoundaryLineInfo>();
    final List<IBoundaryLine> continuityLineList = getContinuityLineList();
    int contiCount = 1;
    for( final IBoundaryLine<IFE1D2DComplexElement, IFE1D2DEdge> line : continuityLineList )
    {
      final List<IFE1D2DNode> nodes = line.getNodes();
      final IFE1D2DNode[] nodeArray = nodes.toArray( new IFE1D2DNode[nodes.size()] );
      final BoundaryLineInfo info = new BoundaryLineInfo( contiCount++, nodeArray );
      result.add( info );
      contiMap.put( line, info );
    }

    /* Add all boundary conditions. */
    final IFEDiscretisationModel1d2d discModel = (IFEDiscretisationModel1d2d) getDisModelWorkspace().getRootFeature().getAdapter( IFEDiscretisationModel1d2d.class );
    final IFlowRelationshipModel model = (IFlowRelationshipModel) m_operationalModelWorkspace.getRootFeature().getAdapter( IFlowRelationshipModel.class );
    for( final IFlowRelationship relationship : model )
    {
      if( relationship instanceof IBoundaryCondition )
      {
        final IBoundaryCondition bc = (IBoundaryCondition) relationship;
        final IObservation<TupleResult> obs = bc.getObservation();
        final TupleResult obsResult = obs.getResult();

        // HACK: 0.5 as grab distance?? normally 0.0 should be enough, but then the contilines are not found, why?
        final IFeatureWrapper2 wrapper2 = DiscretisationModelUtils.findModelElementForBC( discModel, bc.getPosition(), 1.0 );

        if( wrapper2 instanceof IBoundaryLine )
        {
          final IBoundaryLine<IFE1D2DComplexElement, IFE1D2DEdge> contiLine = (IBoundaryLine<IFE1D2DComplexElement, IFE1D2DEdge>) wrapper2;
          final BoundaryLineInfo info = contiMap.get( contiLine );

          final IComponent timeComponent = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
          final IComponent qComponent = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE );
          final IComponent hComponent = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL );
          if( qComponent != null )
            info.setObservation( obs, timeComponent, qComponent, ITimeStepinfo.TYPE.CONTI_BC_Q );
          else if( hComponent != null )
            info.setObservation( obs, timeComponent, hComponent, ITimeStepinfo.TYPE.CONTI_BC_H );
          else
            throw new SimulationException( "Falsche Parameter an Kontinuit‰tslinien-Randbedingung: " + bc.getName(), null );
        }
        else if( wrapper2 instanceof IFE1D2DNode && DiscretisationModelUtils.is1DNode( (IFE1D2DNode<IFE1D2DEdge>) wrapper2 ) )
        {
          // create new contiline
          final IFE1D2DNode[] nodeArray = new IFE1D2DNode[] { (IFE1D2DNode) wrapper2 };
          final BoundaryLineInfo info = new BoundaryLineInfo( contiCount++, nodeArray );

          final IComponent timeComponent = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
          final IComponent qComponent = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE );
          final IComponent hComponent = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL );
          if( qComponent != null )
            info.setObservation( obs, timeComponent, qComponent, ITimeStepinfo.TYPE.CONTI_BC_Q );
          else if( hComponent != null )
            info.setObservation( obs, timeComponent, hComponent, ITimeStepinfo.TYPE.CONTI_BC_H );
          else
            throw new SimulationException( "Falsche Parameter an Kontinuit‰tslinien-Randbedingung: " + bc.getName(), null );
        }
        else if( wrapper2 instanceof IElement2D )
        {
          final IElement2D<IFE1D2DComplexElement, IFE1D2DEdge> ele2d = (IElement2D<IFE1D2DComplexElement, IFE1D2DEdge>) wrapper2;

          final String gmlID = ele2d.getGmlID();
          final int id = 0; // TODO: get ascii element id for gmlid

          final BoundaryConditionInfo info = new BoundaryConditionInfo( id, ITimeStepinfo.TYPE.ELE_BCE_2D );

          final IComponent timeComponent = TupleResultUtilities.findComponentById( obsResult, "time" );
          final IComponent qComponent = TupleResultUtilities.findComponentById( obsResult, "q" );
          final IComponent hComponent = TupleResultUtilities.findComponentById( obsResult, "h" );
          final IComponent valueComponent = qComponent == null ? hComponent : qComponent;

          info.setObservation( obs, timeComponent, valueComponent );
        }
        else
          throw new SimulationException( "Nicht zugeorndete Randbedingung: " + bc.getName(), null );
        // TODO: consider 1delements
      }
    }

    return result;
  }

  public IControlModel1D2D getControlModel( )
  {
    final Feature controlModelCollection = (Feature) m_controlModelRoot.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_FP_MODEL_COLLECTION );

    final Object link = controlModelCollection.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_XP_ACTIVE_MODEL );
    final Feature controlModelFeature = FeatureHelper.getFeature( m_controlModelRoot.getWorkspace(), link );

    return (IControlModel1D2D) controlModelFeature.getAdapter( IControlModel1D2D.class );
  }
}
