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
package org.kalypso.kalypsomodel1d2d.conv.results;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.gml.processes.constDelaunay.ConstraintDelaunayHelper;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DDebug;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.EReadError;
import org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler;
import org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv.RESULTLINES;
import org.kalypso.kalypsomodel1d2d.conv.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.conv.results.lengthsection.LengthSectionHandler1d;
import org.kalypso.kalypsomodel1d2d.ops.CalcUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IFlowRelation1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IFlowRelation2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.ITeschkeFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.GMLNodeResult;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResultCollection;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.SimpleNodeResult;
import org.kalypso.kalypsomodel1d2d.sim.ISimulation1D2DConstants;
import org.kalypso.kalypsomodel1d2d.sim.NodeResultMinMaxCatcher;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.schema.IWspmDictionaryConstants;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * @author Thomas Jung
 */
public class NodeResultsHandler implements IRMA10SModelElementHandler
{
  private static final double NODE_SEARCH_DIST = 0.2;

  private static final int WSP_EXTRAPOLATION_RANGE = 1;

  private final Map<Integer, INodeResult> m_nodeIndex = new HashMap<Integer, INodeResult>();

  private final HashMap<Integer, ArcResult> m_arcIndex = new HashMap<Integer, ArcResult>();

  private final HashMap<Integer, ElementResult> m_elemIndex = new HashMap<Integer, ElementResult>();

  private final Set<String> m_lengthsection1dNodes = new HashSet<String>();

  private final GMLWorkspace m_resultWorkspace;

  private final FeatureList m_resultList;

  private final String m_crs;

  private final ITriangleEater m_triangleEater;

  private final NodeResultMinMaxCatcher m_resultMinMaxCatcher;

  private Date m_time;

  private final IFlowRelationshipModel m_flowModel;

  private final IControlModel1D2D m_controlModel;

  private final LengthSectionHandler1d m_lsHandler;

  private final IFEDiscretisationModel1d2d m_discModel;

  final private Map<String, Map<GM_Position, Double>> m_mapSWANResults;

  private Map<GM_Position, Double> m_mapWAVEHsig = null;

  private Map<GM_Position, Double> m_mapWAVEDir = null;

  private Map<GM_Position, Double> m_mapWAVEPeriod = null;

  public NodeResultsHandler( final GMLWorkspace resultWorkspace, final ITriangleEater triangleEater, final IFlowRelationshipModel flowModel, final IControlModel1D2D controlModel, final IFEDiscretisationModel1d2d discModel, final NodeResultMinMaxCatcher resultMinMaxCatcher, final LengthSectionHandler1d lsHandler )
  {
    this( resultWorkspace, triangleEater, flowModel, controlModel, discModel, resultMinMaxCatcher, lsHandler, null );
  }

  public NodeResultsHandler( final GMLWorkspace resultWorkspace, final ITriangleEater triangleEater, final IFlowRelationshipModel flowModel, final IControlModel1D2D controlModel, final IFEDiscretisationModel1d2d discModel, final NodeResultMinMaxCatcher resultMinMaxCatcher, final LengthSectionHandler1d lsHandler, final Map<String, Map<GM_Position, Double>> mapSWANResults )
  {
    m_resultWorkspace = resultWorkspace;
    m_triangleEater = triangleEater;
    m_flowModel = flowModel;
    m_controlModel = controlModel;
    m_discModel = discModel;
    m_resultMinMaxCatcher = resultMinMaxCatcher;
    m_lsHandler = lsHandler;
    m_resultList = (FeatureList) m_resultWorkspace.getRootFeature().getProperty( INodeResultCollection.QNAME_PROP_NODERESULT_MEMBER );

    m_crs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    m_mapSWANResults = mapSWANResults;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#end()
   */
  @Override
  public void end( )
  {
    /* extrapolate the water level into dry areas */
    extrapolateWaterLevel( 0 );
    // create the triangles for each element
    for( final ElementResult element : m_elemIndex.values() )
    {
      element.createCenterNode(); // split the element
      splitElement( element );

      // set Centernode null again in order to free some memory
      // remove element from list

    }
  }

  /**
   * extrapolates the water level into the dry elements until every dry element got assigned or until the maximum
   * extrapolation range has been reached.
   */
  private void extrapolateWaterLevel( int count )
  {
    KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.NodeResultsHandler.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$

    if( count > WSP_EXTRAPOLATION_RANGE )
      return;
    int assigned = 0;
    for( final ElementResult element : m_elemIndex.values() )
    {
      if( element.checkWaterlevels() )
        assigned = assigned + 1;
    }
    if( assigned > 0 )
    {
      count = count + 1;

      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.NodeResultsHandler.3" ) + assigned + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.NodeResultsHandler.6" ) + count + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

      extrapolateWaterLevel( count );
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#getCreatedFeatures()
   */
  public List<IFeatureWrapper2> getCreatedFeatures( )
  {
    return null;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleArc(java.lang.String, int, int, int, int,
   *      int, int)
   */
  @Override
  public void handleArc( final String lineString, final int id, final int node1ID, final int node2ID, final int elementLeftID, final int elementRightID, final int middleNodeID )
  {
    // to get the model-geometry, we save all arcs in a Hash-Map
    final ArcResult arcResult = new ArcResult( id, node1ID, node2ID, elementLeftID, elementRightID, middleNodeID );
    m_arcIndex.put( id, arcResult );

    /* store the information of the connection between arcs and elements at the element object */
    /* left element */
    writeArcInfoAtElement( elementLeftID, arcResult );
    /* right element */
    writeArcInfoAtElement( elementRightID, arcResult );

    if( middleNodeID == 0 )
      return; // maybe this is a good place to mark the 1d-nodes....

    // get the information which nodes are mid-side nodes
    final INodeResult result = m_nodeIndex.get( middleNodeID );

    // check for illegal arcs
    if( result == null )
    {
      m_arcIndex.remove( id );
      return;
    }
    if( middleNodeID != -1 )
    {
      result.setMidSide( true );
    }
    else
      result.setMidSide( false );
  }

  private void writeArcInfoAtElement( final int elementID, final ArcResult arcResult )
  {
    /* does the element already exist? */
    if( m_elemIndex.containsKey( elementID ) )
    {
      // get the element
      final ElementResult elementResult = m_elemIndex.get( elementID );
      // store the arc
      elementResult.setArc( arcResult );
    }
    else
    {
      // create the element
      final ElementResult elementResult = new ElementResult( elementID, 0, 0, 0 );
      // store the arc
      elementResult.setArc( arcResult );
      // store the element
      m_elemIndex.put( elementID, elementResult );
    }
  }

  @Override
  public void handleFlowResitance( final String lineString, final int id, final double combinedLambda, final double soilLambda, final double vegetationLambda )
  {
    /*
     * IT IS NECESSARY, THAT THE FLOW RESISTANCE VALUES ARE STANDING BELOW THE ELEMENTS IN THE 2D-RESULT FILE!!
     */
    if( m_elemIndex.containsKey( id ) )
    {
      // get the element
      final ElementResult elementResult = m_elemIndex.get( id );

      // for calculating the shear stress we just use the lambda of the soil.
      elementResult.setLambda( soilLambda );
    }
    else
    {
      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.NodeResultsHandler.9" ) ); //$NON-NLS-1$ //$NON-NLS-2$
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleElement(java.lang.String, int, int, int,
   *      int)
   */
  @Override
  public void handleElement( final String lineString, final int id, final int currentRougthnessClassID, final int previousRoughnessClassID, final int eleminationNumber )
  {
    // For each element calculate the geometry (elemID, cornernode1, midsidenode1, cornernode2, midsidenode2,

    // cornernode3, midsidenode3, (cornernode4, midsidenode4), element type)
    /* =========== calculate the geometry ============= */

    /*
     * right here, all element objects should have been created by the arcHandler IT IS NECESSARY, THAT THE ELEMENTS ARE
     * STANDING BELOW THE ARCS IN THE 2D-RESULT FILE!!
     */
    if( m_elemIndex.containsKey( id ) )
    {
      // get the element
      final ElementResult elementResult = m_elemIndex.get( id );

      // for completion add the additional parameters
      elementResult.setCurrentRougthnessClassID( currentRougthnessClassID );
      elementResult.setPreviousRoughnessClassID( previousRoughnessClassID );
      elementResult.setEleminationNumber( eleminationNumber );

      /* get the number of the nodes of the arcs */
      final int numArcs = elementResult.getNumArcs();

      /* get the first arc */
      ArcResult currentArc = elementResult.getArc( 0 );
      INodeResult nodeDown = m_nodeIndex.get( currentArc.node1ID );
      INodeResult nodeUp = m_nodeIndex.get( currentArc.node2ID );

      // is it a 1d- or 2d-element
      if( currentRougthnessClassID == 89 || currentRougthnessClassID == -89 ) // 1d
      {
        /* 1d elements */

        final ICalculationUnit1D calcUnit = getCalcUnit( elementResult );

        try
        {
          final IStatus status = handle1dElement( nodeDown, nodeUp, calcUnit );
          if( status != Status.OK_STATUS )
            KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.NodeResultsHandler.11" ) + status.getMessage() ); //$NON-NLS-1$ //$NON-NLS-2$

        }
        catch( final Exception e )
        {
          KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.NodeResultsHandler.13" ) ); //$NON-NLS-1$ //$NON-NLS-2$
          e.printStackTrace();
          return;
        }
      }
      else if( currentRougthnessClassID >= 904 )
      {
        /* weirs & bridges */
        try
        {
          final ICalculationUnit1D calcUnit = getCalcUnit( elementResult );
          // TODO: right now, we handle them as normal 2d - element. Do we need more?
          handle1dElement( nodeDown, nodeUp, calcUnit );
          // handle1dStructure( nodeDown, nodeUp );
        }
        catch( final Exception e )
        {
          KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.NodeResultsHandler.15" ) ); //$NON-NLS-1$ //$NON-NLS-2$
          e.printStackTrace();
          return;
        }
      }
      else
      {
        /* 2d elements */
        if( currentArc.elementLeftID == id )
        {
          elementResult.setCornerNodes( nodeDown );
          elementResult.setCornerNodes( nodeUp );
        }

        /* element on the right side of the arc */
        else
        {
          elementResult.setCornerNodes( nodeUp );
          elementResult.setCornerNodes( nodeDown );
        }

        /* handle result information */

        /* midside node of the current arc */
        INodeResult midsideNode = m_nodeIndex.get( currentArc.middleNodeID );
        if( midsideNode == null )
          return;
        midsideNode.setArc( currentArc );

        // TODO: this check has to run again after the water level check of an element
        NodeResultHelper.checkMidsideNodeData( nodeDown, nodeUp, midsideNode );
        elementResult.setMidsideNodes( midsideNode );

        nodeDown.setArc( currentArc );
        currentArc.setNodeDown( nodeDown );

        nodeUp.setArc( currentArc );
        currentArc.setNodeUp( nodeUp );

        /**
         * For the other arcs, just save one corner node.<br>
         * This corner node has to be the opposite node of the connectionNode (last saved corner node)<br>
         * and must lie on the same arc (connected by the same arc).<br>
         * get the current arc of the element (arc)<br>
         * check the orientation of the arc (element is left or right-sided)<br>
         * check the connection. <br>
         */

        for( int pointIndex = 1; pointIndex < numArcs; pointIndex++ )
        {
          final GMLNodeResult connectionNode = (GMLNodeResult) elementResult.getCornerNodes( pointIndex );
          for( int arc = 1; arc < numArcs; arc++ )
          {

            currentArc = elementResult.getArc( arc );

            final int elementLeftID = currentArc.elementLeftID;
            final int elementRightID = currentArc.elementRightID;
            nodeDown = m_nodeIndex.get( currentArc.node1ID );
            nodeUp = m_nodeIndex.get( currentArc.node2ID );

            /* element on the left side of the arc */
            midsideNode = m_nodeIndex.get( currentArc.middleNodeID );
            midsideNode.setArc( currentArc );

            if( elementLeftID == id && nodeDown.equals( connectionNode ) )
            {
              if( elementResult.getNumCornerNodes() < numArcs )
              {
                elementResult.setCornerNodes( nodeUp );
              }

              nodeUp.setArc( currentArc );
              currentArc.setNodeUp( nodeUp );

              connectionNode.setArc( currentArc );
              currentArc.setNodeDown( connectionNode );

              NodeResultHelper.checkMidsideNodeData( nodeDown, nodeUp, midsideNode );
              elementResult.setMidsideNodes( midsideNode );
              break;
            }

            /* element on the right side of the arc */
            else if( elementRightID == id && nodeUp.equals( connectionNode ) )
            {
              if( elementResult.getNumCornerNodes() < numArcs )
              {
                elementResult.setCornerNodes( nodeDown );
              }
              nodeDown.setArc( currentArc );
              currentArc.setNodeDown( nodeDown );

              connectionNode.setArc( currentArc );
              currentArc.setNodeUp( connectionNode );

              NodeResultHelper.checkMidsideNodeData( nodeDown, nodeUp, midsideNode );
              elementResult.setMidsideNodes( midsideNode );
              break;
            }
          }
        }

        /* check the element (number of arcs, nodes, mid-side nodes) */

        /* check water levels for dry nodes */
        elementResult.checkWaterlevels();
      }
    }
    else
    {
      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.NodeResultsHandler.17" ) ); //$NON-NLS-1$ //$NON-NLS-2$
    }
  }

  private IFlowRelationship getFlowRelation( final INodeResult nodeResult )
  {
    final GM_Position nodePos = nodeResult.getPoint().getPosition();

    final IFlowRelationship[] flowRelationships = m_flowModel.findFlowrelationships( nodePos, NODE_SEARCH_DIST );

    // for some reason there are flow relations that are from type BoundaryCondition
    // go through the found array and get the first found teschke flow relation
    for( final IFlowRelationship element : flowRelationships )
    {
      if( element instanceof IFlowRelation1D )
      {
        return element;
      }
      else if( element instanceof IFlowRelation2D )
      {
        return element;
      }
    }
    return null;
  }

  /**
   * collects the necessary data for the length section and stores it in an observation.
   */
  private IStatus handleLengthSectionData( final INodeResult nodeResult, final IFlowRelation1D flowRelation1d, final ICalculationUnit1D calcUnit )
  {
    final IProfileFeature profile = flowRelation1d.getProfile();
    final double profileStation = profile.getStation();
    // station
    final BigDecimal station = ProfilUtil.stationToBigDecimal( profileStation );

    // thalweg
    final BigDecimal thalweg = new BigDecimal( nodeResult.getPoint().getZ() ).setScale( 4, BigDecimal.ROUND_HALF_UP );

    /* calculated data */
    // wsp
    final BigDecimal waterlevel = new BigDecimal( nodeResult.getWaterlevel() ).setScale( 4, BigDecimal.ROUND_HALF_UP );

    // velocity
    final BigDecimal velocity = new BigDecimal( nodeResult.getAbsoluteVelocity() ).setScale( 4, BigDecimal.ROUND_HALF_UP );

    // depth
    final BigDecimal depth = new BigDecimal( nodeResult.getDepth() ).setScale( 4, BigDecimal.ROUND_HALF_UP );

    /* discharge */
    // Q = v x A
    // A = A(y) //get it from the polynomials
    final BigDecimal area = NodeResultHelper.getCrossSectionArea( (ITeschkeFlowRelation) flowRelation1d, depth );

    if( area == null )
      return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.NodeResultsHandler.18", nodeResult.getGmlID(), station.doubleValue() ) ); //$NON-NLS-1$

    final BigDecimal discharge = velocity.multiply( area ).setScale( 3, BigDecimal.ROUND_HALF_UP );
    nodeResult.setDischarge( discharge.doubleValue() );

    // markers for Trennfl‰chen / Bordvollpunkte u.‰.
    try
    {
      if( calcUnit != null )
      {
        // maybe produce warning if node without calcunit is found?
        m_lsHandler.addValue( calcUnit, station, thalweg, IWspmDictionaryConstants.LS_COMPONENT_GROUND );
        m_lsHandler.addValue( calcUnit, station, waterlevel, IWspmDictionaryConstants.LS_COMPONENT_WATERLEVEL );
        m_lsHandler.addValue( calcUnit, station, velocity, IWspmDictionaryConstants.LS_COMPONENT_VELOCITY );
        m_lsHandler.addValue( calcUnit, station, discharge, IWspmDictionaryConstants.LS_COMPONENT_RUNOFF );
        // m_lsHandler.addValues( calcUnitName, station, depth, IWspmDictionaryConstants.LS_COMPONENT_DEPTH );
        // m_lsHandler.addValues( calcUnitName, station, slope, IWspmDictionaryConstants.LS_COMPONENT_SLOPE );
      }
    }
    catch( final Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

    return Status.OK_STATUS;
  }

  private IStatus handle1dElement( final INodeResult nodeDown, final INodeResult nodeUp, final ICalculationUnit1D calcUnit ) throws GM_Exception
  {
    final INodeResult[] nodes = new INodeResult[2];
    nodes[0] = nodeDown;
    nodes[1] = nodeUp;
    final GM_Curve[] curves = new GM_Curve[2];

    for( int i = 0; i < nodes.length; i++ )
    {
      final IFlowRelationship lFlowRel = getFlowRelation( nodes[i] );
      if( !(lFlowRel instanceof IFlowRelation1D) )
      {
        break;
      }
      final IFlowRelation1D flowRelation1d = (IFlowRelation1D) lFlowRel;

      if( flowRelation1d == null )
        break;

      /* check, if node was already handled for lengthsection */
      if( !m_lengthsection1dNodes.contains( nodes[i].getGmlID() ) )
      {
        handleLengthSectionData( nodes[i], flowRelation1d, calcUnit );
        // TODO: right now, no consequences of the returned status
        m_lengthsection1dNodes.add( nodes[i].getGmlID() );
      }

      final IProfileFeature profile = flowRelation1d.getProfile();

      // get the profile Curves of the two nodes defining the current element
      curves[i] = NodeResultHelper.getProfileCurveFor1dNode( profile );
    }

    /* Probably profile information missing */
    if( curves[0] == null || curves[1] == null )
      return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.NodeResultsHandler.20" ) ); //$NON-NLS-1$

    final double curveDistance = curves[0].distance( curves[1] );
    create1dTriangles( nodes, curves, curveDistance );

    return Status.OK_STATUS;
  }

  private ICalculationUnit1D getCalcUnit( final ElementResult elementResult )
  {
    if( m_discModel == null )
      return null;

    /* get the first arc */
    final ArcResult currentArc = elementResult.getArc( 0 );
    final INodeResult downNodeResult = m_nodeIndex.get( currentArc.node1ID );
    final INodeResult upNodeResult = m_nodeIndex.get( currentArc.node2ID );

    final IFE1D2DNode upNode = m_discModel.findNode( upNodeResult.getPoint(), NODE_SEARCH_DIST );
    final IFE1D2DNode downNode = m_discModel.findNode( downNodeResult.getPoint(), NODE_SEARCH_DIST );
    if( upNode == null || downNode == null )
      return null;

    IFE1D2DElement element = null;
    final IFE1D2DElement[] elements = upNode.getElements();
    for( final IFE1D2DElement anElement : elements )
    {
      if( anElement.getNodes().contains( downNode ) )
      {
        element = anElement;
        break;
      }
    }

    if( element == null )
      return null;

    final ICalculationUnit calculationUnit = m_controlModel.getCalculationUnit();
    final ICalculationUnit subUnit = CalcUnitOps.findSubUnit( calculationUnit, element );

    if( subUnit instanceof ICalculationUnit1D )
      return (ICalculationUnit1D) subUnit;

    return null;
  }

  /**
   * Triangulates the area between two profile curves. All created triangles will feed the triangle eaters.
   */
  private void create1dTriangles( final INodeResult[] nodes, final GM_Curve[] curves, final double curveDistance ) throws GM_Exception
  {
    final String crs = curves[0].getCoordinateSystem();

    // make a polygon from the curves (polygon must be oriented ccw)

    final GM_Position[] polygonPoses = GeometryUtilities.getPolygonfromCurves( curves );

    final GM_Position[][] triangles = GeometryUtilities.triangulateRing( polygonPoses );

    for( final GM_Position[] element : triangles )
      feedTriangleEaterWith1dResults( nodes, curves, curveDistance, crs, element );
  }

  private void create1dJunctionTriangles2( final List<INodeResult> nodeList, final List<GM_Curve> curveList ) throws GM_Exception
  {
    final GM_Curve[] curves = curveList.toArray( new GM_Curve[curveList.size()] );
    final INodeResult[] nodeResults = nodeList.toArray( new INodeResult[nodeList.size()] );

    final String crs = curveList.get( 0 ).getCoordinateSystem();

    final Set< GM_Position > lSetPositions = new HashSet<GM_Position>();
    for( final GM_Curve lCurve: curveList ){

      final GM_Position[] lPositions = lCurve.getAsLineString().getPositions();
      for( int i = 0; i < lPositions.length; ++i )
        lSetPositions.add( lPositions[ i ] );
    }
    final List< GM_Position > lListPos = new ArrayList<GM_Position>();
    lListPos.addAll( lSetPositions );
    if( !lListPos.get( 0 ).equals( lListPos.get( lListPos.size() - 1 ) ) ){
      lListPos.add( lListPos.get( 0 ) );
    }
    final GM_Triangle[] elements2 = ConstraintDelaunayHelper.convertToTriangles( lListPos.toArray( new GM_Position[ lListPos.size() ] ), crs );

    for( final GM_Triangle element : elements2 )
    {
      final GM_Position[] ring = element.getExteriorRing();
      feedTriangleEaterWith1dResults( nodeResults, curves, 1.0, crs, Arrays.copyOf( ring, 3 ) );
    }
  }
  private void feedTriangleEaterWith1dResults( final INodeResult[] nodeResults, final GM_Curve[] curves, final double curveDistance, final String crs, final GM_Position[] ring )
  {
    final INodeResult[] nodes = new INodeResult[3];

    // remove last position
    for( int i = 0; i < ring.length; i++ )
    {
      final GM_Position position = ring[i];
      final double x = position.getX();
      final double y = position.getY();
      double z = position.getZ(); // here: water level

      double wsp = 0;
      double vx = 0;
      double vy = 0;

      final GM_Point point = GeometryFactory.createGM_Point( position, crs );
      final GM_Object buffer = point.getBuffer( curveDistance / 2 );

      boolean intersectFound = false;

      for( int j = 0; j < curves.length; j++ )
      {
        if( buffer.intersects( curves[j] ) )
        {
          wsp = nodeResults[j].getWaterlevel();
          vx = nodeResults[j].getVelocity().get( 0 );
          vy = nodeResults[j].getVelocity().get( 1 );
          z = nodeResults[j].getPoint().getZ();
          intersectFound = true;
        }
      }
      if( !intersectFound )
        KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s ", Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.NodeResultsHandler.64" ) ); //$NON-NLS-1$ //$NON-NLS-2$

      final INodeResult node = new SimpleNodeResult();
      node.setLocation( x, y, z, crs );

      final List<Double> velocity = new LinkedList<Double>();
      velocity.add( vx );
      velocity.add( vy );
      node.setVelocity( velocity );
      node.setDepth( wsp - z );
      node.setWaterlevel( wsp );

      // node.setResultValues( vx, vy, 0, wsp );

      nodes[i] = node;
    }

    m_triangleEater.add( nodes );
  }

  /**
   * Splits the element into triangles. There are 6 / 8 triangles to be built. For each midside node there are two
   * triangles to be built. <br>
   * We begin with the first corner node, the first midside node and the center node.<br>
   * The second triangle is the second corner node, the first midside node and the center node.<br>
   */
  private void splitElement( final ElementResult elementResult )
  {
    final int numMidsideNodes = elementResult.getNumMidsideNodes();
    for( int i = 0; i < numMidsideNodes; i++ )
    {
      // First triangle
      final INodeResult[] nodes = new INodeResult[3];

      nodes[0] = elementResult.getCornerNodes( i );
      nodes[1] = elementResult.getMidsideNodes( i );
      nodes[2] = elementResult.getCenterNode();

      /* check, if the triangle is partially flooded */
      if( checkPartiallyFlooded( nodes ) )
      {
        /* split the triangle in to sub-triangles, if there are wet and dry nodes at the triangle */
        splitTriangle( nodes );
      }
      else
      {
        m_triangleEater.add( nodes );
      }

      // second triangle
      nodes[0] = elementResult.getMidsideNodes( i );
      if( i < numMidsideNodes - 1 )
        nodes[1] = elementResult.getCornerNodes( i + 1 );
      else
        nodes[1] = elementResult.getCornerNodes( 0 );
      nodes[2] = elementResult.getCenterNode();

      /* check, if the triangle is partially flooded */
      if( checkPartiallyFlooded( nodes ) )
      {
        /* split the triangle in to sub-triangles, if there are wet and dry nodes at the triangle */
        splitTriangle( nodes );
      }
      else
      {
        m_triangleEater.add( nodes );
      }
    }
  }

  /**
   * checks if the triangle has wet and dry nodes.
   */
  private boolean checkPartiallyFlooded( final INodeResult[] nodes )
  {
    boolean wet = false;
    boolean dry = false;
    for( final INodeResult node : nodes )
    {
      if( node.isWet() )
        wet = true;
      else
        dry = true;
      if( node.isWet() && dry )
        return true;
      else if( !node.isWet() && wet )
        return true;
    }
    return false;
  }

  /**
   * Splits the triangle into sub-triangles. At first, the arcs to split get identified and the split nodes are added to
   * the node list
   * 
   * @param nodes
   *          node list of the triangle to split. In this list the split nodes are added.
   */
  private void splitTriangle( final INodeResult[] nodes )
  {
    final List<Integer> splitArcs = new LinkedList<Integer>();

    final List<INodeResult> nodesInserted = new ArrayList<INodeResult>( 6 );
    nodesInserted.add( nodes[0] );
    nodesInserted.add( nodes[1] );
    nodesInserted.add( nodes[2] );

    // check the arcs
    if( NodeResultHelper.checkTriangleArc( nodes[0], nodes[1] ) )
    {
      // remember the split arc
      final INodeResult addedNode = insertNode( nodes[0], nodes[1] );
      if( addedNode != null )
      {
        splitArcs.add( 0 );
        nodesInserted.add( addedNode );
      }
    }
    if( NodeResultHelper.checkTriangleArc( nodes[1], nodes[2] ) )
    {
      // remember the split arc
      final INodeResult addedNode = insertNode( nodes[1], nodes[2] );
      if( addedNode != null )
      {
        splitArcs.add( 1 );
        nodesInserted.add( addedNode );
      }
    }
    if( NodeResultHelper.checkTriangleArc( nodes[2], nodes[0] ) )
    {
      // remember the split arc
      final INodeResult addedNode = insertNode( nodes[2], nodes[0] );
      if( addedNode != null )
      {
        splitArcs.add( 2 );
        nodesInserted.add( addedNode );
      }
    }

    switch( splitArcs.size() )
    {
      /* case1: one arc is split */
      case 1:
        splitIntoTwoTriangles( nodesInserted.toArray( new INodeResult[nodesInserted.size()] ), splitArcs.get( 0 ) );
        break;

        /* case2: two arcs were split */
      case 2:
        splitIntoThreeTriangles( nodesInserted.toArray( new INodeResult[nodesInserted.size()] ), splitArcs.get( 0 ), splitArcs.get( 1 ) );
        break;

      default:
        break;
    }
  }

  private void splitIntoThreeTriangles( final INodeResult[] nodes, final Integer arc1, final Integer arc2 )
  {
    if( arc1 == 0 && arc2 == 1 )
    {
      /*
       * the first inserted node lies between node0 and node1 the second between node1 and node2
       */

      /* check, if the corner node of the split arc is dry in order to save just the wet triangle */
      if( nodes[1].isWet() )
      {
        INodeResult[] triNodes = new INodeResult[3];

        /* triangle: inserted node0, inserted node1, node1 */
        triNodes[0] = nodes[3];
        triNodes[1] = nodes[1];
        triNodes[2] = nodes[4];

        if( checkPartiallyFlooded( triNodes ) )
          m_triangleEater.add( triNodes );

        /* triangle: inserted node1, node2, inserted node0 */
        triNodes[0] = nodes[0];
        triNodes[1] = nodes[3];
        triNodes[2] = nodes[2];

        m_triangleEater.add( triNodes );

        triNodes = new INodeResult[3];

        /* triangle: node0, inserted node0, node2 */
        triNodes[0] = nodes[3];
        triNodes[1] = nodes[4];
        triNodes[2] = nodes[2];

        m_triangleEater.add( triNodes );
      }
      else if( nodes[0].isWet() && nodes[2].isWet() )
      {
        INodeResult[] triNodes = new INodeResult[3];

        /* triangle: inserted node1, node2, inserted node0 */
        triNodes[0] = nodes[0];
        triNodes[1] = nodes[3];
        triNodes[2] = nodes[2];

        m_triangleEater.add( triNodes );

        triNodes = new INodeResult[3];

        /* triangle: node0, inserted node0, node2 */
        triNodes[0] = nodes[3];
        triNodes[1] = nodes[4];
        triNodes[2] = nodes[2];

        m_triangleEater.add( triNodes );

        triNodes = new INodeResult[3];

        /* triangle: inserted node0, inserted node1, node1 */
        triNodes[0] = nodes[3];
        triNodes[1] = nodes[1];
        triNodes[2] = nodes[4];

        m_triangleEater.add( triNodes );
      }
    }
    else if( arc1 == 0 && arc2 == 2 )
    {
      /*
       * the first inserted node lies between node0 and node1 the second between node2 and node0
       */

      /* check, if the corner node of the split arc is dry in order to save just the wet triangle */
      if( nodes[0].isWet() )
      {
        INodeResult[] triNodes = new INodeResult[3];

        /* triangle: inserted node0, inserted node1, node0 */
        triNodes[0] = nodes[3];
        triNodes[1] = nodes[4];
        triNodes[2] = nodes[0];

        m_triangleEater.add( triNodes );

        triNodes = new INodeResult[3];

        /* triangle: inserted node0, node1, inserted node1 */
        triNodes[0] = nodes[3];
        triNodes[1] = nodes[1];
        triNodes[2] = nodes[4];

        m_triangleEater.add( triNodes );

        triNodes = new INodeResult[3];

        /* triangle: node2, inserted node1, node1 */
        triNodes[0] = nodes[2];
        triNodes[1] = nodes[4];
        triNodes[2] = nodes[1];

        m_triangleEater.add( triNodes );
      }
      else if( nodes[1].isWet() && nodes[2].isWet() )
      {
        INodeResult[] triNodes = new INodeResult[3];

        /* triangle: inserted node0, node1, inserted node1 */
        triNodes[0] = nodes[3];
        triNodes[1] = nodes[1];
        triNodes[2] = nodes[4];

        m_triangleEater.add( triNodes );
        triNodes = new INodeResult[3];

        /* triangle: node2, inserted node1, node1 */
        triNodes[0] = nodes[2];
        triNodes[1] = nodes[4];
        triNodes[2] = nodes[1];

        m_triangleEater.add( triNodes );
        triNodes = new INodeResult[3];

        /* triangle: inserted node0, inserted node1, node0 */
        triNodes[0] = nodes[3];
        triNodes[1] = nodes[4];
        triNodes[2] = nodes[0];

        m_triangleEater.add( triNodes );
      }
    }
    else if( arc1 == 1 && arc2 == 2 )
    {
      /*
       * the first inserted node lies between node1 and node2 the second between node2 and node0
       */

      /* check, if the corner node of the split arc is dry in order to save just the wet triangle */
      if( nodes[2].isWet() )
      {
        INodeResult[] triNodes = new INodeResult[3];

        /* triangle0: inserted node0, node2, inserted node1 */
        triNodes[0] = nodes[3];
        triNodes[1] = nodes[2];
        triNodes[2] = nodes[4];

        m_triangleEater.add( triNodes );

        triNodes = new INodeResult[3];

        /* triangle: inserted node1, node1, inserted node0 */
        triNodes[0] = nodes[4];
        triNodes[1] = nodes[1];
        triNodes[2] = nodes[3];

        m_triangleEater.add( triNodes );

        triNodes = new INodeResult[3];

        /* triangle: node0, node1, inserted node2 */
        triNodes[0] = nodes[0];
        triNodes[1] = nodes[1];
        triNodes[2] = nodes[4];

        m_triangleEater.add( triNodes );
      }
      else if( nodes[0].isWet() && nodes[1].isWet() )
      {
        INodeResult[] triNodes = new INodeResult[3];

        /* triangle: inserted node1, node1, inserted node0 */
        triNodes[0] = nodes[4];
        triNodes[1] = nodes[1];
        triNodes[2] = nodes[3];

        m_triangleEater.add( triNodes );

        triNodes = new INodeResult[3];

        /* triangle: node0, node1, inserted node2 */
        triNodes[0] = nodes[0];
        triNodes[1] = nodes[1];
        triNodes[2] = nodes[4];

        m_triangleEater.add( triNodes );

        triNodes = new INodeResult[3];
        /* triangle0: inserted node0, node2, inserted node1 */
        triNodes[0] = nodes[3];
        triNodes[1] = nodes[2];
        triNodes[2] = nodes[4];

        m_triangleEater.add( triNodes );
      }
      else
      {
        KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.NodeResultsHandler.66" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      }
    }
  }

  /**
   * One corner node is part of the inundation line and one arc is split => two triangles.<br>
   * There are three possibilities depending on the split arc.<br>
   * - the inserted node lies between node0 and node1.<br>
   * - the inserted node lies between node1 and node2.<br>
   * - the inserted node lies between node2 and node0.<br>
   * 
   * @param nodes
   *          list of all nodes of the triangle to split
   * @param splitArcs
   *          list of the split arc numbers (here just one)
   */
  private void splitIntoTwoTriangles( final INodeResult[] nodes, final Integer splitArc )
  {
    if( splitArc == 0 )
    {
      if( nodes[1].isWet() )
      {
        final INodeResult[] triNodes = new INodeResult[3];

        triNodes[0] = nodes[3];
        triNodes[1] = nodes[1];
        triNodes[2] = nodes[2];

        m_triangleEater.add( triNodes );

        triNodes[0] = nodes[3];
        triNodes[1] = nodes[2];
        triNodes[2] = nodes[0];

        m_triangleEater.add( triNodes );
      }
      else if( nodes[0].isWet() )
      {
        final INodeResult[] triNodes = new INodeResult[3];

        triNodes[0] = nodes[3];
        triNodes[1] = nodes[2];
        triNodes[2] = nodes[0];

        m_triangleEater.add( triNodes );

        triNodes[0] = nodes[3];
        triNodes[1] = nodes[1];
        triNodes[2] = nodes[2];

        m_triangleEater.add( triNodes );
      }
    }
    else if( splitArc == 1 )
    {
      if( nodes[1].isWet() )
      {
        final INodeResult[] triNodes = new INodeResult[3];

        triNodes[0] = nodes[0];
        triNodes[1] = nodes[1];
        triNodes[2] = nodes[3];

        m_triangleEater.add( triNodes );

        triNodes[0] = nodes[0];
        triNodes[1] = nodes[3];
        triNodes[2] = nodes[2];

        m_triangleEater.add( triNodes );
      }
      else if( nodes[2].isWet() )
      {
        final INodeResult[] triNodes = new INodeResult[3];

        triNodes[0] = nodes[0];
        triNodes[1] = nodes[3];
        triNodes[2] = nodes[2];

        m_triangleEater.add( triNodes );

        triNodes[0] = nodes[0];
        triNodes[1] = nodes[1];
        triNodes[2] = nodes[3];

        m_triangleEater.add( triNodes );
      }
    }
    else if( splitArc == 2 )
    {
      if( nodes[0].isWet() )
      {
        final INodeResult[] triNodes = new INodeResult[3];

        triNodes[0] = nodes[3];
        triNodes[1] = nodes[0];
        triNodes[2] = nodes[1];

        m_triangleEater.add( triNodes );

        triNodes[0] = nodes[3];
        triNodes[1] = nodes[1];
        triNodes[2] = nodes[2];

        m_triangleEater.add( triNodes );

      }
      else if( nodes[2].isWet() )
      {
        final INodeResult[] triNodes = new INodeResult[3];

        triNodes[0] = nodes[3];
        triNodes[1] = nodes[1];
        triNodes[2] = nodes[2];

        m_triangleEater.add( triNodes );

        triNodes[0] = nodes[3];
        triNodes[1] = nodes[0];
        triNodes[2] = nodes[1];

        m_triangleEater.add( triNodes );
      }
    }
    else
    {
      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.NodeResultsHandler.68" ) ); //$NON-NLS-1$ //$NON-NLS-2$
    }
  }

  private INodeResult insertNode( final INodeResult node1, final INodeResult node2 )
  {
    final INodeResult insertedNode = new SimpleNodeResult();

    final double wspNode1 = node1.getWaterlevel();
    final GM_Point point1 = node1.getPoint();
    final double zNode1 = point1.getZ();
    final double wspNode2 = node2.getWaterlevel();
    final GM_Point point2 = node2.getPoint();
    final double zNode2 = point2.getZ();

    // getting the z-value for the zero point
    final double z3;
    final double dz1;
    final double dz2;

    if( node1.isWet() )
    {
      z3 = node1.getWaterlevel();
      if( wspNode1 > zNode2 )
      {
        dz1 = wspNode1 - zNode1;
        dz2 = wspNode2 - zNode2;
      }
      else
      {
        dz1 = wspNode1 - zNode1;
        dz2 = wspNode1 - zNode2;
      }
    }
    else if( (wspNode1 - zNode1) < 0 )
    {
      z3 = node2.getWaterlevel();
      if( wspNode2 > zNode1 )
      {
        dz1 = zNode1 - wspNode1;
        dz2 = zNode2 - wspNode2;
      }
      else
      {
        dz1 = zNode1 - wspNode2;
        dz2 = zNode2 - wspNode2;
      }
    }
    else
      // dz1 = 0
      return null;

    // => dz1 always > 0 and dz2 always < 0 !!

    // getting the zero point
    final double dist12 = point2.distance( point1 );
    final double dist13 = NodeResultHelper.getZeroPoint( dist12, dz1, dz2 );

    // getting the geo-coordinates of the zero-point
    final double dx12 = point2.getX() - point1.getX();
    final double dy12 = point2.getY() - point1.getY();

    final double x3 = point1.getX() + (dist13 / dist12) * dx12;
    final double y3 = point1.getY() + (dist13 / dist12) * dy12;

    final String crs = point1.getCoordinateSystem();

    insertedNode.setLocation( x3, y3, z3, crs );
    final List<Double> velocity = new LinkedList<Double>();
    velocity.add( 0.0 );
    velocity.add( 0.0 );
    insertedNode.setVelocity( velocity );
    insertedNode.setDepth( 0.0 );
    insertedNode.setWaterlevel( z3 );

    return insertedNode;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleNode(java.lang.String, int, double, double,
   *      double)
   */
  @Override
  public void handleNode( final String lineString, final int id, final double easting, final double northing, final double elevation )
  {
    final Feature parentFeature = m_resultList.getParentFeature();
    final IRelationType parentRelation = m_resultList.getParentFeatureTypeProperty();

    try
    {
      /* Create new Node-Result */
      final Feature feature = m_resultWorkspace.createFeature( parentFeature, parentRelation, parentRelation.getTargetFeatureType() );
      m_resultWorkspace.addFeatureAsComposition( parentFeature, parentRelation, -1, feature );

      /* Remember node result for additional result data */
      final GMLNodeResult result = new GMLNodeResult( feature );
      m_nodeIndex.put( id, result );

      /* Fill node result with data */
      result.setName( "" + id ); //$NON-NLS-1$

      // TODO: description: beschreibt, welche Rechenvariante und so weiter... oder noch besser an der collection
      // result.setDescription( "" + id );

      result.setCalcId( id ); 
      result.setLocation( easting, northing, elevation, m_crs );
      // if swan results found.
      if( m_mapSWANResults != null )
      { 
        final GM_Position lPositionKey = GeometryFactory.createGM_Position( NumberUtils.getRoundedToSignificant( easting, SWANResultsReader.INT_ROUND_SIGNIFICANT ), NumberUtils.getRoundedToSignificant( northing, SWANResultsReader.INT_ROUND_SIGNIFICANT ) );
        try
        { 
          result.setWaveDirection( m_mapWAVEDir.get( lPositionKey ) );
          result.setWaveHsig( m_mapWAVEHsig.get( lPositionKey ) );
          result.setWavePeriod( m_mapWAVEPeriod.get( lPositionKey ) );
          m_resultMinMaxCatcher.addNodeResult( result );
        }
        catch( final Exception e )
        {
          // //middle nodes that cannot be found in output of swan, will be ignored here
          // e.printStackTrace();
        }
      }
      /* check min/max values */
      // TODO: velocity not yet set here?
      // m_resultMinMaxCatcher.addNodeResult( result );
      // TODO: move to handleResult stuff?
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handlerError(java.lang.String,
   *      org.kalypso.kalypsomodel1d2d.conv.EReadError)
   */
  @Override
  public void handleError( final String lineString, final EReadError errorHints )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handlerUnIdentifyable(java.lang.String)
   */
  @Override
  public void handlerUnIdentifyable( final String lineString )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#start()
   */
  @Override
  public void start( )
  {
    if( m_mapSWANResults == null )
    {
      return;
    }
    final Set<String> lKeysSet = m_mapSWANResults.keySet();
    for( final Object element : lKeysSet )
    {
      final String strKey = (String) element;
      if( strKey.toLowerCase().contains( ISimulation1D2DConstants.SIM_SWAN_HSIG_OUT_PARAM.toLowerCase() ) )
      {
        m_mapWAVEHsig = m_mapSWANResults.get( strKey );
      }
      else if( strKey.toLowerCase().contains( ISimulation1D2DConstants.SIM_SWAN_DIRECTION_OUT_PARAM.toLowerCase() ) )
      {
        m_mapWAVEDir = m_mapSWANResults.get( strKey );
      }
      else if( strKey.toLowerCase().contains( ISimulation1D2DConstants.SIM_SWAN_PERIOD_OUT_PARAM.toLowerCase() ) )
      {
        m_mapWAVEPeriod = m_mapSWANResults.get( strKey );
      }
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleResult(java.lang.String, int, double,
   *      double, double, double)
   */
  @Override
  public void handleResult( final String lineString, final int id, final double vx, final double vy, final double virtualDepth, final double waterlevel )
  {
    final INodeResult result = m_nodeIndex.get( id );
    if( result == null )
    {
      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s %d ", Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.NodeResultsHandler.71" ), id ); //$NON-NLS-1$ //$NON-NLS-2$
      return;
    }

    // check if node is dry
    result.setResultValues( vx, vy, virtualDepth, waterlevel );

    m_resultMinMaxCatcher.addNodeResult( result );

  }

  @Override
  public void handleTimeDependentAdditionalResult( final String lineString, final int id, final double velXComponent, final double velYComponent, final double depthComponent, final RESULTLINES resultlines )
  {
    final INodeResult result = m_nodeIndex.get( id );

    if( result == null )
    {
      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s %d ", Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.NodeResultsHandler.73" ), id ); //$NON-NLS-1$ //$NON-NLS-2$
      return;
    }
    switch( resultlines )
    {
      case LINE_VO:
        result.setResultPrevStepValues( velXComponent, velYComponent, depthComponent );

        break;
      case LINE_GA:
        result.setTimeDerivativeValues( velXComponent, velYComponent, depthComponent );

        break;
      case LINE_GO:
        result.setTimeDerivativeValuesPrevStep( velXComponent, velYComponent, depthComponent );

        break;
      case LINE_VA:
        System.out.println( "normally the handleResult function can be called without water stage information! The 2D-file may be 'broken'" );
        break;

        // TODO: catch LINE_VA case and print message; normally the handleResult function can be called without water
        // stage information!
        // Normally this shouldn't happen, because otherwise the 2D-file is 'broken'

    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleTime(java.lang.String, java.util.Date)
   */
  @Override
  public void handleTime( final String line, final Date time )
  {
    m_time = time;
    m_triangleEater.setTime( time );
  }

  /**
   * Returns the time which was read from the .2d file.
   */
  public Date getTime( )
  {
    return m_time;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleJunction(java.lang.String, int, int, int,
   *      int)
   */
  @Override
  public void handleJunction( final String parseLine, final int junctionID, final int element1dID, final int boundaryLine2dID, final int node1dID )
  {
    // TODO: implement
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleNodeInformation(java.lang.String, int, int,
   *      double, double, double, double)
   */
  @Override
  public void handleNodeInformation( final String line, final int id, final int dry, final double value1, final double value2, final double value3, final double value4 )
  {
    final INodeResult result = m_nodeIndex.get( id );
    if( result == null )
    {
      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s %d ", Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.NodeResultsHandler.75" ), id ); //$NON-NLS-1$ //$NON-NLS-2$
      return;
    }
    result.setDry( dry );

    /* check dry nodes for strange values */
    // if( dry != 1 )
    // {
    // final double waterlevel = result.getWaterlevel();
    // final double z = result.getPoint().getZ();
    //
    // /* a dry node can not have a water level above datum */
    // if( waterlevel - z > 0 )
    // {
    // result.setWaterlevel( z );
    // result.setDepth( 0.0 );
    // }
    // }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handle1dJunctionInformation(java.lang.String,
   *      int, java.util.List)
   */
  @Override
  public void handle1dJunctionInformation( final String line, final int junctionId, final List<Integer> junctionNodeIDList )
  {

    try
    {
      // get all junction profile curves
      final List<GM_Curve> profileCurveList = new ArrayList<GM_Curve>();
      final List<INodeResult> nodeList = new ArrayList<INodeResult>();

      for( final Integer nodeID : junctionNodeIDList )
      {
        if( nodeID == null )
          continue;

        final INodeResult nodeResult = m_nodeIndex.get( nodeID );

        final IFlowRelationship lFlowRel = getFlowRelation( nodeResult );
        if( !(lFlowRel instanceof IFlowRelation1D) )
        {
          break;
        }
        final IFlowRelation1D teschkeRelation = (IFlowRelation1D) lFlowRel;

        final IProfileFeature profile = teschkeRelation.getProfile();

        // get the profile Curves of the two nodes defining the current element
        final GM_Curve curve = NodeResultHelper.getProfileCurveFor1dNode( profile );
        // TODO: maybe cut all curves at possible intersection points(?)

        profileCurveList.add( curve );
        nodeList.add( nodeResult );
      }

      /* now we have all curves and use Triangle in order to get the triangles */

      if( nodeList.size() > 1 )
        create1dJunctionTriangles2( nodeList, profileCurveList );
    }
    catch( final Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

  }
}
