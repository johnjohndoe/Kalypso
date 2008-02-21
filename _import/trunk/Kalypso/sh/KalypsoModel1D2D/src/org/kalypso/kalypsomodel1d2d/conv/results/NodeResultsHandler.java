/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.gml.processes.constDelaunay.ConstraintDelaunayHelper;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DDebug;
import org.kalypso.kalypsomodel1d2d.conv.EReadError;
import org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler;
import org.kalypso.kalypsomodel1d2d.conv.IRoughnessIDProvider;
import org.kalypso.kalypsomodel1d2d.conv.results.lengthsection.LengthSectionHandler1d;
import org.kalypso.kalypsomodel1d2d.ops.CalcUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IContinuityLine2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IFlowRelation1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.ITeschkeFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.GMLNodeResult;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResultCollection;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.SimpleNodeResult;
import org.kalypso.kalypsomodel1d2d.sim.NodeResultMinMaxCatcher;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.schema.IWspmDictionaryConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.model.feature.index.FeatureIndex;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.sort.SplitSort;
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

  public NodeResultsHandler( final GMLWorkspace resultWorkspace, final ITriangleEater triangleEater, final IFlowRelationshipModel flowModel, final IControlModel1D2D controlModel, final IFEDiscretisationModel1d2d discModel, final NodeResultMinMaxCatcher resultMinMaxCatcher, final LengthSectionHandler1d lsHandler )
  {
    m_resultWorkspace = resultWorkspace;
    m_triangleEater = triangleEater;
    m_flowModel = flowModel;
    m_controlModel = controlModel;
    m_discModel = discModel;
    m_resultMinMaxCatcher = resultMinMaxCatcher;
    m_lsHandler = lsHandler;
    m_resultList = (FeatureList) m_resultWorkspace.getRootFeature().getProperty( INodeResultCollection.QNAME_PROP_NODERESULT_MEMBER );

    m_crs = KalypsoCorePlugin.getDefault().getCoordinatesSystem();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#end()
   */
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
    if( count > WSP_EXTRAPOLATION_RANGE )
      return;
    int assigned = 0;
    for( final ElementResult element : m_elemIndex.values() )
    {
      if( element.checkWaterlevels() == true )
        assigned = assigned + 1;
    }
    if( assigned > 0 )
    {
      count = count + 1;
      System.out.println( "Anzahl zugewiesener Wasserspiegel: " + assigned );
      System.out.println( "Anzahl Aufrufe extrapolateWaterLevel: " + count );
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
    if( m_elemIndex.containsKey( elementID ) == true )
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

  public void handleFlowResitance( final String lineString, final int id, final double combinedLambda, final double soilLambda, final double vegetationLambda )
  {
    /*
     * IT IS NECESSARY, THAT THE FLOW RESISTANCE VALUES ARE STANDING BELOW THE ELEMENTS IN THE 2D-RESULT FILE!!
     */
    if( m_elemIndex.containsKey( id ) == true )
    {
      // get the element
      final ElementResult elementResult = m_elemIndex.get( id );

      // for calculating the shear stress we just use the lambda of the soil.
      elementResult.setLambda( soilLambda );
    }
    else
    {
      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", "element does not exist in the arc list, check model!" );
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleElement(java.lang.String, int, int, int,
   *      int)
   */
  @SuppressWarnings("unchecked")
  public void handleElement( final String lineString, final int id, final int currentRougthnessClassID, final int previousRoughnessClassID, final int eleminationNumber )
  {
    // For each element calculate the geometry (elemID, cornernode1, midsidenode1, cornernode2, midsidenode2,

    // cornernode3, midsidenode3, (cornernode4, midsidenode4), element type)
    /* =========== calculate the geometry ============= */

    /*
     * right here, all element objects should have been created by the arcHandler
     * 
     * IT IS NECESSARY, THAT THE ELEMENTS ARE STANDING BELOW THE ARCS IN THE 2D-RESULT FILE!!
     */
    if( m_elemIndex.containsKey( id ) == true )
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
            KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", "error while handling 1d element:" + status.getMessage() );

        }
        catch( final Exception e )
        {
          KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", "exception while handling 1d element." );
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
          KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", "exception while handling 1d element." );
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
      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", "element does not exist in the arc list, check model!" );
    }
  }

  private IFlowRelation1D getFlowRelation( final INodeResult nodeResult )
  {
    final GM_Position nodePos = nodeResult.getPoint().getPosition();

    final IFlowRelationship[] flowRelationships = m_flowModel.findFlowrelationships( nodePos, NODE_SEARCH_DIST );

    // TODO: for some reason there are flow relations that are from type BoundaryCondition
    // go through the found array and get the first found teschke flow relation
    for( final IFlowRelationship element : flowRelationships )
    {
      if( element instanceof IFlowRelation1D )
      {
        return (IFlowRelation1D) element;
      }
    }
    return null;
  }

  /**
   * collects the necessary data for the length section and stores it in an observation.
   */
  private IStatus handleLengthSectionData( final INodeResult nodeResult, final IFlowRelation1D flowRelation1d, final ICalculationUnit1D calcUnit )
  {
    final WspmProfile profile = flowRelation1d.getProfile();
    final double profileStation = profile.getStation();
    // station
    final BigDecimal station = WspmProfile.stationToBigDecimal( profileStation );

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
      return StatusUtilities.createErrorStatus( "Es konnten keine Profilflächendaten gelesen werden (Knoten: " + nodeResult.getGmlID() + ", Station: " + station.doubleValue() );

    final BigDecimal discharge = velocity.multiply( area ).setScale( 3, BigDecimal.ROUND_HALF_UP );
    nodeResult.setDischarge( discharge.doubleValue() );

    // markers for Trennflächen / Bordvollpunkte u.ä.
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
      final IFlowRelation1D flowRelation1d = getFlowRelation( nodes[i] );

      if( flowRelation1d == null )
        break;

      /* check, if node was already handled for lengthsection */
      if( !m_lengthsection1dNodes.contains( nodes[i].getGmlID() ) )
      {
        final IStatus status = handleLengthSectionData( nodes[i], flowRelation1d, calcUnit );
        // TODO: right now, no consequences of the returned status
        m_lengthsection1dNodes.add( nodes[i].getGmlID() );
      }

      final WspmProfile profile = flowRelation1d.getProfile();

      // get the profile Curves of the two nodes defining the current element
      curves[i] = NodeResultHelper.getProfileCurveFor1dNode( profile );
    }

    /* Probably profile information missing */
    if( curves[0] == null || curves[1] == null )
      return StatusUtilities.createErrorStatus( "Error while handling 1d-result file: There are no information of the profile in the model." );

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
  @SuppressWarnings("unchecked")
  private void create1dTriangles( final INodeResult[] nodes, final GM_Curve[] curves, final double curveDistance ) throws GM_Exception
  {
    final String crs = curves[0].getCoordinateSystem();

    // make a polygon from the curves (polygon must be oriented ccw)

    final GM_Position[] polygonPoses = GeometryUtilities.getPolygonfromCurves( curves );

    final GM_Position[][] triangles = GeometryUtilities.triangulateRing( polygonPoses );

    for( final GM_Position[] element : triangles )
      feedTriangleEaterWith1dResults( nodes, curves, curveDistance, crs, element );
  }

  @SuppressWarnings("unchecked")
  private void createJunctionTriangles( final INodeResult nodeResult1d, final FeatureList resultList, final GM_Curve nodeCurve1d, final GM_Curve boundaryCurve, final double curveDistance ) throws FileNotFoundException, IOException, CoreException, InterruptedException, GM_Exception
  {
    BufferedReader nodeReader = null;
    BufferedReader eleReader = null;
    PrintWriter pwSimuLog;

    final String crs = nodeCurve1d.getCoordinateSystem();

    final List<GM_Curve> breaklines = new ArrayList<GM_Curve>( 2 );
    breaklines.add( nodeCurve1d );
    breaklines.add( boundaryCurve );

    pwSimuLog = new PrintWriter( System.out );

    final File tempDir = FileUtilities.createNewTempDir( "Triangle" );
    final File polyfile = new File( tempDir, "input.poly" );

    BufferedOutputStream strmPolyInput = null;

    try
    {
      strmPolyInput = new BufferedOutputStream( new FileOutputStream( polyfile ) );
      ConstraintDelaunayHelper.writePolyFileForLinestrings( strmPolyInput, breaklines, pwSimuLog );
      strmPolyInput.close();

      // create command
      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s ", "create command" );
      final StringBuffer cmd = ConstraintDelaunayHelper.createTriangleCommand( polyfile );

      // start Triangle
      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s ", "start Triangle" );
      ConstraintDelaunayHelper.execTriangle( pwSimuLog, tempDir, cmd );

      // get the triangulation
      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s ", "get the triangulation" );
      final File nodeFile = new File( tempDir, "input.1.node" );
      final File eleFile = new File( tempDir, "input.1.ele" );

      if( !nodeFile.exists() || !eleFile.exists() )
      {
        pwSimuLog.append( "Fehler beim Ausführen von triangle.exe" );
        pwSimuLog.append( "Stellen Sie sicher, dass triangle.exe über die Windows PATH-Variable auffindbar ist." );
        pwSimuLog.append( "Fehler: triangle.exe konnte nicht ausgeführt werden." );
        KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s ", "error while executing triangle.exe" );
        return;
      }

      nodeReader = new BufferedReader( new InputStreamReader( new FileInputStream( nodeFile ) ) );
      eleReader = new BufferedReader( new InputStreamReader( new FileInputStream( eleFile ) ) );

      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s ", "parse triangle nodes" );
      final GM_Position[] points = ConstraintDelaunayHelper.parseTriangleNodeOutput( nodeReader );

      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s ", "parse triangle elements" );
      final List<GM_Surface> elements = ConstraintDelaunayHelper.parseTriangleElementOutput( eleReader, crs, points );

      for( final GM_Surface<GM_SurfacePatch> element : elements )
      {
        for( final GM_SurfacePatch surfacePatch : element )
        {
          final GM_Position[] ring = surfacePatch.getExteriorRing();
          feedTriangleEaterWithJunctionResults( nodeResult1d, resultList, nodeCurve1d, boundaryCurve, curveDistance, crs, ring );
        }
      }
    }
    finally
    {
      IOUtils.closeQuietly( nodeReader );
      IOUtils.closeQuietly( eleReader );
      IOUtils.closeQuietly( pwSimuLog );
      IOUtils.closeQuietly( strmPolyInput );
      FileUtilities.deleteRecursive( tempDir );
    }
  }

  @SuppressWarnings("unchecked")
  private void create1dJunctionTriangles( final List<INodeResult> nodeList, final List<GM_Curve> curveList ) throws FileNotFoundException, IOException, CoreException, InterruptedException, GM_Exception
  {
    BufferedReader nodeReader = null;
    BufferedReader eleReader = null;
    PrintWriter pwSimuLog;

    final GM_Curve[] curves = curveList.toArray( new GM_Curve[curveList.size()] );
    final INodeResult[] nodeResults = nodeList.toArray( new INodeResult[nodeList.size()] );

    final String crs = curveList.get( 0 ).getCoordinateSystem();

    pwSimuLog = new PrintWriter( System.out );

    final File tempDir = FileUtilities.createNewTempDir( "Triangle" );
    final File polyfile = new File( tempDir, "input.poly" );

    BufferedOutputStream strmPolyInput = null;

    try
    {
      strmPolyInput = new BufferedOutputStream( new FileOutputStream( polyfile ) );
      ConstraintDelaunayHelper.writePolyFileForLinestrings( strmPolyInput, curveList, pwSimuLog );
      strmPolyInput.close();

      // create command
      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s ", "create command" );
      final StringBuffer cmd = ConstraintDelaunayHelper.createTriangleCommand( polyfile );

      // start Triangle
      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s ", "start Triangle" );
      ConstraintDelaunayHelper.execTriangle( pwSimuLog, tempDir, cmd );

      // get the triangulation
      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s ", "get the triangulation" );
      final File nodeFile = new File( tempDir, "input.1.node" );
      final File eleFile = new File( tempDir, "input.1.ele" );

      if( !nodeFile.exists() || !eleFile.exists() )
      {
        pwSimuLog.append( "Fehler beim Ausführen von triangle.exe" );
        pwSimuLog.append( "Stellen Sie sicher, dass triangle.exe über die Windows PATH-Variable auffindbar ist." );
        pwSimuLog.append( "Fehler: triangle.exe konnte nicht ausgeführt werden." );
        KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s ", "error while executing triangle.exe" );
        return;
      }

      nodeReader = new BufferedReader( new InputStreamReader( new FileInputStream( nodeFile ) ) );
      eleReader = new BufferedReader( new InputStreamReader( new FileInputStream( eleFile ) ) );

      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s ", "parse triangle nodes" );
      final GM_Position[] points = ConstraintDelaunayHelper.parseTriangleNodeOutput( nodeReader );

      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s ", "parse triangle elements" );
      final List<GM_Surface> elements = ConstraintDelaunayHelper.parseTriangleElementOutput( eleReader, crs, points );

      for( final GM_Surface<GM_SurfacePatch> element : elements )
      {
        for( final GM_SurfacePatch surfacePatch : element )
        {
          final GM_Position[] ring = surfacePatch.getExteriorRing();

          final GM_Position[] poses = new GM_Position[3];
          /* remove last position */
          for( int i = 0; i < ring.length - 1; i++ )
          {
            poses[i] = ring[i];
          }

          feedTriangleEaterWith1dResults( nodeResults, curves, 1.0, crs, poses );
        }
      }
    }
    finally
    {
      IOUtils.closeQuietly( nodeReader );
      IOUtils.closeQuietly( eleReader );
      IOUtils.closeQuietly( pwSimuLog );
      IOUtils.closeQuietly( strmPolyInput );
      FileUtilities.deleteRecursive( tempDir );
    }
  }

  private void feedTriangleEaterWithJunctionResults( final INodeResult nodeResult1d, final FeatureList resultList, final GM_Curve nodeCurve1d, final GM_Curve boundaryCurve, final double curveDistance, final String crs, final GM_Position[] ring )
  {
    final List<INodeResult> nodes = new LinkedList<INodeResult>();

    for( int i = 0; i < ring.length - 1; i++ )
    {
      final GM_Position position = ring[i];
      final double x = position.getX();
      final double y = position.getY();
      final double z = position.getZ(); // here: water level

      double wsp = 0;
      double vx = 0;
      double vy = 0;
      double h = 0;

      final GM_Point point = GeometryFactory.createGM_Point( position, crs );

      final double grabDistance = curveDistance / 2;
      final GM_Object buffer = point.getBuffer( grabDistance );

      // TODO: what happens, if the boundaryCurve intersects the 1d-profile ?
      if( nodeCurve1d.intersects( boundaryCurve ) )
        KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s ", "JunctionElement: boundary line intersects 1d-profile." );
      else
      {
        if( buffer.intersects( nodeCurve1d ) == true )
        {
          wsp = nodeResult1d.getWaterlevel();
          vx = nodeResult1d.getVelocity().get( 0 );
          vy = nodeResult1d.getVelocity().get( 1 );
        }
        else if( buffer.intersects( boundaryCurve ) == true )
        {
          wsp = nodeResult1d.getWaterlevel(); // water level is the same for all junction nodes

          // search for the nearest nodeResult in the FeatureList
          final Feature feature = GeometryUtilities.findNearestFeature( point, grabDistance, resultList, GMLNodeResult.QNAME_PROP_LOCATION );
          final INodeResult nodeResult = feature == null ? null : (INodeResult) feature.getAdapter( INodeResult.class );
          if( nodeResult != null )
          {
            final INodeResult node = (INodeResult) feature;

            vx = node.getVelocity().get( 0 );
            vy = node.getVelocity().get( 1 );
            h = node.getDepth();
          }
        }
        else
        {
          KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s ", "JunctionElement: no profile points found." );
        }
      }

      final INodeResult node = new SimpleNodeResult();
      node.setLocation( x, y, z, crs );
      // node.setResultValues( vx, vy, h, wsp );

      final List<Double> velocity = new LinkedList<Double>();
      velocity.add( vx );
      velocity.add( vy );
      node.setVelocity( velocity );
      node.setDepth( h );
      node.setWaterlevel( wsp );

      nodes.add( node );
    }

    m_triangleEater.add( nodes, true );

  }

  private void feedTriangleEaterWith1dResults( final INodeResult[] nodeResults, final GM_Curve[] curves, final double curveDistance, final String crs, final GM_Position[] ring )
  {
    final List<INodeResult> nodes = new LinkedList<INodeResult>();

    for( final GM_Position position : ring )
    {
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
        if( buffer.intersects( curves[j] ) == true )
        {
          wsp = nodeResults[j].getWaterlevel();
          vx = nodeResults[j].getVelocity().get( 0 );
          vy = nodeResults[j].getVelocity().get( 1 );
          z = nodeResults[j].getPoint().getZ();
          intersectFound = true;
        }
      }
      if( intersectFound == false )
        KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s ", "no profile points found." );

      final INodeResult node = new SimpleNodeResult();
      node.setLocation( x, y, z, crs );

      final List<Double> velocity = new LinkedList<Double>();
      velocity.add( vx );
      velocity.add( vy );
      node.setVelocity( velocity );
      node.setDepth( wsp - z );
      node.setWaterlevel( wsp );

      // node.setResultValues( vx, vy, 0, wsp );

      nodes.add( node );
    }

    if( nodes.size() == 3 )
      m_triangleEater.add( nodes, true );
  }

  /**
   * Splits the element into triangles.
   * 
   * There are 6 / 8 triangles to be built. For each midside node there are two triangles to be built. <br>
   * We begin with the first corner node, the first midside node and the center node.<br>
   * The second triangle is the second corner node, the first midside node and the center node.<br>
   */
  private void splitElement( final ElementResult elementResult )
  {
    final int numMidsideNodes = elementResult.getNumMidsideNodes();
    if( numMidsideNodes == 0 )
      return;
    for( int i = 0; i < numMidsideNodes; i++ )
    {
      // First triangle
      final List<INodeResult> nodeList = new LinkedList<INodeResult>();

      final INodeResult[] nodes = new INodeResult[3];

      nodes[0] = elementResult.getCornerNodes( i );
      nodes[1] = elementResult.getMidsideNodes( i );
      nodes[2] = elementResult.getCenterNode();

      if( checkIfTriangleWet( nodes ) == true )
      {

        // int orient = checkOrientation( nodes );

        // if( orient == 1 )
        // {
        nodeList.add( nodes[0] );
        nodeList.add( nodes[1] );
        nodeList.add( nodes[2] );
        // }
        // else
        // {
        // nodeList.add( nodes[0] );
        // nodeList.add( nodes[2] );
        // nodeList.add( nodes[1] );
        // }

        /* check, if the triangle is partially flooded */
        if( checkPartiallyFlooded( nodeList ) == true )
        {
          /* split the triangle in to sub-triangles, if there are wet and dry nodes at the triangle */
          splitTriangle( nodeList );
          nodeList.clear();
        }
        else
        {
          m_triangleEater.add( nodeList, true );
          nodeList.clear();
        }
      }
      else
      {
        nodeList.add( nodes[0] );
        nodeList.add( nodes[1] );
        nodeList.add( nodes[2] );

        m_triangleEater.add( nodeList, false );
        nodeList.clear();
      }

      // second triangle
      nodes[0] = elementResult.getMidsideNodes( i );
      if( i < numMidsideNodes - 1 )
        nodes[1] = elementResult.getCornerNodes( i + 1 );
      else
        nodes[1] = elementResult.getCornerNodes( 0 );
      nodes[2] = elementResult.getCenterNode();

      if( checkIfTriangleWet( nodes ) == true )
      {

        // right now there is no need to check the orientation. But maybe it is needed later.
        //
        // orient = checkOrientation( nodes );
        //
        // if( orient == 1 )
        // {
        nodeList.add( nodes[0] );
        nodeList.add( nodes[1] );
        nodeList.add( nodes[2] );
        // }
        // else
        // {
        // nodeList.add( nodes[0] );
        // nodeList.add( nodes[2] );
        // nodeList.add( nodes[1] );
        // }

        /*
         * nodeList.add( elementResult.getMidsideNodes( i ) ); if( i < numMidsideNodes - 1 ) nodeList.add(
         * elementResult.getCornerNodes( i + 1 ) ); else nodeList.add( elementResult.getCornerNodes( 0 ) );
         * 
         * nodeList.add( elementResult.getCenterNode() );
         */
        /* check, if the triangle is partially flooded */
        if( checkPartiallyFlooded( nodeList ) == true )
        {
          /* split the triangle in to sub-triangles, if there are wet and dry nodes at the triangle */
          splitTriangle( nodeList );
          nodeList.clear();
        }
        else
        {
          m_triangleEater.add( nodeList, true );
          nodeList.clear();
        }
      }
      else
      {
        nodeList.add( nodes[0] );
        nodeList.add( nodes[1] );
        nodeList.add( nodes[2] );

        m_triangleEater.add( nodeList, false );
        nodeList.clear();
      }
    }
  }

  private boolean checkIfTriangleWet( final INodeResult[] nodes )
  {
    for( final INodeResult node : nodes )
    {
      if( node.isWet() == true )
        return true;
    }
    return false;
  }

  /**
   * checks if the triangle has wet and dry nodes.
   */
  private boolean checkPartiallyFlooded( final List<INodeResult> nodes )
  {
    boolean wet = false;
    boolean dry = false;

    for( final INodeResult node : nodes )
    {
      if( node.getDepth() <= 0 )
        dry = true;
      else
        wet = true;
    }

    if( wet == true && dry == true )
      return true;
    else
      return false;
  }

  /**
   * Splits the triangle into sub-triangles. At first, the arcs to split get identified and the split nodes are added to
   * the node list
   * 
   * @param nodes
   *            node list of the triangle to split. In this list the split nodes are added.
   */
  private void splitTriangle( final List<INodeResult> nodes )
  {

    final List<Integer> splitArcs = new LinkedList<Integer>();

    // check the arcs
    if( NodeResultHelper.checkTriangleArc( nodes.get( 0 ), nodes.get( 1 ) ) == true )
    {
      // remember the split arc
      final INodeResult addedNode = insertNode( nodes.get( 0 ), nodes.get( 1 ) );
      if( addedNode != null )
      {
        splitArcs.add( 0 );
        nodes.add( addedNode );
      }
    }

    if( NodeResultHelper.checkTriangleArc( nodes.get( 1 ), nodes.get( 2 ) ) == true )
    {
      // remember the split arc
      final INodeResult addedNode = insertNode( nodes.get( 1 ), nodes.get( 2 ) );
      if( addedNode != null )
      {
        splitArcs.add( 1 );
        nodes.add( addedNode );
      }
    }
    if( NodeResultHelper.checkTriangleArc( nodes.get( 2 ), nodes.get( 0 ) ) == true )
    {
      // remember the split arc
      final INodeResult addedNode = insertNode( nodes.get( 2 ), nodes.get( 0 ) );
      if( addedNode != null )
      {
        nodes.add( addedNode );
        splitArcs.add( 2 );
      }
    }

    switch( splitArcs.size() )
    {
      /* case1: one arc is split */
      case 1:
        splitIntoTwoTriangles( nodes, splitArcs.get( 0 ) );
        break;

      /* case2: two arcs were split */
      case 2:
        splitIntoThreeTriangles( nodes, splitArcs.get( 0 ), splitArcs.get( 1 ) );
        break;

      default:
        break;
    }
  }

  private void splitIntoThreeTriangles( final List<INodeResult> nodes, final Integer arc1, final Integer arc2 )
  {
    if( arc1 == 0 && arc2 == 1 )
    {
      /*
       * the first inserted node lies between node0 and node1 the second between node1 and node2
       */

      /* check, if the corner node of the split arc is dry in order to save just the wet triangle */
      if( nodes.get( 1 ).isWet() == true )
      {
        List<INodeResult> triNodes = new LinkedList<INodeResult>();

        /* triangle: inserted node0, inserted node1, node1 */
        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 1 ) );
        triNodes.add( nodes.get( 4 ) );

        if( checkPartiallyFlooded( triNodes ) == true )
          m_triangleEater.add( triNodes, true );

        triNodes.clear();

        /* triangle: inserted node1, node2, inserted node0 */
        triNodes.add( nodes.get( 0 ) );
        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 2 ) );

        m_triangleEater.add( triNodes, false );

        triNodes.clear();

        triNodes = new LinkedList<INodeResult>();

        /* triangle: node0, inserted node0, node2 */
        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 4 ) );
        triNodes.add( nodes.get( 2 ) );

        m_triangleEater.add( triNodes, false );

        triNodes.clear();

      }
      else if( nodes.get( 0 ).isWet() == true && nodes.get( 2 ).isWet() == true )
      {
        List<INodeResult> triNodes = new LinkedList<INodeResult>();

        /* triangle: inserted node1, node2, inserted node0 */
        triNodes.add( nodes.get( 0 ) );
        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 2 ) );

        m_triangleEater.add( triNodes, true );

        triNodes.clear();
        triNodes = new LinkedList<INodeResult>();

        /* triangle: node0, inserted node0, node2 */
        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 4 ) );
        triNodes.add( nodes.get( 2 ) );

        m_triangleEater.add( triNodes, true );

        triNodes.clear();
        triNodes = new LinkedList<INodeResult>();

        /* triangle: inserted node0, inserted node1, node1 */
        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 1 ) );
        triNodes.add( nodes.get( 4 ) );

        m_triangleEater.add( triNodes, false );
        triNodes.clear();
      }
    }
    else if( arc1 == 0 && arc2 == 2 )
    {
      /*
       * the first inserted node lies between node0 and node1 the second between node2 and node0
       */

      /* check, if the corner node of the split arc is dry in order to save just the wet triangle */
      if( nodes.get( 0 ).isWet() == true )
      {
        List<INodeResult> triNodes = new LinkedList<INodeResult>();

        /* triangle: inserted node0, inserted node1, node0 */
        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 4 ) );
        triNodes.add( nodes.get( 0 ) );

        m_triangleEater.add( triNodes, true );
        triNodes.clear();

        triNodes = new LinkedList<INodeResult>();

        /* triangle: inserted node0, node1, inserted node1 */
        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 1 ) );
        triNodes.add( nodes.get( 4 ) );

        m_triangleEater.add( triNodes, false );
        triNodes.clear();

        triNodes = new LinkedList<INodeResult>();

        /* triangle: node2, inserted node1, node1 */
        triNodes.add( nodes.get( 2 ) );
        triNodes.add( nodes.get( 4 ) );
        triNodes.add( nodes.get( 1 ) );

        m_triangleEater.add( triNodes, false );
        triNodes.clear();
      }
      else if( nodes.get( 1 ).isWet() == true && nodes.get( 2 ).isWet() == true )
      {
        List<INodeResult> triNodes = new LinkedList<INodeResult>();

        /* triangle: inserted node0, node1, inserted node1 */
        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 1 ) );
        triNodes.add( nodes.get( 4 ) );

        m_triangleEater.add( triNodes, true );
        triNodes.clear();
        triNodes = new LinkedList<INodeResult>();

        /* triangle: node2, inserted node1, node1 */
        triNodes.add( nodes.get( 2 ) );
        triNodes.add( nodes.get( 4 ) );
        triNodes.add( nodes.get( 1 ) );

        m_triangleEater.add( triNodes, true );
        triNodes.clear();
        triNodes = new LinkedList<INodeResult>();

        /* triangle: inserted node0, inserted node1, node0 */
        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 4 ) );
        triNodes.add( nodes.get( 0 ) );

        m_triangleEater.add( triNodes, false );
        triNodes.clear();
      }
    }
    else if( arc1 == 1 && arc2 == 2 )
    {
      /*
       * the first inserted node lies between node1 and node2 the second between node2 and node0
       */

      /* check, if the corner node of the split arc is dry in order to save just the wet triangle */
      if( nodes.get( 2 ).isWet() == true )
      {
        List<INodeResult> triNodes = new LinkedList<INodeResult>();

        /* triangle0: inserted node0, node2, inserted node1 */
        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 2 ) );
        triNodes.add( nodes.get( 4 ) );

        m_triangleEater.add( triNodes, true );
        triNodes.clear();

        triNodes = new LinkedList<INodeResult>();

        /* triangle: inserted node1, node1, inserted node0 */
        triNodes.add( nodes.get( 4 ) );
        triNodes.add( nodes.get( 1 ) );
        triNodes.add( nodes.get( 3 ) );

        m_triangleEater.add( triNodes, false );
        triNodes.clear();

        triNodes = new LinkedList<INodeResult>();

        /* triangle: node0, node1, inserted node2 */
        triNodes.add( nodes.get( 0 ) );
        triNodes.add( nodes.get( 1 ) );
        triNodes.add( nodes.get( 4 ) );

        m_triangleEater.add( triNodes, false );
        triNodes.clear();
      }
      else if( nodes.get( 0 ).isWet() == true && nodes.get( 1 ).isWet() == true )
      {
        List<INodeResult> triNodes = new LinkedList<INodeResult>();

        /* triangle: inserted node1, node1, inserted node0 */
        triNodes.add( nodes.get( 4 ) );
        triNodes.add( nodes.get( 1 ) );
        triNodes.add( nodes.get( 3 ) );

        m_triangleEater.add( triNodes, true );
        triNodes.clear();

        triNodes = new LinkedList<INodeResult>();

        /* triangle: node0, node1, inserted node2 */
        triNodes.add( nodes.get( 0 ) );
        triNodes.add( nodes.get( 1 ) );
        triNodes.add( nodes.get( 4 ) );

        m_triangleEater.add( triNodes, true );
        triNodes.clear();

        triNodes = new LinkedList<INodeResult>();
        /* triangle0: inserted node0, node2, inserted node1 */
        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 2 ) );
        triNodes.add( nodes.get( 4 ) );

        m_triangleEater.add( triNodes, false );
        triNodes.clear();
      }
      else
      {
        KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", "Fehler in splitIntoThreeTriangles: Dreieck wurde nicht gesplittet" );
      }
    }
  }

  /**
   * One corner node is part of the inundation line and one arc is split => two triangles.<br>
   * There are three possibilities depending on the split arc.<br> - the inserted node lies between node0 and node1.<br> -
   * the inserted node lies between node1 and node2.<br> - the inserted node lies between node2 and node0.<br>
   * 
   * @param nodes
   *            list of all nodes of the triangle to split
   * @param splitArcs
   *            list of the split arc numbers (here just one)
   * 
   */
  private void splitIntoTwoTriangles( final List<INodeResult> nodes, final Integer splitArc )
  {

    if( splitArc == 0 )
    {
      /*
       * the inserted node lies between node0 and node1.
       */

      /* triangle1: inserted node0, node1, node2 */

      /* check, if the corner node of the split arc is dry in order to save just the wet triangle */
      if( nodes.get( 1 ).isWet() == true )
      {
        final List<INodeResult> triNodes = new LinkedList<INodeResult>();

        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 1 ) );
        triNodes.add( nodes.get( 2 ) );

        m_triangleEater.add( triNodes, true );

        triNodes.clear();

        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 2 ) );
        triNodes.add( nodes.get( 0 ) );

        m_triangleEater.add( triNodes, false );

      }

      /* triangle2: inserted node0, node2, node0 */

      /* check, if the corner node of the split arc is dry in order to save just the wet triangle */
      else if( nodes.get( 0 ).isWet() == true )
      {
        final List<INodeResult> triNodes = new LinkedList<INodeResult>();

        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 2 ) );
        triNodes.add( nodes.get( 0 ) );

        triNodes.clear();

        m_triangleEater.add( triNodes, true );

        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 1 ) );
        triNodes.add( nodes.get( 2 ) );

        m_triangleEater.add( triNodes, false );

      }

    }
    else if( splitArc == 1 )
    {
      /*
       * the inserted node lies between node0 and node1.
       */

      /* triangle1: node0, node1, inserted node0 */

      /* check, if the corner node of the split arc is dry in order to save just the wet triangle */
      if( nodes.get( 1 ).isWet() == true )
      {
        final List<INodeResult> triNodes = new LinkedList<INodeResult>();

        triNodes.add( nodes.get( 0 ) );
        triNodes.add( nodes.get( 1 ) );
        triNodes.add( nodes.get( 3 ) );

        m_triangleEater.add( triNodes, true );

        triNodes.clear();

        triNodes.add( nodes.get( 0 ) );
        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 2 ) );

        m_triangleEater.add( triNodes, false );

      }

      /* triangle2: node0, inserted node0, node2 */

      /* check, if the corner node of the split arc is dry in order to save just the wet triangle */
      else if( nodes.get( 2 ).isWet() == true )
      {
        final List<INodeResult> triNodes = new LinkedList<INodeResult>();

        triNodes.add( nodes.get( 0 ) );
        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 2 ) );

        m_triangleEater.add( triNodes, true );

        triNodes.clear();

        triNodes.add( nodes.get( 0 ) );
        triNodes.add( nodes.get( 1 ) );
        triNodes.add( nodes.get( 3 ) );

        m_triangleEater.add( triNodes, false );

      }
    }
    else if( splitArc == 2 )
    {
      /*
       * the inserted node lies between node0 and node1.
       */

      /* triangle1: inserted node0, node0, node1 */

      /* check, if the corner node of the split arc is dry in order to save just the wet triangle */
      if( nodes.get( 0 ).isWet() == true )
      {
        final List<INodeResult> triNodes = new LinkedList<INodeResult>();

        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 0 ) );
        triNodes.add( nodes.get( 1 ) );

        m_triangleEater.add( triNodes, true );

        triNodes.clear();

        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 1 ) );
        triNodes.add( nodes.get( 2 ) );

        m_triangleEater.add( triNodes, false );

      }

      /* triangle2: inserted node0, node1, node2 */

      /* check, if the corner node of the split arc is dry in order to save just the wet triangle */
      else if( nodes.get( 2 ).isWet() == true )
      {
        final List<INodeResult> triNodes = new LinkedList<INodeResult>();

        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 1 ) );
        triNodes.add( nodes.get( 2 ) );

        m_triangleEater.add( triNodes, true );

        triNodes.clear();

        triNodes.add( nodes.get( 3 ) );
        triNodes.add( nodes.get( 0 ) );
        triNodes.add( nodes.get( 1 ) );

        m_triangleEater.add( triNodes, false );

      }
    }
    else
    {
      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", "Fehler in splitIntoTwoTriangles: Dreieck wurd nicht gesplittet" );
    }
  }

  private INodeResult insertNode( final INodeResult node1, final INodeResult node2 )
  {
    final INodeResult node3 = new SimpleNodeResult();

    final double wspNode1 = node1.getWaterlevel();
    final double zNode1 = node1.getPoint().getZ();
    final double wspNode2 = node2.getWaterlevel();
    final double zNode2 = node2.getPoint().getZ();

    final double dist12 = node2.getPoint().distance( node1.getPoint() );
    final double dz1;
    final double dz2;

    if( (wspNode1 - zNode1) > 0 ) // the same as node1.isWet() == true;
    {
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
    final double dist13 = NodeResultHelper.getZeroPoint( dist12, dz1, dz2 );

    // getting the z-value for the zero point
    final double z3;

    if( node1.isWet() == true )
      z3 = node1.getWaterlevel();
    else
      z3 = node2.getWaterlevel();

    // getting the geo-coordinates of the zero-point
    final double dx12 = node2.getPoint().getX() - node1.getPoint().getX();
    final double dy12 = node2.getPoint().getY() - node1.getPoint().getY();

    final double x3 = node1.getPoint().getX() + (dist13 / dist12) * dx12;
    final double y3 = node1.getPoint().getY() + (dist13 / dist12) * dy12;

    final String crs = node1.getPoint().getCoordinateSystem();

    node3.setLocation( x3, y3, z3, crs );
    // node3.setResultValues( 0, 0, 0, z3 );

    final List<Double> velocity = new LinkedList<Double>();
    velocity.add( 0.0 );
    velocity.add( 0.0 );
    node3.setVelocity( velocity );
    node3.setDepth( 0.0 );
    node3.setWaterlevel( z3 );

    return node3;

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleNode(java.lang.String, int, double, double,
   *      double)
   */
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
      result.setName( "" + id );

      // TODO: description: beschreibt, welche Rechenvariante und so weiter... oder noch besser an der collection
      // result.setDescription( "" + id );

      result.setCalcId( id );
      result.setLocation( easting, northing, elevation, m_crs );

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
  public void handleError( final String lineString, final EReadError errorHints )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handlerUnIdentifyable(java.lang.String)
   */
  public void handlerUnIdentifyable( final String lineString )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#setIRoughnessIDProvider(org.kalypso.kalypsomodel1d2d.conv.IRoughnessIDProvider)
   */
  public void setIRoughnessIDProvider( final IRoughnessIDProvider roughnessIDProvider ) throws IllegalArgumentException
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#start()
   */
  public void start( )
  {
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleResult(java.lang.String, int, double,
   *      double, double, double)
   */
  public void handleResult( final String lineString, final int id, final double vx, final double vy, final double virtualDepth, final double waterlevel )
  {
    final INodeResult result = m_nodeIndex.get( id );
    if( result == null )
    {
      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s %d ", "Result for non-existing node: ", id );
      return;
    }

    // check if node is dry
    result.setResultValues( vx, vy, virtualDepth, waterlevel );
    m_resultMinMaxCatcher.addNodeResult( result );

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleTime(java.lang.String, java.util.Date)
   */
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
  @SuppressWarnings("unchecked")
  public void handleJunction( final String parseLine, final int junctionID, final int element1dID, final int boundaryLine2dID, final int node1dID )
  {
    /* get 1d node */
    // get node result for 1d node
    final Map<Object, Feature> indexedFeatures = FeatureIndex.indexFeature( m_resultList, GMLNodeResult.QNAME_PROP_CALCID );

    final Feature feature = indexedFeatures.get( node1dID );
    // TODO this should work, but it does not! Implement the adapter in the feature-adapter-factory
    // final INodeResult nodeResult1d = (INodeResult) feature.getAdapter( INodeResult.class );
    final INodeResult nodeResult1d = new GMLNodeResult( feature );
    final double waterlevel = nodeResult1d.getWaterlevel(); // we know, that the water level at the boundary nodes is
    // unique for all nodes

    try
    {
      final IFlowRelation1D teschkeRelation = getFlowRelation( nodeResult1d );
      final GM_Curve nodeCurve1d = NodeResultHelper.getProfileCurveFor1dNode( teschkeRelation.getProfile() );

      final List<IFELine> continuityLine2Ds = m_controlModel.getCalculationUnit().getContinuityLines();
      IContinuityLine2D selectedBoundaryLine = null;
      for( final IFELine continuityLine2D : continuityLine2Ds )
      {
        if( continuityLine2D.getGmlID().equals( Integer.toString( boundaryLine2dID ) ) )
        {
          selectedBoundaryLine = (IContinuityLine2D) continuityLine2D;
          break;
        }
      }
      final GM_Point[] linePoints = NodeResultHelper.getLinePoints( selectedBoundaryLine );

      // here, we just use the corner nodes of the mesh. mid-side nodes are not used.
      // get the node results of that points and add it to the Featurelist
      final FeatureList resultList = new SplitSort( null, null );
      for( final GM_Point point : linePoints )
      {
        final INodeResult nodeResult2d = NodeResultHelper.getNodeResult( point, m_resultList, 0.5 );
        if( nodeResult2d != null )
        {
          resultList.add( nodeResult2d );
        }
      }

      // just get the area between inundation line
      final GM_Curve boundaryCurve = NodeResultHelper.getCurveForBoundaryLine( linePoints, waterlevel, m_crs );

      final double curveDistance = nodeCurve1d.distance( boundaryCurve );

      /* now we have two curves and use Triangle in order to get the triangles */
      createJunctionTriangles( nodeResult1d, resultList, nodeCurve1d, boundaryCurve, curveDistance );
    }
    catch( final GM_Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( final Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    // triangulate the line with the 1d profile

    // feed the triangle eater

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleNodeInformation(java.lang.String, int, int,
   *      double, double, double, double)
   */
  public void handleNodeInformation( final String line, final int id, final int dry, final double value1, final double value2, final double value3, final double value4 )
  {
    final INodeResult result = m_nodeIndex.get( id );
    if( result == null )
    {
      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s %d ", "Result for non-existing node: ", id );
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

        final IFlowRelation1D teschkeRelation = getFlowRelation( nodeResult );

        if( teschkeRelation == null )
          break;

        final WspmProfile profile = teschkeRelation.getProfile();

        // get the profile Curves of the two nodes defining the current element
        final GM_Curve curve = NodeResultHelper.getProfileCurveFor1dNode( profile );
        // TODO: maybe cut all curves at possible intersection points(?)

        profileCurveList.add( curve );
        nodeList.add( nodeResult );
      }

      /* now we have all curves and use Triangle in order to get the triangles */

      if( nodeList.size() > 1 )
        create1dJunctionTriangles( nodeList, profileCurveList );
    }
    catch( final Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

  }
}
