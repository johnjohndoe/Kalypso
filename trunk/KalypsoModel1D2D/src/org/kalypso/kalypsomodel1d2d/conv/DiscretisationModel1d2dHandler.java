/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.kalypsomodel1d2d.conv;

import gnu.trove.THashSet;
import gnu.trove.TIntObjectHashMap;
import gnu.trove.TIntObjectProcedure;
import gnu.trove.TObjectProcedure;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv.RESULTLINES;
import org.kalypso.kalypsomodel1d2d.conv.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.FlowRelationUtilitites;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IFlowRelation1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IWeirFlowRelation;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeletePolyElementCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.element1d.Create2dElementCommand;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.schema.gml.QIntervallResult;
import org.kalypso.model.wspm.tuhh.schema.gml.QIntervallResultCollection;
import org.kalypso.model.wspm.tuhh.schema.schemata.IWspmTuhhQIntervallConstants;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.gml.binding.math.IPolynomial1D;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * The handler that converts the RMA-Kalypso element events into discretisation model elements and links
 * 
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 * @author Patrice Congo
 * @author ilya
 */
public class DiscretisationModel1d2dHandler implements IRMA10SModelElementHandler
{
  private final IStatusCollector m_stati = new StatusCollector( KalypsoModel1D2DPlugin.PLUGIN_ID );

  /**
   * The model to fill with the parsed fe element from
   */
  protected final IFEDiscretisationModel1d2d m_model;

  private final GMLWorkspace m_workspace;

  private final IPositionProvider m_positionProvider;

  private final TIntObjectHashMap<IFE1D2DNode> m_nodesMap = new TIntObjectHashMap<>();

  private final TIntObjectHashMap<IFE1D2DEdge> m_edgesNameConversionMap = new TIntObjectHashMap<>();

  private final TIntObjectHashMap<List<IFE1D2DEdge>> m_elementEdges = new TIntObjectHashMap<>();

  private final Map<Integer, SortedMap<Integer, Integer>> m_mapIdBuildingType = new HashMap<>();

  private final Map<Integer, Integer> m_mapIdBuildingDirection = new HashMap<>();

  private final Set<String> m_dirtyModels = new HashSet<>();

  private Map<Integer, QIntervallResult> m_mapQResults = null;

  private Map<String, IPolynomial1D> m_map1dPolynomial = null;

  private GMLWorkspace m_qResWorkspace;

  private QIntervallResultCollection m_qresultCollection;

  private Set<Integer> m_set1dFlowNodes = null;

  private char m_charDomain;

  private List<Double> m_listPolyRanges;

  private final IFlowRelationshipModel m_flowModel;

  private final GMLWorkspace m_flowWorkspace;

  private final List<Feature> m_listNewFlowElements = new ArrayList<>();

  private final RoughnessHandler m_roughnessHandler;

  private boolean m_importRoughness;

  private final String m_crs;

  private final IScenarioDataProvider m_scenarioDataProvider;

  public DiscretisationModel1d2dHandler( final IScenarioDataProvider scenarioDataProvider, final IPositionProvider positionProvider ) throws CoreException
  {
    m_scenarioDataProvider = scenarioDataProvider;

    m_model = scenarioDataProvider.getModel( IFEDiscretisationModel1d2d.class.getName() );
    m_workspace = m_model.getWorkspace();

    m_flowModel = scenarioDataProvider.getModel( IFlowRelationshipModel.class.getName() );
    m_flowWorkspace = m_flowModel.getWorkspace();

    m_positionProvider = positionProvider;
    m_crs = m_positionProvider.getCoordinateSystem();

    final IRoughnessClsCollection roughnessModel = scenarioDataProvider.getModel( IRoughnessClsCollection.class.getName() );
    m_roughnessHandler = new RoughnessHandler( roughnessModel );
  }

  @Override
  public void start( )
  {
  }

  @Override
  public void end( )
  {
    final int lIntCountCreated = createFlowRels1d();

    final Feature[] lAllElementsFlow = m_flowModel.getFlowRelationsShips().getFeatureList().toFeatures();
    if( lIntCountCreated > 0 && lAllElementsFlow.length > 0 )
    {
      m_flowWorkspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_flowWorkspace, m_flowModel, lAllElementsFlow, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );//
      m_dirtyModels.add( IFlowRelationshipModel.class.getName() );
    }

    final TIntObjectHashMap<IPolyElement> elementsNameConversionMap = new TIntObjectHashMap<>();
    m_elementEdges.forEachEntry( new TIntObjectProcedure<List<IFE1D2DEdge>>()
    {

      @Override
      public boolean execute( final int rmaID, final List<IFE1D2DEdge> edgeList )
      {
        final IPolyElement element = m_model.createElement2D( edgeList.toArray( new IFE1D2DEdge[edgeList.size()] ) );
        elementsNameConversionMap.put( rmaID, element );
        return true;
      }
    } );

    // create weirs
    final Set<Integer> lSetKeys = m_mapIdBuildingType.keySet();
    for( final Integer id : lSetKeys )
    {
      final SortedMap<Integer, Integer> lMapElements = m_mapIdBuildingType.get( id );
      createWeirs( elementsNameConversionMap, lMapElements.values(), m_mapIdBuildingDirection.get( id ) );
    }

    // remove all dangling nodes
    // e.g. middle nodes
    final THashSet<IFE1D2DNode> removeNodes = new THashSet<>( m_edgesNameConversionMap.size() );
    m_nodesMap.forEachValue( new TObjectProcedure<IFE1D2DNode>()
    {

      @Override
      public boolean execute( final IFE1D2DNode node )
      {
        if( node.getLinkedEdges().length == 0 )
          removeNodes.add( node );
        return true;
      }
    } );
    m_model.removeAllNodes( removeNodes );

    // invalidate full extent
    m_workspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_workspace, m_model, new Feature[0], FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
    m_dirtyModels.add( IFEDiscretisationModel1d2d.class.getName() );

    if( m_listNewFlowElements.size() > 0 )
    {
      // invalidate full extent
      m_flowWorkspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_flowWorkspace, m_flowModel, new Feature[0], FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
      m_dirtyModels.add( IFlowRelationshipModel.class.getName() );
    }

    if( m_roughnessHandler.changeModel() )
      m_dirtyModels.add( IRoughnessClsCollection.class.getName() );

    m_stati.add( m_roughnessHandler.getStatus() );
  }

  /**
   * merges all poly elements of the same building type( weir id > 900 ) into one poly element and creates a new weir
   * flow relationship on this element.
   * after creating of weir flow relationship on new poly element the flow direction is reconstructed from 2d file, the
   * observation is not reconstructed, only initialized
   * lNodePrev lNodeAct +--------->+--------->+ |lPolyPrev | lPoly | | | | | | | | lCommonEdge | | | | | | |
   * +<---------+<---------+ lNodeBckPrev lNodeBckAct
   */
  private IPolyElement createWeirs( final TIntObjectHashMap<IPolyElement> elementsNameConversionMap, final Collection<Integer> pListElementsIdsRma, final int pIntDegrees )
  {
    final DeletePolyElementCmd deleteCommand = new DeletePolyElementCmd( m_model );

    // calculate weir polygon
    GM_Polygon weirGeom = null;
    for( final Integer lIntRMAId : pListElementsIdsRma )
    {
      final IPolyElement polyElement = elementsNameConversionMap.get( lIntRMAId );
      deleteCommand.addElementToRemove( polyElement );
      final GM_Polygon geometry = polyElement.getGeometry();
      if( weirGeom == null )
        weirGeom = geometry;
      else
        weirGeom = (GM_Polygon)weirGeom.union( geometry );
    }

    try
    {
      IPolyElement weirElement = null;
      if( pListElementsIdsRma.size() > 1 )
      {
        // cleanup and update
        if( pListElementsIdsRma.size() > 1 )
        {
          m_scenarioDataProvider.postCommand( IFEDiscretisationModel1d2d.class.getName(), deleteCommand );
        }

        // create weir element
        final GM_Position[] exteriorRing = weirGeom.getSurfacePatch().getExteriorRing();
        final GM_Point[] points = new GM_Point[exteriorRing.length];
        for( int i = 0; i < exteriorRing.length; i++ )
          points[i] = GeometryFactory.createGM_Point( exteriorRing[i], weirGeom.getCoordinateSystem() );

        final Create2dElementCommand command = new Create2dElementCommand( m_model, points );
        m_scenarioDataProvider.postCommand( IFlowRelationshipModel.class.getName(), command );
        weirElement = command.getNewElement();
      }
      else
      {
        final Integer onlyRmaID = pListElementsIdsRma.iterator().next();
        weirElement = elementsNameConversionMap.get( onlyRmaID );
      }

      // add flow relationship
      final GM_Position flowPositionFromElement = FlowRelationUtilitites.getFlowPositionFromElement( weirElement );
      final FeatureList featureList = m_flowModel.getFlowRelationsShips().getFeatureList();
      final IWeirFlowRelation weirRelation = (IWeirFlowRelation)FeatureHelper.createFeatureForListProp( featureList, IWeirFlowRelation.QNAME, -1 );

      /* Call getObservation once to initialize it */
      weirRelation.getBuildingObservation();
      weirRelation.setDirection( pIntDegrees );
      weirRelation.setPosition( GeometryFactory.createGM_Point( flowPositionFromElement, weirElement.getGeometry().getCoordinateSystem() ) );

      return weirElement;
    }
    catch( final Exception e )
    {
      throw new IllegalStateException( e );
    }
  }

  /**
   * creates 1d flow parameters that was found on 2d model
   * this function implements the reconstruction of 1d parameters on the according 1d elements, profile data are not
   * handled in 2d file, so only the polynomials are created and the flow relationships are not connected to any
   * profiles
   */
  private int createFlowRels1d( )
  {
    if( m_set1dFlowNodes == null || m_mapQResults == null )
      return 0;

    int lIntCountNew = 0;
    final SortedMap<BigDecimal, IProfileFeature> profilesByStation = new TreeMap<>();
    for( final Object element : m_set1dFlowNodes )
    {
      final Integer lId = (Integer)element;
      final IFE1D2DNode lActNode = getNode( lId );
      final QIntervallResult lQResult = m_mapQResults.get( lId );
      try
      {
        final IFlowRelation1D flowRel = FlowRelationUtilitites.addTeschke( m_flowModel, lActNode, lQResult, profilesByStation );
        flowRel.setName( lQResult.getName() );
        flowRel.setDescription( lQResult.getDescription() );
        lIntCountNew++;
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }
    return lIntCountNew;
  }

  @Override
  public void handleArc( final String lineString, final int id, final int node1ID, final int node2ID, final int elementLeftID, final int elementRightID, final int middleNodeID )
  {
    final IFE1D2DNode node1 = getNode( node1ID );
    if( node1 == null )
      throw new RuntimeException( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.DiscretisationModel1d2dHandler.0", node1ID, id ) ); //$NON-NLS-1$

    final IFE1D2DNode node2 = getNode( node2ID );
    if( node2 == null )
      throw new RuntimeException( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.DiscretisationModel1d2dHandler.1", node2ID, id ) ); //$NON-NLS-1$

    /* Create edge for the two existing nodes */
    final IFE1D2DEdge edge = getOrCreateEdge( id, node1, node2 );

    if( elementLeftID == elementRightID )
      m_model.createElement1D( edge );
    else
    {
      // 2d
      if( elementLeftID != 0 )
        getOrCreateEdgeList( elementLeftID ).add( edge );
      if( elementRightID != 0 )
        getOrCreateEdgeList( elementRightID ).add( edge );
    }
  }

  private List<IFE1D2DEdge> getOrCreateEdgeList( final int elementID )
  {
    List<IFE1D2DEdge> list = m_elementEdges.get( elementID );
    if( list == null )
    {
      list = new ArrayList<>( 4 );
      m_elementEdges.put( elementID, list );
    }
    return list;
  }

  private IFE1D2DEdge getOrCreateEdge( final int rmaID, final IFE1D2DNode node1, final IFE1D2DNode node2 )
  {
    IFE1D2DEdge edge = m_edgesNameConversionMap.get( rmaID );
    if( edge == null )
    {
      edge = m_model.createEdge( node1, node2 );
      m_edgesNameConversionMap.put( rmaID, edge );
    }
    return edge;
  }

  private final IFE1D2DNode getNode( final int rmaID )
  {
    return m_nodesMap.get( rmaID );
  }

  @Override
  public void handleElement( final String lineString, final int id, final int currentRoughnessClassID, final int previousRoughnessClassID, final int eleminationNumber )
  {
    if( currentRoughnessClassID > 900 )
    {
      try
      {
        SortedMap<Integer, Integer> lMapElements = m_mapIdBuildingType.get( currentRoughnessClassID );
        if( lMapElements == null )
        {
          lMapElements = new TreeMap<>();
          m_mapIdBuildingType.put( currentRoughnessClassID, lMapElements );
          m_mapIdBuildingDirection.put( currentRoughnessClassID, eleminationNumber );
        }

        // to save the order of elements with weir according to its original output order
        lMapElements.put( previousRoughnessClassID, id );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }
  }

  @Override
  public void handleNode( final String lineString, final int id, final double xCoord, final double yCoord, final double elevation )
  {
    final IFE1D2DNode nodeWithSameId = getNode( id );
    if( nodeWithSameId != null )
      throw new IllegalArgumentException( String.format( "Found multiple nodes with the same id: %s", id ) ); //$NON-NLS-1$

    final GM_Point nodeLocation = m_positionProvider.getGMPoint( xCoord, yCoord, elevation );
    nodeLocation.setCoordinateSystem( m_crs );

    // new node, create or get
    final IFE1D2DNode node = m_model.createNode( nodeLocation );
    m_nodesMap.put( id, node );
  }

  @Override
  public void handleError( final String lineString, final EReadError errorHints )
  {
    // FIXME what is this?
    throw new RuntimeException( "bad line=" + lineString ); //$NON-NLS-1$
  }

  @Override
  public void handleResult( final String lineString, final int id, final double vx, final double vy, final double depth, final double waterlevel )
  {
    // do nothing, because here just the model is being read.
  }

  @Override
  public void handleTime( final String line, final Date time )
  {
    // TODO: maybe set description, ...?
  }

  @Override
  public void handleJunction( final String line, final int junctionID, final int element1dID, final int boundaryLine2dID, final int node1dID )
  {
  }

  @Override
  public void handleFlowResistance( final String line, final int id, final double combinedLambda, final double soilLambda, final double vegetationLambda )
  {
  }

  @Override
  public void handleNodeInformation( final String line, final int id, final int dry, final double value1, final double value2, final double value3, final double value4 )
  {
  }

  @Override
  public void handle1dJunctionInformation( final String line, final int junctionId, final List<Integer> junctionNodeIDList )
  {
  }

  @Override
  public void handleTimeDependentAdditionalResult( final String lineString, final int id, final double vx, final double vy, final double depth, final RESULTLINES resultlines )
  {
  }

  @Override
  public void handle1dPolynomialRangesInformation( final String line, final String pStrPolyKind, final int pIntNodeId, final int pIntAmountRanges, final List<Double> pListPolyAreaMaxRanges )
  {
    if( m_mapQResults == null )
    {
      initMapsPolynoms();
    }
    m_charDomain = getDomainChar( pStrPolyKind );
    m_listPolyRanges = pListPolyAreaMaxRanges;
  }

  private char getDomainChar( final String pStrPolyKind )
  {
    if( pStrPolyKind.equalsIgnoreCase( "PRA" ) ) //$NON-NLS-1$
    {
      return 'A';
    }
    else if( pStrPolyKind.equalsIgnoreCase( "PRQ" ) ) //$NON-NLS-1$
    {
      return 'Q';
    }
    else if( pStrPolyKind.equalsIgnoreCase( "PRB" ) ) //$NON-NLS-1$
    {
      return 'a';
    }
    return 0;
  }

  @Override
  public void handle1dPolynomeMinMax( final String line, final int id, final double min, final double max )
  {
    if( m_mapQResults == null )
    {
      initMapsPolynoms();
    }
    // seems to be recalculated each time for writing data for RMA, TODO: implement as member in teschke relation
    // QIntervallResult qresult = getResult( id );
  }

  private QIntervallResult getOrCreateQResult( final int id )
  {
    if( m_mapQResults == null )
    {
      initMapsPolynoms();
    }
    try
    {
      final QIntervallResult qresultFound = m_mapQResults.get( id );
      if( qresultFound != null )
      {
        return qresultFound;
      }
      final QIntervallResult qresult = m_qresultCollection.getQIntervalls().addNew( QIntervallResult.QNAME_F_QIntervallResult );
      m_mapQResults.put( id, qresult );

      return qresult;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    return null;
  }

  private IPolynomial1D getPolynomial( final int id, final int pIntActRangeNr, final char kind )
  {
    try
    {
      final IPolynomial1D polynomFound = m_map1dPolynomial.get( "" + kind + id + pIntActRangeNr ); //$NON-NLS-1$
      if( polynomFound != null )
      {
        return polynomFound;
      }
      final IPolynomial1D polynom = getOrCreateQResult( id ).createPolynomial();
      polynom.setDomainPhenomenon( getDomainId( kind ) );
      polynom.setRangePhenomenon( IWspmTuhhQIntervallConstants.DICT_PHENOMENON_WATERLEVEL );
      m_map1dPolynomial.put( "" + kind + id + pIntActRangeNr, polynom ); //$NON-NLS-1$
      return polynom;

    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    return null;
  }

  private String getDomainId( final char kind )
  {
    String domainId = ""; //$NON-NLS-1$
    switch( kind )
    {
      case 'Q':
        domainId = IWspmTuhhQIntervallConstants.DICT_PHENOMENON_RUNOFF;
        break;
      case 'A':
        domainId = IWspmTuhhQIntervallConstants.DICT_PHENOMENON_AREA;
        break;
      case 'a':
        domainId = IWspmTuhhQIntervallConstants.DICT_PHENOMENON_ALPHA;
        break;

      default:
        return null;
    }
    return domainId;
  }

  private void initMapsPolynoms( )
  {
    try
    {
      m_qResWorkspace = FeatureFactory.createGMLWorkspace( QIntervallResultCollection.QNAME_F_QIntervallResultCollection, null, GmlSerializer.DEFAULT_FACTORY );
      m_qresultCollection = (QIntervallResultCollection)m_qResWorkspace.getRootFeature();
      m_mapQResults = new HashMap<>();
      m_map1dPolynomial = new HashMap<>();
      m_set1dFlowNodes = new HashSet<>();
    }
    catch( final GMLSchemaException e )
    {
      e.printStackTrace();
      // FIXME: we cannot continue if this happens!
    }
  }

  @Override
  public void handle1dSplittedPolynomialsInformation( final String line, final String pStrPolyKind, final int pIntNodeId, final int pIntActRangeNr, final List<Double> pListPolyCoeffs, final Double pDoubleSlope )
  {
    if( m_mapQResults == null )
    {
      initMapsPolynoms();
    }
    final IPolynomial1D poly1d = getPolynomial( pIntNodeId, pIntActRangeNr, m_charDomain );

    final String description = pStrPolyKind + pIntActRangeNr;
    poly1d.setName( description );
    poly1d.setDescription( description );
    poly1d.setCoefficients( ArrayUtils.toPrimitive( pListPolyCoeffs.toArray( new Double[pListPolyCoeffs.size()] ) ) );
    poly1d.setRange( m_listPolyRanges.get( pIntActRangeNr - 1 ), m_listPolyRanges.get( pIntActRangeNr ) );
    m_set1dFlowNodes.add( pIntNodeId );
    if( pDoubleSlope != null )
    {
      final QIntervallResult lQResult = getOrCreateQResult( pIntNodeId );
      lQResult.setSlope( new BigDecimal( pDoubleSlope ) );
    }
  }

  @Override
  public void handleNode( final String line, final int id, final double easting, final double northing, final double elevation, final double stationName )
  {
    handleNode( line, id, easting, northing, elevation );

    final QIntervallResult result = getOrCreateQResult( id );
    result.setName( "" + stationName ); //$NON-NLS-1$
    result.setDescription( String.format( Messages.getString( "DiscretisationModel1d2dHandler.3" ), stationName ) ); //$NON-NLS-1$
    result.setStation( new BigDecimal( stationName ) );
  }

  public String[] getDirtyModels( )
  {
    return m_dirtyModels.toArray( new String[m_dirtyModels.size()] );
  }

  @Override
  public void handleRoughness( final String id, final String label )
  {
    if( m_importRoughness )
      m_roughnessHandler.addRoughness( id, label );
  }

  public void setImportRoughness( final boolean importRoughness )
  {
    m_importRoughness = importRoughness;
  }

  public IStatus getStatus( )
  {
//    return m_stati.asMultiStatusOrOK( "2D-Import", "Import succesfully finished." );
    return m_stati.asMultiStatusOrOK( Messages.getString( "DiscretisationModel1d2dHandler.1" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.DiscretisationModel1d2dHandler.5" ) ); //$NON-NLS-1$//$NON-NLS-2$
  }
}