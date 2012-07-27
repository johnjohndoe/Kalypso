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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv.RESULTLINES;
import org.kalypso.kalypsomodel1d2d.conv.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ops.ModelOps;
import org.kalypso.kalypsomodel1d2d.ops.TypeInfo;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.PolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.FlowRelationUtilitites;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IFlowRelation1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IWeirFlowRelation;
import org.kalypso.kalypsomodel1d2d.ui.map.ElementGeometryHelper;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeletePolyElementCmd;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.schema.gml.QIntervallResult;
import org.kalypso.model.wspm.tuhh.schema.gml.QIntervallResultCollection;
import org.kalypso.model.wspm.tuhh.schema.schemata.IWspmTuhhQIntervallConstants;
import org.kalypso.ogc.gml.command.CompositeCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.gml.binding.math.IPolynomial1D;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
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
  private final IFEDiscretisationModel1d2d m_model;

  private final GMLWorkspace m_workspace;

  private final IPositionProvider m_positionProvider;

  private final Map<Integer, String> m_nodesNameConversionMap = new HashMap<Integer, String>();

  private final Map<Integer, String> m_edgesNameConversionMap = new HashMap<Integer, String>();

  private final Map<Integer, String> m_elementsNameConversionMap = new HashMap<Integer, String>();

  private final Map<Integer, SortedMap<Integer, Integer>> m_mapIdBuildingType = new HashMap<Integer, SortedMap<Integer, Integer>>();

  private final Map<Integer, Integer> m_mapIdBuildingDirection = new HashMap<Integer, Integer>();

  private final Set<String> m_dirtyModels = new HashSet<String>();

  private GM_Envelope m_gmExistingEnvelope;

  private final Set<Integer> m_setNotInsertedNodes;

  private Map<Integer, QIntervallResult> m_mapQResults = null;

  private Map<String, IPolynomial1D> m_map1dPolynomial = null;

  private static boolean[] NOT_CREATED = new boolean[1];

  private GMLWorkspace m_qResWorkspace;

  private QIntervallResultCollection m_qresultCollection;

  private Set<Integer> m_set1dFlowNodes = null;

  private char m_charDomain;

  private List<Double> m_listPolyRanges;

  private final IFlowRelationshipModel m_flowModel;

  private final GMLWorkspace m_flowWorkspace;

  private final List<Feature> m_listNewFlowElements = new ArrayList<Feature>();

  private final List<Feature> m_listNewPolysWithWeir = new ArrayList<Feature>();

  private final Set<Integer> m_setMiddleNodeIDs = new HashSet<Integer>();

  private final RoughnessHandler m_roughnessHandler;

  private final IScenarioDataProvider m_szenarioDataProvider;

  private boolean m_importRoughness;

  private final String m_crs;

  public DiscretisationModel1d2dHandler( final IScenarioDataProvider szenarioDataProvider, final IPositionProvider positionProvider ) throws CoreException
  {
    m_szenarioDataProvider = szenarioDataProvider;

    m_model = szenarioDataProvider.getModel( IFEDiscretisationModel1d2d.class.getName() );
    m_workspace = m_model.getWorkspace();

    m_flowModel = szenarioDataProvider.getModel( IFlowRelationshipModel.class.getName() );
    m_flowWorkspace = m_flowModel.getWorkspace();

    m_positionProvider = positionProvider;
    m_crs = m_positionProvider.getCoordinateSystem();
    m_setNotInsertedNodes = new HashSet<Integer>();

    final IRoughnessClsCollection roughnessModel = szenarioDataProvider.getModel( IRoughnessClsCollection.class.getName() );
    m_roughnessHandler = new RoughnessHandler( roughnessModel );

    try
    {
      m_gmExistingEnvelope = m_model.getNodes().getBoundingBox();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
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

    m_flowModel.getFlowRelationsShips().getFeatureList().invalidate();

    final Feature[] lElementsToRemove = getElementsWithoutGeometry();
    final Set<Feature> lMidleNodesToRemove = getMidleNodeFeaturesToRemove();
    final Feature[] lAllElements = m_model.getElements().getFeatureList().toFeatures();

    if( lAllElements.length > 0 )
    {
      m_dirtyModels.add( IFEDiscretisationModel1d2d.class.getName() );
    }

    m_workspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_workspace, m_model, lAllElements, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );

    removeElements( lElementsToRemove );
    removeMiddleNodes( lMidleNodesToRemove );

    /*
     * HOTFIX: invalidate all geo-indices here. After import, especially into an empty model, the index used to be very
     * specific (e.g. all elements are in one single box), which causes strange effects later (elements beeing printed
     * twice and similar. Invalidating the geo index here fixes that. However this should be fixed in a more general
     * way.
     */
    createFlowRels2d();
    if( m_listNewFlowElements.size() > 0 )
    {
      m_flowWorkspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_flowWorkspace, m_flowModel, m_listNewFlowElements.toArray( new Feature[m_listNewFlowElements.size()] ), FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );//
      m_dirtyModels.add( IFlowRelationshipModel.class.getName() );
    }

    if( m_listNewPolysWithWeir.size() > 0 )
    {
      m_dirtyModels.add( IFEDiscretisationModel1d2d.class.getName() );
    }

    if( m_roughnessHandler.changeModel() )
      m_dirtyModels.add( IRoughnessClsCollection.class.getName() );
    m_stati.add( m_roughnessHandler.getStatus() );

    m_model.getNodes().getFeatureList().invalidate();
    m_model.getEdges().getFeatureList().invalidate();
    m_model.getElements().getFeatureList().invalidate();
  }

  private void removeMiddleNodes( final Set<Feature> lMidleNodesToRemove )
  {
    // middle nodes are not assigned to arcs, so they are without container.
    // so middle nodes can be removed directly
    m_model.getNodes().removeAll( lMidleNodesToRemove );
    m_workspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_workspace, m_model, lMidleNodesToRemove.toArray( new Feature[lMidleNodesToRemove.size()] ), FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE ) );
  }

  private Set<Feature> getMidleNodeFeaturesToRemove( )
  {
    final Set<Feature> lSetToRemove = new HashSet<Feature>();
    for( final Integer lIntMidleNodeRMAId : m_setMiddleNodeIDs )
    {
      final IFE1D2DNode< ? > lNode = getNode( lIntMidleNodeRMAId );
      if( lNode == null )
        continue;
      lSetToRemove.add( lNode );
    }
    return lSetToRemove;
  }

  /**
   * reconstructs 2d flow parameters (2d weir elements)
   *
   * this function implements the reconstruction of 2d parameters with according poly elements
   */
  private void createFlowRels2d( )
  {
    final Set<Integer> lSetKeys = m_mapIdBuildingType.keySet();
    for( final Integer id : lSetKeys )
    {
      final SortedMap<Integer, Integer> lMapElements = m_mapIdBuildingType.get( id );
      final List<Integer> lListElements = new ArrayList<Integer>();
      lListElements.addAll( lMapElements.values() );
      /* final IPolyElement lNewWeirPoly = */mergeElementsToWeir( lListElements, m_mapIdBuildingDirection.get( id ) );
    }
  }

  /**
   * merges all poly elements of the same building type( weir id > 900 ) into one poly element and creates a new weir
   * flow relationship on this element.
   *
   * after creating of weir flow relationship on new poly element the flow direction is reconstructed from 2d file, the
   * observation is not reconstructed, only initialized
   *
   *
   *
   * lNodePrev lNodeAct +--------->+--------->+ |lPolyPrev | lPoly | | | | | | | | lCommonEdge | | | | | | |
   * +<---------+<---------+ lNodeBckPrev lNodeBckAct
   *
   */
  private IPolyElement< ? , ? > mergeElementsToWeir( final List<Integer> pListElementsIdsRma, final int pIntDegrees )
  {
    final List<GM_Point> lListRes = new ArrayList<GM_Point>();
    final List<GM_Point> lListResBck = new ArrayList<GM_Point>();
    final List<Feature> lListElementsToRemove = new ArrayList<Feature>();
    PolyElement lPoly = null;
    PolyElement lPolyPrev = null;
    IFE1D2DNode< ? > lNodePrev = null;
    IFE1D2DNode< ? > lNodeBckPrev = null;
    IFE1D2DNode< ? > lNodeAct = null;
    IFE1D2DNode< ? > lNodeBckAct = null;
    IFE1D2DEdge< ? , ? > lCommonEdge = null;
    try
    {
      for( final Object element : pListElementsIdsRma )
      {
        final Integer lIntRMAId = (Integer) element;

        final Feature polyFeature = m_workspace.getFeature( m_elementsNameConversionMap.get( lIntRMAId ) );
        lPoly = (PolyElement) polyFeature.getAdapter( IPolyElement.class );
        if( lPolyPrev != null )
        {
          lCommonEdge = findCommonEdge( lPoly, lPolyPrev );
          if( lCommonEdge == null )
          {
            // TODO: should not happen, but what to do? or maybe the order of elements was changed, but this is possible
            // only manually
            // throw new Exception( "Error in model file, cannot reconstruct 2d building element" );
            return null;
          }
          if( lNodePrev == null )
          {
            lNodePrev = findConnectedNode( lCommonEdge.getNode( 0 ), lPolyPrev, lCommonEdge, null );
            lNodeAct = lCommonEdge.getNode( 0 );
            lListRes.add( lNodePrev.getPoint() );
          }
          else
          {
            lNodeAct = findConnectedNode( lNodePrev, lPolyPrev, lCommonEdge, lNodeBckPrev );
          }

          if( lNodeBckPrev == null )
          {
            lNodeBckPrev = findConnectedNode( lCommonEdge.getNode( 1 ), lPolyPrev, lCommonEdge, null );
            lNodeBckAct = lCommonEdge.getNode( 1 );
            lListResBck.add( lNodeBckPrev.getPoint() );
          }
          else
          {
            lNodeBckAct = findConnectedNode( lNodeBckPrev, lPolyPrev, lCommonEdge, lNodePrev );
          }

          lListRes.add( lNodeAct.getPoint() );
          lNodePrev = lNodeAct;
          lListResBck.add( lNodeBckAct.getPoint() );
          lNodeBckPrev = lNodeBckAct;
        }
        lListElementsToRemove.add( lPoly );
        lPolyPrev = lPoly;
      }

      if( lNodeAct != null )
      {
        lNodeAct = findConnectedNode( lNodeAct, lPoly, lCommonEdge, lNodeBckAct );
        lListRes.add( lNodeAct.getPoint() );
      }

      if( lNodeBckAct != null )
      {
        lNodeBckAct = findConnectedNode( lNodeBckAct, lPoly, lCommonEdge, lNodeAct );
        lListResBck.add( lNodeBckAct.getPoint() );
      }

      Collections.reverse( lListResBck );
      lListRes.addAll( lListResBck );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    final CompositeCommand command = new CompositeCommand( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.DiscretisationModel1d2dHandler.4" ) ); //$NON-NLS-1$

    IPolyElement< ? , ? > lNewPoly = lPoly;
    if( pListElementsIdsRma.size() > 1 )
    {
      try
      {
        // FIXME: using a command here is really ugly!
        final CommandableWorkspace cmdWorkspace2d = m_szenarioDataProvider.getCommandableWorkSpace( IFEDiscretisationModel1d2d.class.getName() );
        lNewPoly = (IPolyElement) ElementGeometryHelper.createAdd2dElement( command, cmdWorkspace2d, m_model, lListRes );
        command.process();
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }
    if( lNewPoly != null )
    {
      final Feature lNewFlowFeature = createNewFlowrelation( lNewPoly, pIntDegrees );
      m_listNewFlowElements.add( lNewFlowFeature );
      // cleanup and update
      if( pListElementsIdsRma.size() > 1 )
      {
        m_listNewPolysWithWeir.add( lNewPoly );
        removeElements( lListElementsToRemove.toArray( new Feature[lListElementsToRemove.size()] ) );
      }
    }

    return lNewPoly;
  }

  private IFE1D2DNode findConnectedNode( final IFE1D2DNode pNodeGiven, final PolyElement lPoly, final IFE1D2DEdge pEdgeToSkip, final IFE1D2DNode pNodeToSkip )
  {
    for( final IFE1D2DEdge lEdge : lPoly.getEdges() )
    {
      if( lEdge.getId().equals( pEdgeToSkip.getId() ) )
        continue;
      final IFE1D2DNode lNode0 = lEdge.getNode( 0 );
      final IFE1D2DNode lNode1 = lEdge.getNode( 1 );
      if( lNode0.equals( pNodeGiven ) && !lNode1.equals( pNodeToSkip ) )
        return lNode1;
      if( lNode1.equals( pNodeGiven ) && !lNode0.equals( pNodeToSkip ) )
        return lNode0;
    }
    return null;
  }

  private IFE1D2DEdge findCommonEdge( final PolyElement pPoly0, final PolyElement pPoly1 )
  {
    for( final IFE1D2DEdge lEdge : pPoly0.getEdges() )
    {
      if( pPoly1.getEdges().contains( lEdge ) )
        return lEdge;
    }
    return null;
  }

  protected IWeirFlowRelation createNew2dWeirFeature( final Feature parentFeature, final IRelationType parentRelation, final int pIntDegrees )
  {
    final IFeatureType newFT = GMLSchemaUtilities.getFeatureTypeQuiet( IWeirFlowRelation.QNAME );
    final Feature newFeature = m_flowWorkspace.createFeature( parentFeature, parentRelation, newFT, -1 );
    final IWeirFlowRelation weirRelation = (IWeirFlowRelation) newFeature.getAdapter( IWeirFlowRelation.class );

    /* Call getObservation once to initialize it */
    weirRelation.getBuildingObservation();
    weirRelation.setDirection( pIntDegrees );
    return weirRelation;
  }

  private Feature createNewFlowrelation( final IPolyElement pPoly, final int pIntDegrees )
  {
    final GM_Position flowPositionFromElement = FlowRelationUtilitites.getFlowPositionFromElement( pPoly );
    final IRelationType parentRelation = m_flowModel.getFlowRelationsShips().getFeatureList().getPropertyType();
    final IFlowRelationship flowRel = createNew2dWeirFeature( m_flowModel, parentRelation, pIntDegrees );

    final String crs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    flowRel.setPosition( GeometryFactory.createGM_Point( flowPositionFromElement, crs ) );

    return flowRel;
  }

  /**
   * creates 1d flow parameters that was found on 2d model
   *
   * this function implements the reconstruction of 1d parameters on the according 1d elements, profile data are not
   * handled in 2d file, so only the polynomials are created and the flow relationships are not connected to any
   * profiles
   *
   */
  private int createFlowRels1d( )
  {
    if( m_set1dFlowNodes == null || m_mapQResults == null )
      return 0;

    int lIntCountNew = 0;
    final SortedMap<BigDecimal, IProfileFeature> profilesByStation = new TreeMap<BigDecimal, IProfileFeature>();
    for( final Object element : m_set1dFlowNodes )
    {
      final Integer lId = (Integer) element;
      final IFE1D2DNode< ? > lActNode = getNode( lId );
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

  private void removeElements( final Feature[] elementsToRemove )
  {
    final DeletePolyElementCmd deleteCmdPolyElement = new DeletePolyElementCmd( m_model );

    for( final Feature feature : elementsToRemove )
    {
      if( feature != null )
      {

        if( TypeInfo.isPolyElementFeature( feature ) )
        {
          deleteCmdPolyElement.addElementToRemove( feature );
        }
      }
    }
    try
    {
      deleteCmdPolyElement.process();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    m_model.getElements().removeAll( Arrays.asList( elementsToRemove ) );
    m_workspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_workspace, m_model, elementsToRemove, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE ) );
  }

  private Feature[] getElementsWithoutGeometry( )
  {
    final Set<Feature> lSetToRemove = new HashSet<Feature>();
    for( final IFE1D2DElement lElement : m_model.getElements() )
    {
      if( lElement instanceof IPolyElement )
      {
        final GM_Surface<GM_SurfacePatch> eleGeom = ((IPolyElement) lElement).getGeometry();
        if( eleGeom == null )
        {
          lSetToRemove.add( lElement );
        }
      }
    }
    return lSetToRemove.toArray( new Feature[lSetToRemove.size()] );
  }

  @Override
  public void handleArc( final String lineString, final int id, final int node1ID, final int node2ID, final int elementLeftID, final int elementRightID, final int middleNodeID )
  {
    final IFE1D2DNode< ? > node1 = getNode( node1ID );
    final IFE1D2DNode< ? > node2 = getNode( node2ID );
    m_setMiddleNodeIDs.add( middleNodeID );

    // FIXME: allow for arc with only one existing node, is this legal?
    if( node1 == null )
    {
      if( !m_setNotInsertedNodes.contains( node1ID ) )
      {
        throw new RuntimeException( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.DiscretisationModel1d2dHandler.0", node1ID, id ) ); //$NON-NLS-1$
      }
      return;
    }
    if( node2 == null )
    {
      if( !m_setNotInsertedNodes.contains( node2ID ) )
      {
        throw new RuntimeException( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.DiscretisationModel1d2dHandler.1", node1ID, id ) ); //$NON-NLS-1$
      }
      return;
    }

    /* Create edge for the two existing nodes */
    final IFE1D2DEdge< ? , ? > existingEdge = m_model.findEdge( node1, node2 );
    final IFE1D2DEdge< ? , ? > edge;
    if( existingEdge != null )
    {
      edge = existingEdge;
    }
    else
    {
      edge = FE1D2DEdge.createFromModel( m_model, node1, node2 );
    }

    final String gmlID = edge.getId();
    m_edgesNameConversionMap.put( id, gmlID );
    if( elementLeftID == elementRightID )
    {
      // 1d
      maybeAddNewElement1d( elementLeftID, edge );
    }
    else
    {
      // 2d
      maybeAddEdgeToElement( elementLeftID, edge );
      maybeAddEdgeToElement( elementRightID, edge );
    }
  }

  private final IFE1D2DNode< ? > getNode( final int rmaID )
  {
    final String nodeGmlID = m_nodesNameConversionMap.get( rmaID );
    if( nodeGmlID == null )
      return null;

    final Feature nodeFeature = m_workspace.getFeature( nodeGmlID );
    return (IFE1D2DNode< ? >) nodeFeature.getAdapter( IFE1D2DNode.class );
  }

  private final void maybeAddEdgeToElement( final int rmaID, final IFE1D2DEdge< ? , ? > edge )
  {
    final String edgeId = edge.getId();

    final IFeatureBindingCollection< ? > lContainers = edge.getContainers();

    int iCountPolyElements = 0;
    for( int i = 0; i < lContainers.size(); ++i )
    {
      final Object lFeature = lContainers.get( i );
      if( lFeature instanceof IPolyElement )
      {
        iCountPolyElements++;
        if( rmaID != 0 )
        {
          final String gmlID = m_elementsNameConversionMap.get( rmaID );

          final String elementID = ((IPolyElement< ? , ? >) lFeature).getId();
          if( gmlID == null && !m_elementsNameConversionMap.values().contains( elementID ) )
          {
            m_elementsNameConversionMap.put( rmaID, elementID );
          }
        }
      }

      // Edge already has two neighbouring elements
      if( iCountPolyElements == 2 )
        return;
    }

    final IPolyElement< ? , ? > element = getOrCreateElement( rmaID, edge );
    if( element != null )
    {
      // add edge to element and element to edge
      final String elementId = element.getId();
      element.addEdge( edgeId );
      edge.addContainer( elementId );
      if( rmaID != 0 )
      {
        // remember imported element
        m_elementsNameConversionMap.put( rmaID, elementId );
      }
    }
  }

  private IPolyElement< ? , ? > getOrCreateElement( final int rmaID, final IFE1D2DEdge< ? , ? > edge )
  {
    if( rmaID == 0 )
    {
      // this is either the outer boundary or an adjacent existing element
      // try to find an element in the model that lies on the edge
      final GM_Point middleNodePoint = edge.getMiddleNodePoint();
      final IPolyElement< ? , ? > existingElement2d = m_model.find2DElement( middleNodePoint, 0.01 );
      if( existingElement2d == null )
        return null;

      return existingElement2d;
    }

    final String gmlID = m_elementsNameConversionMap.get( rmaID );
    if( gmlID != null )
    {
      // this is an imported element
      return (IPolyElement< ? , ? >) m_workspace.getFeature( gmlID ).getAdapter( IPolyElement.class );
    }

    // really create a new element
    return m_model.getElements().addNew( IPolyElement.QNAME, IPolyElement.class );
  }

  private final void maybeAddNewElement1d( final int rmaID, final IFE1D2DEdge edge )
  {
    final String gmlID = m_elementsNameConversionMap.get( rmaID );
    if( gmlID == null )
    {
      final IElement1D existingElement1d = m_model.find1DElement( edge.getMiddleNodePoint(), 0.01 );
      final IElement1D element1d;
      if( existingElement1d != null )
      {
        element1d = existingElement1d;
      }
      else
      {
        element1d = ModelOps.createElement1d( m_model, edge );
      }
      m_elementsNameConversionMap.put( rmaID, element1d.getId() );
    }
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
          lMapElements = new TreeMap<Integer, Integer>();
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
    final IFE1D2DNode< ? > nodeWithSameId = getNode( id );
    if( nodeWithSameId != null )
      throw new RuntimeException( String.format( "ducplicate node id: %s", id ) );

    final GM_Point nodeLocation = m_positionProvider.getGMPoint( xCoord, yCoord, elevation );
    nodeLocation.setCoordinateSystem( m_crs );
    final IFE1D2DNode< ? > existingNode = m_model.findNode( nodeLocation, 0.01 );

    final IFE1D2DNode< ? > node;
    if( existingNode == null )
    {
      if( m_gmExistingEnvelope != null && m_gmExistingEnvelope.contains( nodeLocation.getPosition() ) )
      {
        final IPolyElement< ? , ? > lFoundElement = m_model.find2DElement( nodeLocation, 0.01 );
        if( lFoundElement != null )
        {
          // do not insert nodes that are placed on existing model(overlapped elements)
          m_setNotInsertedNodes.add( id );
          Logger.getLogger( DiscretisationModel1d2dHandler.class.getName() ).log( Level.WARNING, "removed node ", nodeLocation.toString() ); //$NON-NLS-1$
          return;
        }
      }

      // new node, create
      node = m_model.createNode( nodeLocation, -1, NOT_CREATED );
    }
    else
    {
      // this means that in .2d file several nodes with different IDs have the same coords!
      // What to do?
      // For the moment, we will assume that it is the same node
      // FIXME: does not help the user!
      Logger.getLogger( DiscretisationModel1d2dHandler.class.getName() ).log( Level.WARNING, Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.DiscretisationModel1d2dHandler.3", existingNode.getPoint().toString() ) ); //$NON-NLS-1$
      node = existingNode;
    }

    m_nodesNameConversionMap.put( id, node.getId() );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handlerError(java.lang.String,
   *      org.kalypso.kalypsomodel1d2d.conv.EReadError)
   */
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
  public void handleFlowResitance( final String line, final int id, final double combinedLambda, final double soilLambda, final double vegetationLambda )
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

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handle1dPolynomialRangesInformation(java.lang.String,
   *      java.lang.String, int, int, java.util.List)
   */
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

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handle1dPolynomeMinMax(java.lang.String, int,
   *      double, double)
   */
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

  private QIntervallResult getQResult( final int id )
  {
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
      final IPolynomial1D polynom = getQResult( id ).createPolynomial();
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
      m_qresultCollection = (QIntervallResultCollection) m_qResWorkspace.getRootFeature();
      m_mapQResults = new HashMap<Integer, QIntervallResult>();
      m_map1dPolynomial = new HashMap<String, IPolynomial1D>();
      m_set1dFlowNodes = new HashSet<Integer>();
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
      final QIntervallResult lQResult = getQResult( pIntNodeId );
      lQResult.setSlope( new BigDecimal( pDoubleSlope ) );
    }
  }

  @Override
  public void handleNode( final String line, final int id, final double easting, final double northing, final double elevation, final double stationName )
  {
    handleNode( line, id, easting, northing, elevation );

    final QIntervallResult result = getQResult( id );
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
    return m_stati.asMultiStatusOrOK( "2D-Import", Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.DiscretisationModel1d2dHandler.5" ) );
  }
}