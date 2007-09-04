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
package org.kalypso.kalypsomodel1d2d.ui.wizard;

import java.io.FileNotFoundException;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.ops.CalcUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.DiscretisationModelUtils;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.FlowRelationUtilitites;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBridgeFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.ITeschkeFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IWeirFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.Add1DElementFromNodeCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.AddNodeCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeDiscretiationModelCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeleteCmdFactory;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.AddElementToCalculationUnitCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.CreateCalculationUnitCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelUtil;
import org.kalypso.kalypsosimulationmodel.core.Util;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetwork;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetworkCollection;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.schema.gml.ProfileCacherFeaturePropertyFunction;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhSegmentStationComparator;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhStationComparator;
import org.kalypso.model.wspm.tuhh.schema.gml.QIntervallResult;
import org.kalypso.model.wspm.tuhh.schema.gml.QIntervallResultCollection;
import org.kalypso.model.wspm.tuhh.schema.schemata.IWspmTuhhQIntervallConstants;
import org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.phenomenon.IPhenomenon;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.observation.result.TupleResultUtilities;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.wizard.gml.GmlFileImportPage;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * A wizard to import WSPM-Models into a 1D2D Model.
 * 
 * @author Gernot Belger
 */
public class ImportWspmWizard extends Wizard implements IWizard
{
  private static final DateFormat DF = DateFormat.getDateTimeInstance( DateFormat.MEDIUM, DateFormat.SHORT );

  private final List<Feature> m_discModelAdds = new ArrayList<Feature>();

  private final List<Feature> m_terrainModelAdds = new ArrayList<Feature>();

  private GmlFileImportPage m_wspmGmlPage;

  private final IRiverProfileNetworkCollection m_networkModel;

  private final IFEDiscretisationModel1d2d m_discretisationModel;

  private final IFlowRelationshipModel m_flowRelationCollection;

  public ImportWspmWizard( final IFEDiscretisationModel1d2d discretisationModel, final IRiverProfileNetworkCollection networkModel, final IFlowRelationshipModel flowRelationCollection )
  {
    m_discretisationModel = discretisationModel;
    m_networkModel = networkModel;
    m_flowRelationCollection = flowRelationCollection;

    setWindowTitle( Messages.getString("ImportWspmWizard.0") ); //$NON-NLS-1$

    setNeedsProgressMonitor( true );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    /* Choose wspm-reach */
    m_wspmGmlPage = new GmlFileImportPage( "chooseWspmGml", Messages.getString("ImportWspmWizard.2"), null ); //$NON-NLS-1$ //$NON-NLS-2$
    m_wspmGmlPage.setDescription( Messages.getString("ImportWspmWizard.3") ); //$NON-NLS-1$
    m_wspmGmlPage.setValidQNames( new QName[] { TuhhCalculation.QNAME_TUHH_CALC_REIB_CONST } );
    m_wspmGmlPage.setValidKind( true, false );

    /* Choose network collection */
    m_wspmGmlPage.setValidKind( true, false );

    addPage( m_wspmGmlPage );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    /* Collect page data */
    final IStructuredSelection wspmSelection = m_wspmGmlPage.getSelection();

    final IRiverProfileNetworkCollection profNetworkColl = m_networkModel;
    final IFEDiscretisationModel1d2d discModel = m_discretisationModel;
    final IFlowRelationshipModel flowRelModel = m_flowRelationCollection;

    final List<Feature> discModelAdds = m_discModelAdds;
    final List<Feature> terrainModelAdds = m_terrainModelAdds;

    /* Do import */
    final ICoreRunnableWithProgress op = new ICoreRunnableWithProgress()
    {
      public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
      {
        monitor.beginTask( Messages.getString("ImportWspmWizard.4"), 3 ); //$NON-NLS-1$

        try
        {
          /* Activate network map */

          /* Prepare input data */
          final TuhhCalculation calculation = new TuhhCalculation( (Feature) wspmSelection.getFirstElement() );

          /* Check if its the right calculation and if results are present */
          if( calculation.getCalcMode() != TuhhCalculation.MODE.REIB_KONST )
            return StatusUtilities.createWarningStatus( Messages.getString("ImportWspmWizard.5") ); //$NON-NLS-1$

          final TuhhReach reach = calculation.getReach();

          try
          {
            /* Import reach into profile collection */
            monitor.subTask( Messages.getString("ImportWspmWizard.6") ); //$NON-NLS-1$
            final SortedMap<BigDecimal, WspmProfile> profilesByStation = doImportNetwork( reach, profNetworkColl, terrainModelAdds );
            monitor.worked( 1 );

            /* Create 1D-Network */
            monitor.subTask( Messages.getString("ImportWspmWizard.7") ); //$NON-NLS-1$
            final SortedMap<BigDecimal, IFE1D2DNode> elementsByStation = doCreate1DNet( reach, discModel, discModelAdds );
            monitor.worked( 1 );

            /* Create 1D-Network parameters (flow relations) */
            monitor.subTask( Messages.getString("ImportWspmWizard.8") ); //$NON-NLS-1$
            final IStatus status = doReadResults( calculation, elementsByStation, flowRelModel, profilesByStation );
            monitor.worked( 1 );

            return status;
          }
          catch( final Exception e )
          {
            return StatusUtilities.statusFromThrowable( e, Messages.getString("ImportWspmWizard.9") ); //$NON-NLS-1$
          }
        }
        catch( final Throwable t )
        {
          // TODO: remove all added features from terrainModel

          // TODO: remove all added features from discModel

          throw new InvocationTargetException( t );
        }
        finally
        {
          monitor.done();
        }
      }
    };

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, op );
    if( !status.isOK() )
      KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
    ErrorDialog.openError( getShell(), getWindowTitle(), Messages.getString("ImportWspmWizard.10"), status ); //$NON-NLS-1$

    return !status.matches( IStatus.ERROR );
  }

  /**
   * Reads a REIB_CONST result and creates polynomial and building parameters (aka 'flow-relations') from it.
   * 
   * @param elements
   *            by station Must be sorted in the order of the flow direction
   */
  protected static IStatus doReadResults( final TuhhCalculation calculation, final SortedMap<BigDecimal, IFE1D2DNode> elementsByStation, final IFlowRelationshipModel flowRelModel, final SortedMap<BigDecimal, WspmProfile> profilesByStation ) throws MalformedURLException, Exception
  {
    try
    {
      final GMLWorkspace calcWorkspace = calculation.getFeature().getWorkspace();
      final URL calcContext = calcWorkspace.getContext();
      final URL qresultsUrl = new URL( calcContext, "Ergebnisse/" + calculation.getName() + "/_aktuell/Daten/qIntervallResults.gml" ); //$NON-NLS-1$ //$NON-NLS-2$
      final GMLWorkspace qresultsWorkspace = GmlSerializer.createGMLWorkspace( qresultsUrl, calcWorkspace.getFeatureProviderFactory() );
      final QIntervallResultCollection qResultCollection = new QIntervallResultCollection( qresultsWorkspace.getRootFeature() );

      final Feature flowRelParentFeature = flowRelModel.getWrappedFeature();
      final GMLWorkspace flowRelworkspace = flowRelParentFeature.getWorkspace();

      final List resultList = qResultCollection.getQResultFeatures();

      final List<Feature> addedFeatures = new ArrayList<Feature>();
      for( final Object o : resultList )
      {
        final QIntervallResult qresult = new QIntervallResult( FeatureHelper.getFeature( qresultsWorkspace, o ) );

        final BigDecimal station = qresult.getStation();

        // get corresponding 1d-element
        final IFE1D2DNode node = (IFE1D2DNode) PolynomeHelper.forStation( elementsByStation, station );

        final IFlowRelationship flowRel;

        /* Do we have a building? */
        final IObservation<TupleResult> buildingObs = qresult.getBuildingObservation( false );
        if( node == null )
        {
          KalypsoModel1D2DPlugin.getDefault().getLog().log( StatusUtilities.createWarningStatus( "No node for result at station " + station ) );
          flowRel = null;
        }
        else if( buildingObs != null )
        {
          // REMARK: it is importent that elementsByStation is sorted in upstream direction
          final IFE1D2DNode downStreamNode = PolynomeHelper.forStationAdjacent( elementsByStation, station, false );
          final IFE1D2DNode upStreamNode = PolynomeHelper.forStationAdjacent( elementsByStation, station, true );
          if( downStreamNode == null || upStreamNode == null )
          {
            KalypsoModel1D2DPlugin.getDefault().getLog().log( StatusUtilities.createWarningStatus( "Buildings has no adjacent nodes. Import impossible for station: " + station ) );
            flowRel = null;
          }
          else
            flowRel = addBuilding( flowRelModel, node, buildingObs, downStreamNode, upStreamNode );
        }
        else
        {
          flowRel = addTeschke( flowRelModel, node, qresult, profilesByStation );
        }

        if( flowRel != null )
        {
          /* Set common properties and add to list of freshly generated features. */
          flowRel.setName( qresult.getName() );
          flowRel.setDescription( qresult.getDescription() );

          addedFeatures.add( flowRel.getWrappedFeature() );
        }
      }

      final Feature[] addedFeatureArray = addedFeatures.toArray( new Feature[addedFeatures.size()] );
      flowRelworkspace.fireModellEvent( new FeatureStructureChangeModellEvent( flowRelworkspace, flowRelParentFeature, addedFeatureArray, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
    }
    catch( final FileNotFoundException fnfe )
    {
      fnfe.printStackTrace();

      /* Results are noit available, just inform user */
      return StatusUtilities.createWarningStatus( Messages.getString("ImportWspmWizard.15") ); //$NON-NLS-1$
    }

    return Status.OK_STATUS;
  }

  private static IFlowRelationship addTeschke( final IFlowRelationshipModel flowRelModel, final IFE1D2DNode node, final QIntervallResult qresult, final SortedMap<BigDecimal, WspmProfile> profilesByStation ) throws Exception
  {
    final Feature flowRelParentFeature = flowRelModel.getWrappedFeature();
    final GMLWorkspace flowRelworkspace = flowRelParentFeature.getWorkspace();
    final IFeatureType flowRelFT = flowRelworkspace.getGMLSchema().getFeatureType( ITeschkeFlowRelation.QNAME );
    final IRelationType flowRelObsRelation = (IRelationType) flowRelFT.getProperty( ITeschkeFlowRelation.QNAME_PROP_POINTSOBSERVATION );
    final IRelationType flowRelPolynomeRelation = (IRelationType) flowRelFT.getProperty( ITeschkeFlowRelation.QNAME_PROP_POLYNOMES );

    /* create new flow relation at node position */
    final ITeschkeFlowRelation flowRel = flowRelModel.addNew( ITeschkeFlowRelation.QNAME, ITeschkeFlowRelation.class );

    final BigDecimal station = qresult.getStation();

    flowRel.setPosition( node.getPoint() );
    flowRel.setStation( station );
    flowRel.setSlope( qresult.getSlope().setScale( 5, BigDecimal.ROUND_HALF_UP ).doubleValue() ); // Round to 5 fraction
    // digits

    /* copy results into new flow relation */

    /* clone observation */
    final Feature pointObsFeature = (Feature) qresult.getWrappedFeature().getProperty( QIntervallResult.QNAME_P_QIntervallResult_pointsMember );
    if( pointObsFeature != null )
      FeatureHelper.cloneFeature( flowRel.getWrappedFeature(), flowRelObsRelation, pointObsFeature );

    /* relink profile to corresponding profile in profile network */
    final WspmProfile wspmProfile = profilesByStation.get( station );
    if( wspmProfile != null )
      flowRel.setProfileLink( "terrain.gml#" + wspmProfile.getFeature().getId() );

    /* clone polynomes */
    final List polynomeFeatures = qresult.getPolynomialFeatures();
    if( polynomeFeatures != null )
    {
      for( final Object object : polynomeFeatures )
      {
        final GMLWorkspace qresultsWorkspace = qresult.getFeature().getWorkspace();
        final Feature polynomeFeature = FeatureHelper.getFeature( qresultsWorkspace, object );
        if( polynomeFeature != null )
          FeatureHelper.cloneFeature( flowRel.getWrappedFeature(), flowRelPolynomeRelation, polynomeFeature );
      }
    }

    return flowRel;
  }

  private static IBuildingFlowRelation addBuilding( final IFlowRelationshipModel flowRelModel, final IFE1D2DNode node, final IObservation<TupleResult> buildingObs, final IFE1D2DNode downStreamNode, final IFE1D2DNode upStreamNode ) throws CoreException
  {
    final IPhenomenon buildingPhenomenon = buildingObs.getPhenomenon();
    final String buildingId = buildingPhenomenon.getID();
    final QName buildingQName;
    if( IWspmTuhhConstants.BUILDING_TYP_WEHR.equals( buildingId ) )
      buildingQName = IWeirFlowRelation.QNAME;
    else if( IWspmTuhhConstants.BUILDING_TYP_BRUECKE.equals( buildingId ) )
      buildingQName = IBridgeFlowRelation.QNAME;
    else
      throw new IllegalStateException();

    final IBuildingFlowRelation buildingRelation = flowRelModel.addNew( buildingQName, IBuildingFlowRelation.class );
    final IObservation<TupleResult> weirFlowObservation = buildingRelation.getBuildingObservation();

    final GM_Point weirPos = replaceNodeWithElement( node );
    buildingRelation.setPosition( weirPos );

    /* Derive direction from flow-direction of adjacent results */
    final GM_Position downStreamPosition = downStreamNode.getPoint().getPosition();
    final GM_Position upStreamPosition = upStreamNode.getPoint().getPosition();

    /* Direction goes from upstream to downstream */
    final double degrees = GeometryUtilities.directionFromPositions( upStreamPosition, downStreamPosition );
    buildingRelation.setDirection( (int) degrees );

    /* copy building parameter from one observation to the other */
    final TupleResult weirResult = buildingObs.getResult();
    final TupleResult result = weirFlowObservation.getResult();
    final Map<String, String> componentMap = new HashMap<String, String>();
    componentMap.put( IWspmTuhhQIntervallConstants.DICT_COMPONENT_RUNOFF, Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE );
    componentMap.put( IWspmTuhhQIntervallConstants.DICT_COMPONENT_WATERLEVEL_DOWNSTREAM, Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL_DOWNSTREAM );
    componentMap.put( IWspmTuhhQIntervallConstants.DICT_COMPONENT_WATERLEVEL_UPSTREAM, Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL_UPSTREAM );
    TupleResultUtilities.copyValues( weirResult, result, componentMap );
    buildingRelation.setBuildingObservation( weirFlowObservation );

    return buildingRelation;
  }

  private static GM_Point replaceNodeWithElement( final IFE1D2DNode node ) throws CoreException
  {
    final GMLWorkspace workspace = node.getWrappedFeature().getWorkspace();
    final IFEDiscretisationModel1d2d model1d2d = DiscretisationModelUtils.modelForItem( node );

    final IFE1D2DElement[] elements = node.getElements();
    if( elements.length != 2 )
      throw new CoreException( StatusUtilities.createErrorStatus( Messages.getString("ImportWspmWizard.16") ) ); //$NON-NLS-1$

    /* Find upstream and downstream neighbours */
    final IFE1D2DNode neighbour0 = DiscretisationModelUtils.findOtherNode( node, elements[0] );
    final IFE1D2DNode neighbour1 = DiscretisationModelUtils.findOtherNode( node, elements[1] );

    try
    {
      /* delete elements (and associated edges) */
      final Feature feature0 = elements[0].getWrappedFeature();
      final Feature feature1 = elements[1].getWrappedFeature();
      final EasyFeatureWrapper[] toDelete = new EasyFeatureWrapper[] { new EasyFeatureWrapper( null, feature0, feature0.getParent(), feature0.getParentRelation() ),
          new EasyFeatureWrapper( null, feature1, feature1.getParent(), feature1.getParentRelation() ) };

      final ChangeDiscretiationModelCommand elementDeleteCmd = new ChangeDiscretiationModelCommand( workspace, model1d2d );
      DeleteCmdFactory.createDeleteCmd( model1d2d, toDelete, elementDeleteCmd );
      elementDeleteCmd.process();

      // REMARK: get calculation unit of element to delete
      // We know that we have exactly one 1D-Calculation unit
      final IFeatureWrapperCollection<ICalculationUnit1D> containersOfElement0 = elements[0].getContainers();
      final ICalculationUnit1D calcUnit = containersOfElement0.get( 0 );

      /* create new edge beetween neighbour nodes */
      final ChangeDiscretiationModelCommand elementAddCmd = new ChangeDiscretiationModelCommand( workspace, model1d2d );
      final AddNodeCommand addNode0 = new AddNodeCommand( model1d2d, neighbour0.getPoint(), 0.0 );
      final AddNodeCommand addNode1 = new AddNodeCommand( model1d2d, neighbour1.getPoint(), 0.0 );
      final Add1DElementFromNodeCmd eleCmd = new Add1DElementFromNodeCmd( model1d2d, new AddNodeCommand[] { addNode0, addNode1 } );
      elementAddCmd.addCommand( addNode0 );
      elementAddCmd.addCommand( addNode1 );
      elementAddCmd.addCommand( eleCmd );

      elementAddCmd.process();

      // REMARK: also copy the containers of the old elements to the new element
      final AddElementToCalculationUnitCmd calcUnitCmd = new AddElementToCalculationUnitCmd( calcUnit, new IFE1D2DElement[] { eleCmd.getAddedElement() }, model1d2d );
      calcUnitCmd.process();

      /* Return position of weir */
      final GM_Position position = FlowRelationUtilitites.getFlowPositionFromElement( eleCmd.getAddedElement() );
      return GeometryFactory.createGM_Point( position, node.getPoint().getCoordinateSystem() );
    }
    catch( final Exception e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }

  }

  protected static SortedMap<BigDecimal, IFE1D2DNode> doCreate1DNet( final TuhhReach reach, final IFEDiscretisationModel1d2d discretisationModel, final List<Feature> addedFeatures ) throws Exception
  {
    /* sort reach by station and direction */
    final WspmWaterBody waterBody = reach.getWaterBody();
    final boolean isDirectionUpstreams = waterBody.isDirectionUpstreams();
    final TuhhReachProfileSegment[] segments = reach.getReachProfileSegments();
    Arrays.sort( segments, new TuhhSegmentStationComparator( isDirectionUpstreams ) );

    /* Get some common variables */
    final IFeatureWrapperCollection<IFE1D2DElement> discElements = discretisationModel.getElements();
    final FeatureList discElementsList = discElements.getWrappedList();
    final QName discElementsMemberQName = discElementsList.getParentFeatureTypeProperty().getQName();
    final IFeatureWrapperCollection<IFE1D2DEdge> discEdges = discretisationModel.getEdges();

    /* add complex-element to model: Automatically create a calculation unit 1d */
    
    /*
     * NO, this is not the right way to do it! 
     * 
     * During creation of calculation unit, control model for that unit should be created as well.
     * 
     * Use CreateCalculationUnitCmd
     */
    
//    final ICalculationUnit1D calculationUnit1D = discretisationModel.getComplexElements().addNew( Kalypso1D2DSchemaConstants.WB1D2D_F_CALC_UNIT_1D, ICalculationUnit1D.class );
//    addedFeatures.add( calculationUnit1D.getWrappedFeature() );
//    final String calcUnitName = String.format( "WSPM-Import: %s - %s", waterBody.getName(), reach.getName() );
//    calculationUnit1D.setName( calcUnitName );
//    calculationUnit1D.setDescription( "Dieses Teilmodell wurde durch den Import automatisch angelegt." );
    
    /* add nodes to model */
    final List<IFE1D2DEdge> edgeList = new ArrayList<IFE1D2DEdge>( segments.length - 1 );

    /* add elements to model and sort by station in flow direction */
    // IMPORTANT: the right ordering (in flow direction) is later used by the building parameter stuff, so do not change
    // it!
    final SortedMap<BigDecimal, IFE1D2DNode> nodesByStation = new TreeMap<BigDecimal, IFE1D2DNode>( new TuhhStationComparator( isDirectionUpstreams ) );
    IFE1D2DNode lastNode = null;
    for( final TuhhReachProfileSegment segment : segments )
    {
      final WspmProfile profileMember = segment.getProfileMember();
      final double station = profileMember.getStation();

      /* find sohlpunkt */
      final IProfil profil = profileMember.getProfil();
      final IProfilPoint sohlPoint = ProfilUtil.getMinPoint( profil, IWspmConstants.POINT_PROPERTY_HOEHE );
      final GM_Point point = ProfileCacherFeaturePropertyFunction.convertPoint( profil, sohlPoint );

      /* add node */
      final IFE1D2DNode node = discretisationModel.getNodes().addNew( Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
      addedFeatures.add( node.getWrappedFeature() );

      node.setName( "" ); //$NON-NLS-1$
      final String desc = String.format( Messages.getString("ImportWspmWizard.18") + profileMember.getDescription() ); //$NON-NLS-1$
      node.setDescription( desc );

      if( point == null )
        throw new CoreException( StatusUtilities.createErrorStatus( Messages.getString("ImportWspmWizard.19") + station ) ); //$NON-NLS-1$

      node.setPoint( point );

      final BigDecimal stationDecimal = WspmProfile.stationToBigDecimal( station );
      nodesByStation.put( stationDecimal, node );

      if( lastNode != null )
      {
        /* Create edge between lastNode and node */
        final IFE1D2DEdge edge = discEdges.addNew( Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE );
        addedFeatures.add( edge.getWrappedFeature() );

        edge.addNode( lastNode.getGmlID() );
        edge.addNode( node.getGmlID() );

        lastNode.addContainer( edge.getGmlID() );
        node.addContainer( edge.getGmlID() );

        edge.getWrappedFeature().invalidEnvelope();

        edgeList.add( edge );

        /* Create corresponding element */
        final IElement1D element1d = discretisationModel.getElements().addNew( Kalypso1D2DSchemaConstants.WB1D2D_F_ELEMENT1D, IElement1D.class );

        addedFeatures.add( element1d.getWrappedFeature() );

        element1d.setEdge( edge );

        /* Add complex element to list of containers of this element */
//        element1d.getContainers().addRef( calculationUnit1D );

        /* Add this element to the complex element aka calculation unit */
//        calculationUnit1D.addElementAsRef( element1d );
      }

      lastNode = node;
    }

    final Feature[] addedFeatureArray = addedFeatures.toArray( new Feature[addedFeatures.size()] );

    final Feature discModelFeature = discretisationModel.getWrappedFeature();
    final GMLWorkspace workspace = discModelFeature.getWorkspace();
    workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, discModelFeature, addedFeatureArray, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );

    return nodesByStation;
  }

  protected static SortedMap<BigDecimal, WspmProfile> doImportNetwork( final TuhhReach reach, final IRiverProfileNetworkCollection networkCollection, final List<Feature> addedFeatures ) throws Exception
  {
    final SortedMap<BigDecimal, WspmProfile> result = new TreeMap<BigDecimal, WspmProfile>();

    final IRiverProfileNetwork network = networkCollection.addNew( IRiverProfileNetwork.QNAME );
    final Feature networkFeature = network.getWrappedFeature();
    addedFeatures.add( networkFeature );

    /* Set user friendly name and descrption */
    final URL reachContext = reach.getFeature().getWorkspace().getContext();
    final String reachPath = reachContext == null ? "-" : reachContext.toExternalForm(); //$NON-NLS-1$
    final String desc = String.format( Messages.getString("ImportWspmWizard.21"), reach.getWaterBody().getName(), reach.getName(), DF.format( new Date() ), reachPath ); //$NON-NLS-1$
    network.setName( reach.getName() );
    network.setDescription( desc );

    /* Clone all profiles into network */
    final TuhhReachProfileSegment[] reachProfileSegments = reach.getReachProfileSegments();
    for( final TuhhReachProfileSegment segment : reachProfileSegments )
    {
      final WspmProfile profileMember = segment.getProfileMember();
      final BigDecimal station = segment.getStation();

      final IRelationType wspmRelation = (IRelationType) networkFeature.getFeatureType().getProperty( IRiverProfileNetwork.QNAME_PROP_RIVER_PROFILE );
      final Feature clonedProfileFeature = FeatureHelper.cloneFeature( networkFeature, wspmRelation, profileMember.getFeature() );
      addedFeatures.add( clonedProfileFeature );

      result.put( station, new WspmProfile( clonedProfileFeature ) );
    }

    final GMLWorkspace workspace = network.getWrappedFeature().getWorkspace();
    workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, network.getWrappedFeature(), networkFeature, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );

    return result;
  }

  public Feature[] getTerrainModelAdds( )
  {
    return m_terrainModelAdds.toArray( new Feature[m_terrainModelAdds.size()] );
  }

  public Feature[] getDiscretisationModelAdds( )
  {
    return m_discModelAdds.toArray( new Feature[m_discModelAdds.size()] );
  }
}
