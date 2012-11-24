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
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

import javax.xml.namespace.QName;

import org.apache.commons.lang3.ObjectUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IPageChangeProvider;
import org.eclipse.jface.dialogs.IPageChangedListener;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.PageChangedEvent;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.DiscretisationModelUtils;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.FlowRelationUtilitites;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBridgeFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IFlowRelation1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IWeirFlowRelation;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.Add1DElementFromNodeCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.AddNodeCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeDiscretiationModelCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeleteCmdFactory;
import org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelationshipCalcOperation;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetwork;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetworkCollection;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.util.ProfileUtil;
import org.kalypso.model.wspm.core.profil.visitors.ProfileVisitors;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.schema.gml.ProfileCacherFeaturePropertyFunction;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhSegmentStationComparator;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhStationComparator;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingWehr;
import org.kalypso.model.wspm.tuhh.schema.gml.QIntervallResult;
import org.kalypso.model.wspm.tuhh.schema.gml.QIntervallResultCollection;
import org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeProcessor;
import org.kalypso.model.wspm.tuhh.schema.simulation.QIntervalIndex;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.phenomenon.IPhenomenon;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.wizard.gml.GmlFileImportData;
import org.kalypso.ui.wizard.gml.GmlFileImportPage;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * A wizard to import WSPM-Models into a 1D2D Model.
 * 
 * @author Gernot Belger
 */
public class ImportWspmWizard extends Wizard
{
  private static final DateFormat DF = DateFormat.getDateTimeInstance( DateFormat.MEDIUM, DateFormat.SHORT );

  private final List<Feature> m_discModelAdds = new ArrayList<>();

  private ImportWspmWizardPage m_importPage;

  private final IRiverProfileNetworkCollection m_networkModel;

  private final IFEDiscretisationModel1d2d m_discretisationModel;

  private final IFlowRelationshipModel m_flowRelationCollection;

  private final IPageChangedListener m_pageChangeListener = new IPageChangedListener()
  {
    @Override
    public void pageChanged( final PageChangedEvent event )
    {
      handlePageChanged( event );
    }
  };

  private final GmlFileImportData m_gmlImportData = new GmlFileImportData();

  public ImportWspmWizard( final IFEDiscretisationModel1d2d discretisationModel, final IRiverProfileNetworkCollection networkModel, final IFlowRelationshipModel flowRelationCollection )
  {
    m_discretisationModel = discretisationModel;
    m_networkModel = networkModel;
    m_flowRelationCollection = flowRelationCollection;

    setWindowTitle( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.ImportWspmWizard.0" ) ); //$NON-NLS-1$

    setNeedsProgressMonitor( true );
  }

  @Override
  public void addPages( )
  {
    // FIXME: ugly; directly present user with all available WPSMProject instead of let him select a model.gml
    // KMUpdate stuff how to do this

    /* Only show calculation node */
    final GMLXPath projectPath = new GMLXPath( TuhhWspmProject.QN_TYPE );
    final GMLXPath calculationsPath = new GMLXPath( projectPath, TuhhWspmProject.QNAME_PROP_CALC_MEMBER );
    m_gmlImportData.setRootPath( calculationsPath );

    /* Choose wspm-reach */
    m_gmlImportData.setValidQNames( new QName[] { TuhhCalculation.QN_TUHH_CALC_REIB_CONST } );
    m_gmlImportData.setValidKind( true, false );

    final GmlFileImportPage wspmGmlPage = new GmlFileImportPage( "chooseWspmGml", Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.ImportWspmWizard.2" ), m_gmlImportData ); //$NON-NLS-1$ //$NON-NLS-2$
    wspmGmlPage.setDescription( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.ImportWspmWizard.3" ) ); //$NON-NLS-1$

    addPage( wspmGmlPage );

    m_importPage = new ImportWspmWizardPage( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.ImportWspmWizard.1" ) ); //$NON-NLS-1$

    addPage( m_importPage );
  }

  @Override
  public void setContainer( final IWizardContainer wizardContainer )
  {
    final IWizardContainer currentContainer = getContainer();
    if( currentContainer instanceof IPageChangeProvider )
      ((IPageChangeProvider)currentContainer).removePageChangedListener( m_pageChangeListener );

    super.setContainer( wizardContainer );

    if( wizardContainer instanceof IPageChangeProvider )
      ((IPageChangeProvider)wizardContainer).addPageChangedListener( m_pageChangeListener );
  }

  @Override
  public boolean performFinish( )
  {
    // FIXME: move into separate class

    final TuhhCalculation calculation = m_importPage.getCalculation();
    final TuhhReach reach = calculation.getReach();
    final TuhhReachProfileSegment[] segments = m_importPage.getReachProfileSegments();

    final IRiverProfileNetworkCollection profNetworkColl = m_networkModel;
    final IFEDiscretisationModel1d2d discModel = m_discretisationModel;
    final IFlowRelationshipModel flowRelModel = m_flowRelationCollection;

    final List<Feature> discModelAdds = m_discModelAdds;

    // TODO: do not every time create a new network: if an re-import happens
    // - find out if same network already exists... (how?)
    // - ask user to overwrite or not

    /* Set user friendly name and description */
    final IRiverProfileNetwork foundNetwork = findExistingNetwork( profNetworkColl, reach );
    final IRiverProfileNetwork existingNetwork;
    if( foundNetwork == null )
      existingNetwork = null;
    else
    {
      final String msg = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.ImportWspmWizard.11", foundNetwork.getName() ); //$NON-NLS-1$
      final MessageDialog messageDialog = new MessageDialog( getShell(), getWindowTitle(), null, msg, MessageDialog.QUESTION, new String[] { IDialogConstants.YES_LABEL, IDialogConstants.NO_LABEL,
          IDialogConstants.CANCEL_LABEL }, 1 );
      final int open = messageDialog.open();
      System.out.println( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.ImportWspmWizard.12" ) + open ); //$NON-NLS-1$
      if( open == 2 || open == -1 )
        return false;

      if( open == 0 )
        existingNetwork = foundNetwork;
      else
        existingNetwork = null; // do create a new network
    }

    /* Do import */
    final ICoreRunnableWithProgress op = new ICoreRunnableWithProgress()
    {
      @Override
      public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
      {
        monitor.beginTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.ImportWspmWizard.4" ), 3 ); //$NON-NLS-1$

        try
        {
          /* Check if its the right calculation and if results are present */
          if( calculation.getCalcMode() != TuhhCalculation.MODE.REIB_KONST )
            return new Status( IStatus.WARNING, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.ImportWspmWizard.5" ) ); //$NON-NLS-1$

          try
          {
            /* Import reach into profile collection */
            monitor.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.ImportWspmWizard.6" ) ); //$NON-NLS-1$

            final SortedMap<BigDecimal, IProfileFeature> profilesByStation = doImportNetwork( reach, segments, profNetworkColl, existingNetwork );
            monitor.worked( 1 );

            /* Create 1D-Network */
            monitor.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.ImportWspmWizard.7" ) ); //$NON-NLS-1$
            final SortedMap<BigDecimal, IFE1D2DNode> elementsByStation = doCreate1DNet( reach, segments, discModel, discModelAdds );
            monitor.worked( 1 );

            /* Create 1D-Network parameters (flow relations) */
            monitor.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.ImportWspmWizard.8" ) ); //$NON-NLS-1$
            final IStatus status = doReadResults( calculation, segments, elementsByStation, flowRelModel, profilesByStation );
            monitor.worked( 1 );

            return status;
          }
          catch( final Exception e )
          {
            e.printStackTrace();
            return StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.ImportWspmWizard.9" ) ); //$NON-NLS-1$
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
    ErrorDialog.openError( getShell(), getWindowTitle(), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.ImportWspmWizard.10" ), status ); //$NON-NLS-1$

    return !status.matches( IStatus.ERROR );
  }

  /**
   * Searches an already imported profile-network for the given reach.<br>
   * REMARK: at the moment, we just search for a network with the same name as the reach... is there another criterion?
   * 
   * @return <code>null</code>, if none was found.
   */
  private IRiverProfileNetwork findExistingNetwork( final IRiverProfileNetworkCollection profNetworkColl, final TuhhReach reach )
  {
    final String reachName = reach.getName();
    for( final IRiverProfileNetwork riverProfileNetwork : profNetworkColl.getRiverProfileNetworks() )
    {
      final String networkName = riverProfileNetwork.getName();
      if( ObjectUtils.equals( reachName, networkName ) )
        return riverProfileNetwork;
    }

    return null;
  }

  /**
   * Reads a REIB_CONST result and creates polynomial and building parameters (aka 'flow-relations') from it.
   * 
   * @param elements
   *          by station Must be sorted in the order of the flow direction
   */
  protected static IStatus doReadResults( final TuhhCalculation calculation, final TuhhReachProfileSegment[] segments, final SortedMap<BigDecimal, IFE1D2DNode> elementsByStation, final IFlowRelationshipModel flowRelModel, final SortedMap<BigDecimal, IProfileFeature> profilesByStation ) throws MalformedURLException, Exception
  {
    try
    {
      final Set<BigDecimal> allowedStations = new HashSet<>();
      for( final TuhhReachProfileSegment segment : segments )
        allowedStations.add( segment.getStation() );

      final GMLWorkspace calcWorkspace = calculation.getWorkspace();
      final URL calcContext = calcWorkspace.getContext();
      final URL qresultsUrl = new URL( calcContext, "Ergebnisse/" + calculation.getName() + "/_aktuell/Daten/qIntervallResults.gml" ); //$NON-NLS-1$ //$NON-NLS-2$
      final GMLWorkspace qresultsWorkspace = GmlSerializer.createGMLWorkspace( qresultsUrl, calcWorkspace.getFeatureProviderFactory() );
      final QIntervallResultCollection qResultCollection = (QIntervallResultCollection)qresultsWorkspace.getRootFeature();

      final Feature flowRelParentFeature = flowRelModel;
      final GMLWorkspace flowRelworkspace = flowRelParentFeature.getWorkspace();

      final IFeatureBindingCollection<QIntervallResult> resultList = qResultCollection.getQIntervalls();

      final List<Feature> addedFeatures = new ArrayList<>();
      for( final QIntervallResult qresult : resultList )
      {
        final BigDecimal station = qresult.getStation();

        // Only handle results of chosen segments
        if( !allowedStations.contains( station ) )
          continue;

        // get corresponding 1d-element
        final IFE1D2DNode node = QIntervalIndex.forStation( elementsByStation, station );

        final IFlowRelation1D flowRel;

        /* Do we have a building? */
        final IObservation<TupleResult> buildingObs = qresult.getBuildingObservation( false );
        if( node == null )
        {
          final IStatus status = new Status( IStatus.WARNING, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.ImportWspmWizard.14", station ) ); //$NON-NLS-1$
          KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
          flowRel = null;
        }
        else if( buildingObs != null )
        {
          // REMARK: it is important that elementsByStation is sorted in upstream direction
          final IFE1D2DNode downStreamNode = PolynomeProcessor.forStationAdjacent( elementsByStation, station, false );
          final IFE1D2DNode upStreamNode = PolynomeProcessor.forStationAdjacent( elementsByStation, station, true );
          if( downStreamNode == null || upStreamNode == null )
          {
            throw new CoreException( new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.ImportWspmWizard.17" ), null ) ); //$NON-NLS-1$
          }
          else
            flowRel = addBuilding( flowRelModel, node, qresult, downStreamNode, upStreamNode );
        }
        else
        {
          flowRel = FlowRelationUtilitites.addTeschke( flowRelModel, node, qresult, profilesByStation );
        }

        if( flowRel != null )
        {
          flowRel.setCalculation( calculation );

          /* Set common properties and add to list of freshly generated features. */
          flowRel.setName( qresult.getName() );
          flowRel.setDescription( qresult.getDescription() );

          addedFeatures.add( flowRel );
        }
      }

      final Feature[] addedFeatureArray = addedFeatures.toArray( new Feature[addedFeatures.size()] );
      flowRelworkspace.fireModellEvent( new FeatureStructureChangeModellEvent( flowRelworkspace, flowRelParentFeature, addedFeatureArray, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
    }
    catch( final FileNotFoundException fnfe )
    {
      fnfe.printStackTrace();

      /* Results are not available, just inform user */
      return new Status( IStatus.WARNING, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.ImportWspmWizard.15" ) ); //$NON-NLS-1$
    }

    return Status.OK_STATUS;
  }

  private static IBuildingFlowRelation addBuilding( final IFlowRelationshipModel flowRelModel, final IFE1D2DNode node, final QIntervallResult qresult, final IFE1D2DNode downStreamNode, final IFE1D2DNode upStreamNode ) throws CoreException
  {
    final IObservation<TupleResult> qresultBuildingObs = qresult.getBuildingObservation( false );
    final IPhenomenon buildingPhenomenon = qresultBuildingObs.getPhenomenon();
    final String buildingId = buildingPhenomenon.getID();
    final QName buildingQName;
    if( BuildingWehr.ID.equals( buildingId ) )
      buildingQName = IWeirFlowRelation.QNAME;
    else if( BuildingBruecke.ID.equals( buildingId ) )
      buildingQName = IBridgeFlowRelation.QNAME;
    else
      throw new IllegalStateException();

    /* Derive direction from flow-direction of adjacent results */
    final GM_Position downStreamPosition = downStreamNode.getPoint().getPosition();
    final GM_Position upStreamPosition = upStreamNode.getPoint().getPosition();
    final GM_Position oldBuildingPos = GeometryUtilities.createGM_PositionAtCenter( downStreamPosition, upStreamPosition );

    // check if there is already a relation on that position; if it is, just replace it with the new one
    final IFlowRelationship[] existingFlowRels = flowRelModel.findFlowrelationships( oldBuildingPos, FlowRelationUtilitites.SEARCH_DISTANCE );
    IBuildingFlowRelation existingRelation = null;
    for( final IFlowRelationship existingFlowrel : existingFlowRels )
    {
      if( existingFlowrel instanceof IBuildingFlowRelation )
      {
        existingRelation = (IBuildingFlowRelation)existingFlowrel;
        break;
      }
    }

    final IBuildingFlowRelation buildingRelation;
    if( existingRelation == null )
      buildingRelation = flowRelModel.getFlowRelationsShips().addNew( buildingQName, IBuildingFlowRelation.class );
    else
      buildingRelation = existingRelation;

    final GM_Point buildingPos = replaceNodeWithElement( node );
    if( buildingPos != null )
    {
      if( existingRelation == null )
      {
        // no existing realtion -> add the new one!

        // throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, "Should not happen: could not create
        // element for non-existing bulding", null ) );
      }
      buildingRelation.setPosition( buildingPos );
    }

    /* Direction goes from upstream to downstream */
    final double degrees = GeometryUtilities.directionFromPositions( upStreamPosition, downStreamPosition );
    buildingRelation.setDirection( (int)degrees );

    FlowRelationshipCalcOperation.copyBuildingData( buildingRelation, qresult );

    return buildingRelation;
  }

  private static GM_Point replaceNodeWithElement( final IFE1D2DNode node ) throws CoreException
  {
    final GMLWorkspace workspace = node.getWorkspace();
    final IFEDiscretisationModel1d2d model1d2d = DiscretisationModelUtils.modelForItem( node );

    final IFE1D2DElement[] elements = node.getAdjacentElements();
    if( elements.length != 2 )
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.ImportWspmWizard.16" ) ) ); //$NON-NLS-1$

    /* Find upstream and downstream neighbours */
    final IFE1D2DNode neighbour0 = DiscretisationModelUtils.findOtherNode( node, elements[0] );
    final IFE1D2DNode neighbour1 = DiscretisationModelUtils.findOtherNode( node, elements[1] );

    try
    {
      /* delete elements (and associated edges) */
      final Feature feature0 = elements[0];
      final Feature feature1 = elements[1];
      final EasyFeatureWrapper[] toDelete = new EasyFeatureWrapper[] { new EasyFeatureWrapper( null, feature0 ), new EasyFeatureWrapper( null, feature1 ) };

      final ChangeDiscretiationModelCommand elementDeleteCmd = new ChangeDiscretiationModelCommand( workspace, model1d2d );
      DeleteCmdFactory.createDeleteCmd( model1d2d, toDelete, elementDeleteCmd );
      elementDeleteCmd.process();

      /* create new edge between neighbor nodes */
      // FIXME: use Create1dElementCommand instead?
      final ChangeDiscretiationModelCommand elementAddCmd = new ChangeDiscretiationModelCommand( workspace, model1d2d );
      final AddNodeCommand addNode0 = new AddNodeCommand( model1d2d, neighbour0.getPoint(), 0.0 );
      final AddNodeCommand addNode1 = new AddNodeCommand( model1d2d, neighbour1.getPoint(), 0.0 );
      final Add1DElementFromNodeCmd eleCmd = new Add1DElementFromNodeCmd( model1d2d, new AddNodeCommand[] { addNode0, addNode1 } );
      elementAddCmd.addCommand( addNode0 );
      elementAddCmd.addCommand( addNode1 );
      elementAddCmd.addCommand( eleCmd );

      elementAddCmd.process();

      // REMARK: the element might be null, if the building already exist (...), in that case we just return null
      final IElement1D addedElement = eleCmd.getAddedElement();
      if( addedElement == null )
        return null;

      /* Return position of building */
      final GM_Position position = FlowRelationUtilitites.getFlowPositionFromElement( addedElement );

      return GeometryFactory.createGM_Point( position, node.getPoint().getCoordinateSystem() );
    }
    catch( final Exception e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }

  }

  protected static SortedMap<BigDecimal, IFE1D2DNode> doCreate1DNet( final TuhhReach reach, final TuhhReachProfileSegment[] segments, final IFEDiscretisationModel1d2d discretisationModel, final List<Feature> addedFeatures ) throws Exception
  {
    /* sort reach by station and direction */
    final WspmWaterBody waterBody = reach.getWaterBody();
    final boolean isDirectionUpstreams = waterBody.isDirectionUpstreams();
    Arrays.sort( segments, new TuhhSegmentStationComparator( isDirectionUpstreams ) );

    /* Get some common variables */
    /* add complex-element to model: Automatically create a calculation unit 1d */

    /*
     * NO, this is not the right way to do it! During the creation of calculation unit, control model for that unit
     * should be created as well. Use CreateCalculationUnitCmd
     */

    // final ICalculationUnit1D calculationUnit1D = discretisationModel.getComplexElements().addNew(
    // Kalypso1D2DSchemaConstants.WB1D2D_F_CALC_UNIT_1D, ICalculationUnit1D.class );
    // addedFeatures.add( calculationUnit1D.getWrappedFeature() );
    // final String calcUnitName = String.format( "WSPM-Import: %s - %s", waterBody.getName(), reach.getName() );
    // calculationUnit1D.setName( calcUnitName );
    // calculationUnit1D.setDescription( "Dieses Teilmodell wurde durch den Import automatisch angelegt." );
    /* add nodes to model */
    final List<IFE1D2DEdge> edgeList = new ArrayList<>( segments.length - 1 );
    final List<IFE1D2DNode> nodesList = new ArrayList<>();

    /* add elements to model and sort by station in flow direction */
    // IMPORTANT: the right ordering (in flow direction) is later used by the building parameter stuff, so do not change
    // it!
    final SortedMap<BigDecimal, IFE1D2DNode> nodesByStation = new TreeMap<>( new TuhhStationComparator( isDirectionUpstreams ) );
    IFE1D2DNode lastNode = null;
    for( final TuhhReachProfileSegment segment : segments )
    {
      final IProfileFeature profileMember = segment.getProfileMember();
      if( profileMember == null )
        continue;

      final String crs = profileMember.getSrsName();
      final double station = profileMember.getStation();

      /* find sohlpunkt */
      final IProfile profil = profileMember.getProfile();

      final IProfileRecord sohlPoint = ProfileVisitors.findMinimum( profil, IWspmConstants.POINT_PROPERTY_HOEHE );
      final GM_Point point = ProfileCacherFeaturePropertyFunction.convertPoint( profil, sohlPoint, crs );

      // if there is already a node, do not create it again
      final IFE1D2DNode existingNode = discretisationModel.findNode( point, FlowRelationUtilitites.SEARCH_DISTANCE );

      final IFE1D2DNode node;
      if( existingNode == null )
      {
        /* add new node */
        if( point == null )
          throw new CoreException( new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.ImportWspmWizard.19", station ) ) ); //$NON-NLS-1$

        node = discretisationModel.createNode( point );
        addedFeatures.add( node );

        final String desc = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.ImportWspmWizard.18" + profileMember.getDescription() ); //$NON-NLS-1$
        node.setDescription( desc );
      }
      else
      {
        node = existingNode;
      }

      if( lastNode != null )
      {
        // if there is already an element between those two nodes, do not create it again
        // for example, it is possible that both last nodes are existing nodes so element was there
        boolean found = false;
        final GM_Envelope reqEnvelope = GeometryUtilities.grabEnvelopeFromDistance( point, FlowRelationUtilitites.SEARCH_DISTANCE );
        final List<IFE1D2DElement> list = discretisationModel.queryElements( reqEnvelope, null );
        for( final IFE1D2DElement element : list )
        {
          if( element instanceof IElement1D )
          {
            final IElement1D el1d = (IElement1D)element;
            final IFE1D2DEdge edge = el1d.getEdge();
            final IFE1D2DNode[] nodes = el1d.getNodes();
            if( nodes == null )
            {
              System.out.println( element.getId() );
              continue;
            }

            if( edge.containsNode( node ) && edge.containsNode( lastNode ) )
            {
              found = true;
              break;
            }
          }
        }

        if( !found && (nodesList.size() == 0 || !(node.getPoint().getX() == nodesList.get( 0 ).getPoint().getX() && node.getPoint().getY() == nodesList.get( 0 ).getPoint().getY())) )
        {
          /* Create an edge between lastNode and node */
          final IFE1D2DEdge edge = discretisationModel.createEdge( lastNode, node );
          addedFeatures.add( edge );
          edgeList.add( edge );

          /* Create corresponding element */
          final IElement1D element1d = discretisationModel.createElement1D( edge );
          addedFeatures.add( element1d );
        }
      }
      nodesList.add( node );
      lastNode = node;

      final BigDecimal stationDecimal = ProfileUtil.stationToBigDecimal( station );
      nodesByStation.put( stationDecimal, lastNode );
    }

    final Feature[] addedFeatureArray = addedFeatures.toArray( new Feature[addedFeatures.size()] );

    final Feature discModelFeature = discretisationModel;
    final GMLWorkspace workspace = discModelFeature.getWorkspace();
    workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, discModelFeature, addedFeatureArray, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );

    return nodesByStation;
  }

  /**
   * @param existingNetwork
   *          If non-<code>null</code>, this network will be filled (and cleared before) instead of creating a new one.
   */
  protected static SortedMap<BigDecimal, IProfileFeature> doImportNetwork( final TuhhReach reach, final TuhhReachProfileSegment[] segments, final IRiverProfileNetworkCollection networkCollection, final IRiverProfileNetwork existingNetwork ) throws Exception
  {
    final SortedMap<BigDecimal, IProfileFeature> result = new TreeMap<>();

    final IRiverProfileNetwork network;
    if( existingNetwork == null )
      network = networkCollection.getRiverProfileNetworks().addNew( IRiverProfileNetwork.QNAME );
    else
    {
      network = existingNetwork;
      network.getProfiles().clear();
    }

    final Feature networkFeature = network;

    /* Set user friendly name and description */
    final URL reachContext = reach.getWorkspace().getContext();
    final String reachPath = reachContext == null ? "-" : reachContext.toExternalForm(); //$NON-NLS-1$
    final String desc = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.ImportWspmWizard.21", reach.getWaterBody().getName(), reach.getName(), DF.format( new Date() ), reachPath ); //$NON-NLS-1$
    network.setName( reach.getName() );
    network.setDescription( desc );

    /* Clone all profiles into network */
    for( final TuhhReachProfileSegment segment : segments )
    {
      final IProfileFeature profileMember = segment.getProfileMember();

      if( profileMember == null )
        continue;

      final BigDecimal station = segment.getStation();

      final IRelationType wspmRelation = (IRelationType)networkFeature.getFeatureType().getProperty( IRiverProfileNetwork.QNAME_PROP_RIVER_PROFILE );
      final Feature clonedProfileFeature = FeatureHelper.cloneFeature( networkFeature, wspmRelation, profileMember );

      result.put( station, (IProfileFeature)clonedProfileFeature );
    }

    // We fire the add event, even if the network was not added, this should be enough to refresh anything with is
    // related to it...
    final GMLWorkspace workspace = network.getWorkspace();
    workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, network, networkFeature, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );

    return result;
  }

  public Feature[] getDiscretisationModelAdds( )
  {
    return m_discModelAdds.toArray( new Feature[m_discModelAdds.size()] );
  }

  protected void handlePageChanged( final PageChangedEvent event )
  {
    if( event.getSelectedPage() == m_importPage )
    {
      final TuhhCalculation calculation = (TuhhCalculation)m_gmlImportData.getSelectedElement();
      m_importPage.setCalculation( calculation );
    }
  }
}