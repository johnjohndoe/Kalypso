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
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.DiscretisationModelUtils;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IRiverChannel1D;
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
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.Util;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetwork;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetworkCollection;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.schema.gml.ProfileCacherFeaturePropertyFunction;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhSegmentStationComparator;
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
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

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

    setWindowTitle( "Kalypso Wspm Import" );

    setNeedsProgressMonitor( true );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    /* Choose wspm-reach */
    m_wspmGmlPage = new GmlFileImportPage( "chooseWspmGml", "Berechnungsvariante wählen", null );
    m_wspmGmlPage.setDescription( "Bitte wählen Sie eine Berechnungsvariante (konstantes Reibungsgefälle) aus" );
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
        monitor.beginTask( "1D-Modell wird importiert", 3 );

        try
        {
          /* Activate network map */

          /* Prepare input data */
          final TuhhCalculation calculation = new TuhhCalculation( (Feature) wspmSelection.getFirstElement() );

          /* Check if its the right calculation and if results are present */
          if( calculation.getCalcMode() != TuhhCalculation.MODE.REIB_KONST )
            return StatusUtilities.createWarningStatus( "Gewählte Berechnungsvariante muss vom Typ 'Bordvoll ungleichförmig' sein'" );

          final TuhhReach reach = calculation.getReach();

          try
          {
            /* Import reach into profile collection */
            monitor.subTask( " - kopiere Profile..." );
            final SortedMap<BigDecimal, WspmProfile> profilesByStation = doImportNetwork( reach, profNetworkColl, terrainModelAdds );
            monitor.worked( 1 );

            /* Create 1D-Network */
            monitor.subTask( "- erzeuge 1D-Finite-Elemente-Netzwerk..." );
            final SortedMap<BigDecimal, IFE1D2DNode> elementsByStation = doCreate1DNet( reach, discModel, discModelAdds );
            monitor.worked( 1 );

            /* Create 1D-Network parameters (flow relations) */
            monitor.subTask( "- lade stationäre Ergebnisse und erzeuge 1D-Parameter..." );
            final IStatus status = doReadResults( calculation, elementsByStation, flowRelModel, profilesByStation );
            monitor.worked( 1 );

            return status;
          }
          catch( final Exception e )
          {
            return StatusUtilities.statusFromThrowable( e, "Failed to copy profiles" );
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
    ErrorDialog.openError( getShell(), getWindowTitle(), "Probleme beim Import", status );

    return !status.matches( IStatus.ERROR );
  }

  protected static IStatus doReadResults( final TuhhCalculation calculation, final SortedMap<BigDecimal, IFE1D2DNode> elementsByStation, final IFlowRelationshipModel flowRelModel, final SortedMap<BigDecimal, WspmProfile> profilesByStation ) throws MalformedURLException, Exception
  {
    try
    {
      final GMLWorkspace calcWorkspace = calculation.getFeature().getWorkspace();
      final URL calcContext = calcWorkspace.getContext();
      final URL qresultsUrl = new URL( calcContext, "Ergebnisse/" + calculation.getName() + "/_aktuell/Daten/qIntervallResults.gml" );
      final GMLWorkspace qresultsWorkspace = GmlSerializer.createGMLWorkspace( qresultsUrl, calcWorkspace.getFeatureProviderFactory() );
      final QIntervallResultCollection qResultCollection = new QIntervallResultCollection( qresultsWorkspace.getRootFeature() );

      final Feature flowRelParentFeature = flowRelModel.getWrappedFeature();
      final GMLWorkspace flowRelworkspace = flowRelParentFeature.getWorkspace();

      final IFeatureType flowRelFT = flowRelworkspace.getGMLSchema().getFeatureType( ITeschkeFlowRelation.QNAME );
      final IRelationType flowRelObsRelation = (IRelationType) flowRelFT.getProperty( ITeschkeFlowRelation.QNAME_PROP_POINTSOBSERVATION );
      final IRelationType flowRelPolynomeRelation = (IRelationType) flowRelFT.getProperty( ITeschkeFlowRelation.QNAME_PROP_POLYNOMES );

      final List resultList = qResultCollection.getQResultFeatures();

      final List<Feature> addedFeatures = new ArrayList<Feature>();
      for( final Object o : resultList )
      {
        final QIntervallResult qresult = new QIntervallResult( FeatureHelper.getFeature( qresultsWorkspace, o ) );

        final BigDecimal slope = qresult.getSlope();
        final String name = qresult.getName();
        final String description = qresult.getDescription();

        final BigDecimal station = qresult.getStation();

        // get corresponding 1d-element
        final IFE1D2DNode node = (IFE1D2DNode) PolynomeHelper.forStation( elementsByStation, station );

        /* Do we have a building? */
        final IObservation<TupleResult> buildingObs = qresult.getBuildingObservation( false );
        if( buildingObs != null )
        {
          // Create weir relation ord bridge relation
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

          /* copy building parameter from one observation to the other */
          final TupleResult weirResult = buildingObs.getResult();
          final TupleResult result = weirFlowObservation.getResult();
          final Map<String, String> componentMap = new HashMap<String, String>();
          componentMap.put( IWspmTuhhQIntervallConstants.DICT_COMPONENT_RUNOFF, Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE );
          componentMap.put( IWspmTuhhQIntervallConstants.DICT_COMPONENT_WATERLEVEL_DOWNSTREAM, Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL_DOWNSTREAM );
          componentMap.put( IWspmTuhhQIntervallConstants.DICT_COMPONENT_WATERLEVEL_UPSTREAM, Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL_UPSTREAM );
          TupleResultUtilities.copyValues( weirResult, result, componentMap );
          buildingRelation.setBuildingObservation( weirFlowObservation );
        }
        else if( node == null )
        {
          KalypsoModel1D2DPlugin.getDefault().getLog().log( StatusUtilities.createWarningStatus( "No node for result at station " + station ) );
        }
        else
        {
          /* create new flow relation at node position */
          final ITeschkeFlowRelation flowRel = flowRelModel.addNew( ITeschkeFlowRelation.QNAME, ITeschkeFlowRelation.class );
          // final Feature flowRelFeature = flowRelworkspace.createFeature( flowRelParentFeature, flowRelParentRelation,
          // flowRelFT );
          // flowRelworkspace.addFeatureAsComposition( flowRelParentFeature, flowRelParentRelation, -1, flowRelFeature
          // );

          flowRel.setName( name );
          flowRel.setName( description );
          flowRel.setPosition( node.getPoint() );
          flowRel.setStation( station );
          flowRel.setSlope( slope.setScale( 5, BigDecimal.ROUND_HALF_UP ).doubleValue() );

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
              final Feature polynomeFeature = FeatureHelper.getFeature( qresultsWorkspace, object );
              if( polynomeFeature != null )
                FeatureHelper.cloneFeature( flowRel.getWrappedFeature(), flowRelPolynomeRelation, polynomeFeature );
            }
          }

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
      return StatusUtilities.createWarningStatus( "Es liegen keine Ergebnisse für diese Rechenvariante vor. Es wurde nur die 1D-Netzgeometrie erzeugt." );
    }

    return Status.OK_STATUS;
  }

  private static GM_Point replaceNodeWithElement( final IFE1D2DNode node ) throws CoreException
  {
    final GMLWorkspace workspace = node.getWrappedFeature().getWorkspace();
    final IFEDiscretisationModel1d2d model1d2d = DiscretisationModelUtils.modelForItem( node );

    // Lauter TODO-S!
    final IFE1D2DElement[] elements = node.getElements();
    if( elements.length != 2 )
      throw new CoreException( StatusUtilities.createErrorStatus( "Ein Wehr am Ende oder am Anfang eines Berechnungabschnittes kann nicht importiert werden\nFügen Sie wenigstens ein Profil im Ober- bzw. Unterwasser ein." ) );

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

      /* create new edge beetween neighbour nodes */
      final ChangeDiscretiationModelCommand elementAddCmd = new ChangeDiscretiationModelCommand( workspace, model1d2d );
      final AddNodeCommand addNode0 = new AddNodeCommand( model1d2d, neighbour0.getPoint(), 0.0 );
      final AddNodeCommand addNode1 = new AddNodeCommand( model1d2d, neighbour1.getPoint(), 0.0 );
      final Add1DElementFromNodeCmd eleCmd = new Add1DElementFromNodeCmd( model1d2d, new AddNodeCommand[] { addNode0, addNode1 } );
      elementAddCmd.addCommand( addNode0 );
      elementAddCmd.addCommand( addNode1 );
      elementAddCmd.addCommand( eleCmd );
      elementAddCmd.process();

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
    final boolean isDirectionUpstreams = reach.getWaterBody().isDirectionUpstreams();
    final TuhhReachProfileSegment[] segments = reach.getReachProfileSegments();
    Arrays.sort( segments, new TuhhSegmentStationComparator( isDirectionUpstreams ) );

    /* Get some common variables */
    final IFeatureWrapperCollection<IFE1D2DElement> discElements = discretisationModel.getElements();
    final FeatureList discElementsList = discElements.getWrappedList();
    final QName discElementsMemberQName = discElementsList.getParentFeatureTypeProperty().getQName();
    final IFeatureWrapperCollection<IFE1D2DEdge> discEdges = discretisationModel.getEdges();

    /* add complex-element to model */
    // REMARK: The next line does not work because of the substitution problem with the featurelistwrapper
    // final IRiverChannel1D riverChannel = (IRiverChannel1D) discretisationModel.getComplexElements().addNew(
    // IRiverChannel1D.QNAME );
    final IFeatureWrapperCollection<IFE1D2DComplexElement> discComplexElements = discretisationModel.getComplexElements();
    final FeatureList wrappedComplexList = discComplexElements.getWrappedList();
    final Feature riverChannelFeature = Util.createFeatureForListProp( wrappedComplexList, wrappedComplexList.getParentFeatureTypeProperty().getQName(), Kalypso1D2DSchemaConstants.WB1D2D_F_RIVER_CHANNEL1D/* IRiverChannel1D.QNAME */);
    addedFeatures.add( riverChannelFeature );
    final IRiverChannel1D riverChannel = (IRiverChannel1D) riverChannelFeature.getAdapter( IRiverChannel1D.class );

    /* add nodes to model */
    final List<IFE1D2DEdge> edgeList = new ArrayList<IFE1D2DEdge>( segments.length - 1 );

    /* add elements to model */
    final SortedMap<BigDecimal, IFE1D2DNode> nodesByStation = new TreeMap<BigDecimal, IFE1D2DNode>();
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

      node.setName( "" );
      final String desc = String.format( "Importiert aus WPSM Modell\n\n" + profileMember.getDescription() );
      node.setDescription( desc );

      if( point == null )
        throw new CoreException( StatusUtilities.createErrorStatus( "Sohltiefpunkt konnte nicht georeferenziert an Profil Station km: " + station ) );

      node.setPoint( point );

      nodesByStation.put( new BigDecimal( station ), node );

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

        // REMARK: The next line does not work because of the substitution problem with the featurelistwrapper
        // final IElement1D<IFE1D2DComplexElement, IFE1D2DEdge> element1d = (IElement1D<IFE1D2DComplexElement,
        // IFE1D2DEdge>) discElements = discretisationModel.getElements().addNew( IElement1D.QNAME );
        final Feature elementFeature = Util.createFeatureForListProp( discElementsList, discElementsMemberQName, Kalypso1D2DSchemaConstants.WB1D2D_F_ELEMENT1D );
        addedFeatures.add( elementFeature );
        final IElement1D element1d = (IElement1D) elementFeature.getAdapter( IElement1D.class );

        element1d.setEdge( edge );
        // adding element to 1d containers is not necessary since setting the edge to the element already does
        // edge.addContainer( element1d.getGmlID() );

        riverChannel.getElements().add( element1d );
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
    final String reachPath = reachContext == null ? "-" : reachContext.toExternalForm();
    final String desc = String.format( "Importiert aus WSPM-Gewässerstrang: %s - %s\nImportiert am %s aus %s", reach.getWaterBody().getName(), reach.getName(), DF.format( new Date() ), reachPath );
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
