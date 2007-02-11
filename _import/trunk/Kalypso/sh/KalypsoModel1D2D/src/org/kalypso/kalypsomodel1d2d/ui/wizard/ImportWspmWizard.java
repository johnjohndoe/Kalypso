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

import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

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
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.IRiverChannel1D;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.Util;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetwork;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetworkCollection;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.schema.gml.ProfileCacherFeaturePropertyFunction;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhSegmentStationComparator;
import org.kalypso.ui.wizard.gml.GmlFileImportPage;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

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

  public ImportWspmWizard( final IFEDiscretisationModel1d2d discretisationModel, final IRiverProfileNetworkCollection networkModel )
  {
    m_discretisationModel = discretisationModel;
    m_networkModel = networkModel;

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
    m_wspmGmlPage = new GmlFileImportPage( "chooseWspmGml", "Gew‰sserstrang", null );
    m_wspmGmlPage.setDescription( "Bitte w‰hlen Sie einen Gew‰sserstrang aus" );
    m_wspmGmlPage.setValidQNames( new QName[] { TuhhReach.QNAME_TUHH_REACH } );
    m_wspmGmlPage.setValidKind( true, false );

    /* Choose network collection */
    m_wspmGmlPage.setValidKind( true, false );

    // maybe choose results

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

    final List<Feature> discModelAdds = m_discModelAdds;
    final List<Feature> terrainModelAdds = m_terrainModelAdds;

    /* Do import */
    final ICoreRunnableWithProgress op = new ICoreRunnableWithProgress()
    {
      public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
      {
        monitor.beginTask( "1D-Modell wird importiert", 2 );

        try
        {
          /* Activate network map */

          /* Prepare input data */
          final TuhhReach reach = new TuhhReach( (Feature) wspmSelection.getFirstElement() );

          /* Import reach into profile collection */
          monitor.subTask( " ... kopiere Profile" );
          try
          {
            doImportNetwork( reach, profNetworkColl, terrainModelAdds );
          }
          catch( final Exception e )
          {
            return StatusUtilities.statusFromThrowable( e, "Failed to copy profiles" );
          }
          monitor.worked( 1 );

          /* Create 1D-Network */
          monitor.subTask( "... erzeuge 1D-Finite-Elemente-Netzwerk" );
          doCreate1DNet( reach, discModel, discModelAdds );
          monitor.worked( 1 );

          return Status.OK_STATUS;
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
    ErrorDialog.openError( getShell(), getWindowTitle(), "Profile konnten nicht importiert werden", status );

    return status.isOK();
  }

  protected static void doCreate1DNet( final TuhhReach reach, final IFEDiscretisationModel1d2d discretisationModel, final List<Feature> addedFeatures ) throws Exception
  {
    /* sort reach by station and direction */
    final boolean isDirectionUpstreams = reach.getWaterBody().isDirectionUpstreams();
    final TuhhReachProfileSegment[] segments = reach.getReachProfileSegments();
    Arrays.sort( segments, new TuhhSegmentStationComparator( isDirectionUpstreams ) );

    /* add nodes to model */
    final Map<TuhhReachProfileSegment, IFE1D2DNode> nodes = new LinkedHashMap<TuhhReachProfileSegment, IFE1D2DNode>( segments.length );
    for( final TuhhReachProfileSegment segment : segments )
    {
      final WspmProfile profileMember = segment.getProfileMember();

      /* find sohlpunkt */
      final IProfil profil = profileMember.getProfil();
      final IProfilPoint sohlPoint = ProfilUtil.getMinPoint( profil, IProfilPoint.POINT_PROPERTY.HOEHE );
      final GM_Point point = ProfileCacherFeaturePropertyFunction.convertPoint( sohlPoint );

      /* add node and remember it */
      final IFE1D2DNode node = discretisationModel.getNodes().addNew( Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
      addedFeatures.add( node.getWrappedFeature() );

      node.setName( "" );
      final String desc = String.format( "Importiert aus WPSM Modell\n\n" + profileMember.getDescription() );
      node.setDescription( desc );
      node.setPoint( point );

      nodes.put( segment, node );
    }

    /* add edges to model */
    final Collection<IFE1D2DNode> nodeCollection = nodes.values();
    final IFE1D2DNode[] nodeArray = nodeCollection.toArray( new IFE1D2DNode[nodeCollection.size()] );
    final List<IFE1D2DEdge> edgeList = new ArrayList<IFE1D2DEdge>( nodeArray.length - 1 );
    for( int i = 0; i < nodeArray.length - 1; i++ )
    {
      final IFE1D2DNode node0 = nodeArray[i];
      final IFE1D2DNode node1 = nodeArray[i + 1];

      final IFE1D2DEdge edge = discretisationModel.getEdges().addNew( Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE );
      addedFeatures.add( edge.getWrappedFeature() );

      edge.addNode( node0.getGmlID() );
      edge.addNode( node1.getGmlID() );

      node0.addContainer( edge.getGmlID() );
      node1.addContainer( edge.getGmlID() );

      edge.getWrappedFeature().invalidEnvelope();

      edgeList.add( edge );
    }

    /* add elements to model */
    final List<IElement1D> elements = new ArrayList<IElement1D>();
    for( final IFE1D2DEdge edge : edgeList )
    {
      // REMARK: The next line does not work because of the substitution problem with the featurelistwrapper
      // final IElement1D<IFE1D2DComplexElement, IFE1D2DEdge> element1d = (IElement1D<IFE1D2DComplexElement,
      // IFE1D2DEdge>) discElements = discretisationModel.getElements().addNew( IElement1D.QNAME );

      final IFeatureWrapperCollection<IFE1D2DElement> discElements = discretisationModel.getElements();
      final FeatureList wrappedList = discElements.getWrappedList();
      final Feature feature = Util.createFeatureForListProp( wrappedList, wrappedList.getParentFeatureTypeProperty().getQName(), IElement1D.QNAME );
      addedFeatures.add( feature );
      final IElement1D element1d = (IElement1D) feature.getAdapter( IElement1D.class );

      element1d.setEdge( edge );

      elements.add( element1d );

    }

    /* add complex-element to model */
    // REMARK: The next line does not work because of the substitution problem with the featurelistwrapper
    // final IRiverChannel1D riverChannel = (IRiverChannel1D) discretisationModel.getComplexElements().addNew(
    // IRiverChannel1D.QNAME );
    final IFeatureWrapperCollection<IFE1D2DComplexElement> discComplexElements = discretisationModel.getComplexElements();
    final FeatureList wrappedComplexList = discComplexElements.getWrappedList();
    final Feature feature = Util.createFeatureForListProp( wrappedComplexList, wrappedComplexList.getParentFeatureTypeProperty().getQName(), IRiverChannel1D.QNAME );
    final IRiverChannel1D riverChannel = (IRiverChannel1D) feature.getAdapter( IRiverChannel1D.class );
    addedFeatures.add( riverChannel.getWrappedFeature() );

    for( final IElement1D element1D : elements )
      riverChannel.getElements().add( element1D );

    final Feature[] addedFeatureArray = addedFeatures.toArray( new Feature[addedFeatures.size()] );

    final Feature discModelFeature = discretisationModel.getWrappedFeature();
    final GMLWorkspace workspace = discModelFeature.getWorkspace();
    workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, discModelFeature, addedFeatureArray, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
  }

  protected static void doImportNetwork( final TuhhReach reach, final IRiverProfileNetworkCollection networkCollection, final List<Feature> addedFeatures ) throws Exception
  {
    final IRiverProfileNetwork network = networkCollection.addNew( IRiverProfileNetwork.QNAME );
    final Feature networkFeature = network.getWrappedFeature();
    addedFeatures.add( networkFeature );

    /* Set user friendly name and descrption */
    final URL reachContext = reach.getFeature().getWorkspace().getContext();
    final String reachPath = reachContext == null ? "-" : reachContext.toExternalForm();
    final String desc = String.format( "Importiert aus WSPM-Gew‰sserstrang: %s - %s\nImportiert am %s aus %s", reach.getWaterBody().getName(), reach.getName(), DF.format( new Date() ), reachPath );
    network.setName( reach.getName() );
    network.setDescription( desc );

    /* Clone all profiles into network */
    final TuhhReachProfileSegment[] reachProfileSegments = reach.getReachProfileSegments();
    for( final TuhhReachProfileSegment segment : reachProfileSegments )
    {
      final WspmProfile profileMember = segment.getProfileMember();

      final IRelationType wspmRelation = (IRelationType) networkFeature.getFeatureType().getProperty( IRiverProfileNetwork.QNAME_PROP_RIVER_PROFILE );
      final Feature clonedProfileFeature = FeatureHelper.cloneFeature( networkFeature, wspmRelation, profileMember.getFeature() );
      addedFeatures.add( clonedProfileFeature );
    }

    final GMLWorkspace workspace = network.getWrappedFeature().getWorkspace();
    workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, network.getWrappedFeature(), networkFeature, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
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
