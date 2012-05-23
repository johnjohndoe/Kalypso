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
package org.kalypso.ui.rrm.internal.results.view.tree.strategies;

import java.net.MalformedURLException;
import java.net.URL;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.model.channels.Channel;
import org.kalypso.model.hydrology.binding.model.channels.StorageChannel;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.UIRrmImages.DESCRIPTORS;
import org.kalypso.ui.rrm.internal.results.view.base.HydrologyResultReference;
import org.kalypso.ui.rrm.internal.results.view.base.KalypsoHydrologyResults.RRM_RESULT;
import org.kalypso.ui.rrm.internal.results.view.tree.HydrologyCalculationFoldersCollector;
import org.kalypso.ui.rrm.internal.results.view.tree.handlers.HydrologyCalculationCaseGroupUiHandler;
import org.kalypso.ui.rrm.internal.results.view.tree.handlers.HydrologyGroupUiHandler;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeStrategy;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNode;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNodeModel;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.IFeatureBindingCollectionVisitor;

/**
 * @author Dirk Kuch
 */
public class NaModelStrategy implements ITreeNodeStrategy
{

  private final RrmScenario m_scenario;

  public NaModelStrategy( final RrmScenario scenario )
  {
    m_scenario = scenario;
  }

  @Override
  public TreeNode buildNodes( final TreeNodeModel model )
  {
    final TreeNode virtualRootNode = new TreeNode( model, null, null, null );

    try
    {

      final IFolder folderSimulations = m_scenario.getSimulationsFolder();

      folderSimulations.accept( new IResourceVisitor()
      {
        @Override
        public boolean visit( final IResource resource )
        {
          if( !(resource instanceof IFolder) )
            return true;
          else if( isBaseFolder( (IFolder) resource ) )
            return true;
          else if( isCalculationCaseFolder( (IFolder) resource ) )
          {
            if( StringUtils.startsWithIgnoreCase( resource.getName(), "tmp" ) ) //$NON-NLS-1$
              return true;

            virtualRootNode.addChild( buildCalculationCaseNodes( virtualRootNode, new RrmSimulation( (IFolder) resource ) ) );
            return true;
          }

          return true;
        }

        private boolean isBaseFolder( final IFolder folder )
        {
          return folder.equals( folderSimulations );
        }

        private boolean isCalculationCaseFolder( final IFolder folder )
        {
          return folder.getParent().equals( folderSimulations );
        }

      }, 1, false );
    }
    catch( final Exception ex )
    {
      ex.printStackTrace();
    }

    return virtualRootNode;
  }

  protected TreeNode buildCalculationCaseNodes( final TreeNode parent, final RrmSimulation simulation )
  {
    final TreeNode calcCase = new TreeNode( parent, new HydrologyCalculationCaseGroupUiHandler( simulation ), simulation );
    try
    {
      final IFile modelGml = simulation.getModelGml();
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( modelGml );
      final NaModell model = (NaModell) workspace.getRootFeature();

      // FIXME english project template folder names?!?
      final HydrologyCalculationFoldersCollector visitor = new HydrologyCalculationFoldersCollector( simulation ); //$NON-NLS-1$
      simulation.getResultsFolder().accept( visitor, 1, false );

      final IFolder[] caluculationResultsFolders = visitor.getFolders();

      for( final IFolder calculationResultFolder : caluculationResultsFolders )
      {
        final TreeNode calculationResultNode = new TreeNode( calcCase, new HydrologyCalculationCaseGroupUiHandler( simulation, calculationResultFolder ), calculationResultFolder );

        calculationResultNode.addChild( addNodes( model.getNodes(), calculationResultNode, simulation, calculationResultFolder ) );
        calculationResultNode.addChild( addCatchments( model.getCatchments(), calculationResultNode, simulation, calculationResultFolder ) );
        calculationResultNode.addChild( addStorageChannels( model.getChannels(), calculationResultNode, simulation, calculationResultFolder ) );

        calcCase.addChild( calculationResultNode );
      }

    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    return calcCase;
  }

  private TreeNode addStorageChannels( final IFeatureBindingCollection<Channel> channels, final TreeNode parent, final RrmSimulation simulation, final IFolder calculationFolder )
  {

    final TreeNode base = new TreeNode( parent, new HydrologyGroupUiHandler( simulation, "Speicherstr‰nge", DESCRIPTORS.STORAGE_CHANNEL ), RRM_RESULT.class );
    channels.accept( new IFeatureBindingCollectionVisitor<Channel>()
    {
      @Override
      public void visit( final Channel channel )
      {
        if( channel instanceof StorageChannel )
        {
          final ParameterSetBuilder builder = new ParameterSetBuilder( simulation, channel );
          builder.init( base, UIRrmImages.DESCRIPTORS.STORAGE_CHANNEL, UIRrmImages.DESCRIPTORS.EMPTY_STORAGE_CHANNEL );

          builder.doAddNode( new HydrologyResultReference( simulation, calculationFolder, channel, RRM_RESULT.storageFuellvolumen ) );
          builder.doAddNode( new HydrologyResultReference( simulation, calculationFolder, channel, RRM_RESULT.storageSpeicherUeberlauf ) );

          try
          {
            final StorageChannel storage = (StorageChannel) channel;
            final URL context = storage.getWorkspace().getContext();

            builder.doAddNode( new HydrologyResultReference( simulation, context, storage, storage.getSeaEvaporationTimeseriesLink(), RRM_RESULT.inputEvaporation ) );
          }
          catch( final MalformedURLException e )
          {
            e.printStackTrace();
          }
        }
      }
    } );

    return base;
  }

  private TreeNode addCatchments( final IFeatureBindingCollection<Catchment> catchments, final TreeNode parent, final RrmSimulation simulation, final IFolder calculationFolder )
  {
    final TreeNode base = new TreeNode( parent, new HydrologyGroupUiHandler( simulation, "Teilgebiete", DESCRIPTORS.CATCHMENT ), RRM_RESULT.class );

    catchments.accept( new IFeatureBindingCollectionVisitor<Catchment>()
    {
      @Override
      public void visit( final Catchment catchment )
      {
        if( catchment.isGenerateResults() )
        {
          final ParameterSetBuilder builder = new ParameterSetBuilder( simulation, catchment );
          builder.init( base, UIRrmImages.DESCRIPTORS.CATCHMENT, UIRrmImages.DESCRIPTORS.EMPTY_CATCHMENT );

          builder.doAddNode( new HydrologyResultReference( simulation, calculationFolder, catchment, RRM_RESULT.catchmentTemperature ) );
          builder.doAddNode( new HydrologyResultReference( simulation, calculationFolder, catchment, RRM_RESULT.catchmentNiederschlag ) );
          builder.doAddNode( new HydrologyResultReference( simulation, calculationFolder, catchment, RRM_RESULT.catchmentSchneehoehe ) );
          builder.doAddNode( new HydrologyResultReference( simulation, calculationFolder, catchment, RRM_RESULT.catchmentGesamtTeilgebietsQ ) );
          builder.doAddNode( new HydrologyResultReference( simulation, calculationFolder, catchment, RRM_RESULT.catchmentOberflaechenQNatuerlich ) );
          builder.doAddNode( new HydrologyResultReference( simulation, calculationFolder, catchment, RRM_RESULT.catchmentOberflaechenQVersiegelt ) );
          builder.doAddNode( new HydrologyResultReference( simulation, calculationFolder, catchment, RRM_RESULT.catchmentInterflow ) );
          builder.doAddNode( new HydrologyResultReference( simulation, calculationFolder, catchment, RRM_RESULT.catchmentBasisQ ) );
          builder.doAddNode( new HydrologyResultReference( simulation, calculationFolder, catchment, RRM_RESULT.catchmentGrundwasserQ ) );
          builder.doAddNode( new HydrologyResultReference( simulation, calculationFolder, catchment, RRM_RESULT.catchmentGrundwasserstand ) );
          builder.doAddNode( new HydrologyResultReference( simulation, calculationFolder, catchment, RRM_RESULT.catchmentEvapotranspiration ) );

          try
          {
            final URL context = catchment.getWorkspace().getContext();

            builder.doAddNode( new HydrologyResultReference( simulation, context, catchment, catchment.getEvaporationLink(), RRM_RESULT.inputEvaporation ) );
            builder.doAddNode( new HydrologyResultReference( simulation, context, catchment, catchment.getTemperatureLink(), RRM_RESULT.inputTemperature ) );
          }
          catch( final MalformedURLException e )
          {
            e.printStackTrace();
          }
        }

      }
    } );

    return base;
  }

  private TreeNode addNodes( final IFeatureBindingCollection<Node> nodes, final TreeNode parent, final RrmSimulation simulation, final IFolder calculationFolder )
  {
    final TreeNode base = new TreeNode( parent, new HydrologyGroupUiHandler( simulation, "Knoten", DESCRIPTORS.NA_NODE ), RRM_RESULT.class );
    nodes.accept( new IFeatureBindingCollectionVisitor<Node>()
    {
      @Override
      public void visit( final Node node )
      {
        if( node.isGenerateResults() )
        {
          final ParameterSetBuilder builder = new ParameterSetBuilder( simulation, node );
          builder.init( base, UIRrmImages.DESCRIPTORS.NA_NODE, UIRrmImages.DESCRIPTORS.EMPTY_NA_NODE );

          builder.doAddNode( new HydrologyResultReference( simulation, calculationFolder, node, RRM_RESULT.nodeGesamtknotenAbfluss ) );

          try
          {
            final URL context = node.getWorkspace().getContext();
            builder.doAddNode( new HydrologyResultReference( simulation, context, node, node.getZuflussLink(), RRM_RESULT.inputInflow ) );
          }
          catch( final MalformedURLException e )
          {
            e.printStackTrace();
          }
        }
      }
    } );

    return base;
  }

}
