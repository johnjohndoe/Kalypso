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

import java.net.URL;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.model.channels.Channel;
import org.kalypso.model.hydrology.binding.model.channels.StorageChannel;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ui.rrm.internal.results.view.base.HydrologyResultReference;
import org.kalypso.ui.rrm.internal.results.view.base.KalypsoHydrologyResults.CATCHMENT_RESULT_TYPE;
import org.kalypso.ui.rrm.internal.results.view.base.KalypsoHydrologyResults.NODE_RESULT_TYPE;
import org.kalypso.ui.rrm.internal.results.view.base.KalypsoHydrologyResults.STORAGE_RESULT_TYPE;
import org.kalypso.ui.rrm.internal.results.view.tree.HydrologyCalculationFoldersCollector;
import org.kalypso.ui.rrm.internal.results.view.tree.handlers.HydrologyCalculationCaseGroupUiHandler;
import org.kalypso.ui.rrm.internal.results.view.tree.handlers.HydrologyCatchmentParameterUiHandler;
import org.kalypso.ui.rrm.internal.results.view.tree.handlers.HydrologyCatchmentUiHandler;
import org.kalypso.ui.rrm.internal.results.view.tree.handlers.HydrologyCatchmentsGroupUiHandler;
import org.kalypso.ui.rrm.internal.results.view.tree.handlers.HydrologyNodeParameterUiHandler;
import org.kalypso.ui.rrm.internal.results.view.tree.handlers.HydrologyNodeUiHandler;
import org.kalypso.ui.rrm.internal.results.view.tree.handlers.HydrologyNodesGroupUiHandler;
import org.kalypso.ui.rrm.internal.results.view.tree.handlers.HydrologyStorageChannelUiHandler;
import org.kalypso.ui.rrm.internal.results.view.tree.handlers.HydrologyStorageChannelsGroupUiHandler;
import org.kalypso.ui.rrm.internal.results.view.tree.handlers.HydrologyStorageParameterUiHandler;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeStrategy;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNode;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNodeModel;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.IFeatureBindingCollectionVisitor;

/**
 * @author Dirk Kuch
 */
public class NaModelStrategy implements ITreeNodeStrategy
{
  private final NaModell m_model;

  public NaModelStrategy( final NaModell model )
  {
    m_model = model;
  }

  @Override
  public TreeNode buildNodes( final TreeNodeModel model )
  {
    final TreeNode virtualRootNode = new TreeNode( model, null, null, null );

    try
    {
      final URL context = m_model.getWorkspace().getContext();
      final URL urlCalcCases = UrlResolverSingleton.resolveUrl( context, "../Rechenvarianten/" ); //$NON-NLS-1$
      final IFolder folderCalcCases = ResourceUtilities.findFolderFromURL( urlCalcCases );

      folderCalcCases.accept( new IResourceVisitor()
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
            if( StringUtils.startsWithIgnoreCase( resource.getName(), "tmp" ) )
              return true;

            virtualRootNode.addChild( buildCalculationCaseNodes( virtualRootNode, new RrmSimulation( (IFolder) resource ) ) );
            return true;
          }

          return true;
        }

        private boolean isBaseFolder( final IFolder folder )
        {
          return folder.equals( folderCalcCases );
        }

        private boolean isCalculationCaseFolder( final IFolder folder )
        {
          return folder.getParent().equals( folderCalcCases );
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
      // FIXME english project template folder names?!?
      final HydrologyCalculationFoldersCollector visitor = new HydrologyCalculationFoldersCollector( simulation ); //$NON-NLS-1$
      simulation.getResultsFolder().accept( visitor, 1, false );

      final IFolder[] caluculationResultsFolders = visitor.getFolders();

      for( final IFolder calculationResultFolder : caluculationResultsFolders )
      {
        final TreeNode calculationResultNode = new TreeNode( calcCase, new HydrologyCalculationCaseGroupUiHandler( simulation, calculationResultFolder ), calculationResultFolder );

        calculationResultNode.addChild( buildHydrologyNodes( calculationResultNode, calculationResultFolder ) );
        calculationResultNode.addChild( buildHydrologyCatchments( calculationResultNode, calculationResultFolder ) );
        calculationResultNode.addChild( buildHydrologyStorages( calculationResultNode, calculationResultFolder ) );

        calcCase.addChild( calculationResultNode );
      }

    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }

    return calcCase;
  }

  private TreeNode buildHydrologyStorages( final TreeNode parent, final IFolder calculationFolder )
  {
    final IFeatureBindingCollection<Channel> channels = m_model.getChannels();

    final TreeNode base = new TreeNode( parent, new HydrologyStorageChannelsGroupUiHandler(), STORAGE_RESULT_TYPE.class );
    channels.accept( new IFeatureBindingCollectionVisitor<Channel>()
    {
      @Override
      public void visit( final Channel channel )
      {
        if( channel instanceof StorageChannel )
          base.addChild( toTreeNode( base, (StorageChannel) channel, calculationFolder ) );
      }
    } );

    return base;
  }

  protected TreeNode toTreeNode( final TreeNode parent, final StorageChannel channel, final IFolder calculationFolder )
  {
    final TreeNode node = new TreeNode( parent, new HydrologyStorageChannelUiHandler( channel ), channel );
    node.addChild( new TreeNode( node, new HydrologyStorageParameterUiHandler( channel, STORAGE_RESULT_TYPE.eFuellvolumen ), new HydrologyResultReference( calculationFolder, channel, STORAGE_RESULT_TYPE.eFuellvolumen ) ) );
    node.addChild( new TreeNode( node, new HydrologyStorageParameterUiHandler( channel, STORAGE_RESULT_TYPE.eSpeicherUeberlauf ), new HydrologyResultReference( calculationFolder, channel, STORAGE_RESULT_TYPE.eSpeicherUeberlauf ) ) );

    return node;
  }

  private TreeNode buildHydrologyCatchments( final TreeNode parent, final IFolder calculationFolder )
  {
    final IFeatureBindingCollection<Catchment> catchments = m_model.getCatchments();

    final TreeNode base = new TreeNode( parent, new HydrologyCatchmentsGroupUiHandler(), CATCHMENT_RESULT_TYPE.class );
    catchments.accept( new IFeatureBindingCollectionVisitor<Catchment>()
    {
      @Override
      public void visit( final Catchment catchment )
      {
        if( catchment.isGenerateResults() )
          base.addChild( toTreeNode( base, catchment, calculationFolder ) );
      }
    } );

    return base;
  }

  protected TreeNode toTreeNode( final TreeNode parent, final Catchment catchment, final IFolder calculationFolder )
  {
    final TreeNode nodeCatchment = new TreeNode( parent, new HydrologyCatchmentUiHandler( catchment ), catchment );
    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, CATCHMENT_RESULT_TYPE.eTemperature ), new HydrologyResultReference( calculationFolder, catchment, CATCHMENT_RESULT_TYPE.eTemperature ) ) );
    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, CATCHMENT_RESULT_TYPE.eNiederschlag ), new HydrologyResultReference( calculationFolder, catchment, CATCHMENT_RESULT_TYPE.eNiederschlag ) ) );
    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, CATCHMENT_RESULT_TYPE.eSchneehoehe ), new HydrologyResultReference( calculationFolder, catchment, CATCHMENT_RESULT_TYPE.eSchneehoehe ) ) );
    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, CATCHMENT_RESULT_TYPE.eGesamtTeilgebietsQ ), new HydrologyResultReference( calculationFolder, catchment, CATCHMENT_RESULT_TYPE.eGesamtTeilgebietsQ ) ) );
    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, CATCHMENT_RESULT_TYPE.eOberflaechenQNatuerlich ), new HydrologyResultReference( calculationFolder, catchment, CATCHMENT_RESULT_TYPE.eOberflaechenQNatuerlich ) ) );
    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, CATCHMENT_RESULT_TYPE.eOberflaechenQVersiegelt ), new HydrologyResultReference( calculationFolder, catchment, CATCHMENT_RESULT_TYPE.eOberflaechenQVersiegelt ) ) );
    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, CATCHMENT_RESULT_TYPE.eInterflow ), new HydrologyResultReference( calculationFolder, catchment, CATCHMENT_RESULT_TYPE.eInterflow ) ) );
    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, CATCHMENT_RESULT_TYPE.eBasisQ ), new HydrologyResultReference( calculationFolder, catchment, CATCHMENT_RESULT_TYPE.eBasisQ ) ) );
    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, CATCHMENT_RESULT_TYPE.eGrundwasserQ ), new HydrologyResultReference( calculationFolder, catchment, CATCHMENT_RESULT_TYPE.eGrundwasserQ ) ) );
    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, CATCHMENT_RESULT_TYPE.eGrundwasserstand ), new HydrologyResultReference( calculationFolder, catchment, CATCHMENT_RESULT_TYPE.eGrundwasserstand ) ) );
    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, CATCHMENT_RESULT_TYPE.eEvapotranspiration ), new HydrologyResultReference( calculationFolder, catchment, CATCHMENT_RESULT_TYPE.eEvapotranspiration ) ) );

    return nodeCatchment;
  }

  private TreeNode buildHydrologyNodes( final TreeNode parent, final IFolder calculationFolder )
  {
    final IFeatureBindingCollection<Node> nodes = m_model.getNodes();

    final TreeNode base = new TreeNode( parent, new HydrologyNodesGroupUiHandler(), NODE_RESULT_TYPE.class );
    nodes.accept( new IFeatureBindingCollectionVisitor<Node>()
    {
      @Override
      public void visit( final Node node )
      {
        if( node.isGenerateResults() )
        {
          base.addChild( toTreeNode( base, node, calculationFolder ) );
        }
      }
    } );

    return base;
  }

  protected TreeNode toTreeNode( final TreeNode parent, final Node hydrologyNode, final IFolder calculationFolder )
  {
    final TreeNode node = new TreeNode( parent, new HydrologyNodeUiHandler( hydrologyNode ), hydrologyNode );
    node.addChild( new TreeNode( node, new HydrologyNodeParameterUiHandler( hydrologyNode, NODE_RESULT_TYPE.eGesamtknotenAbfluss ), new HydrologyResultReference( calculationFolder, hydrologyNode, NODE_RESULT_TYPE.eGesamtknotenAbfluss ) ) );

    return node;
  }
}
