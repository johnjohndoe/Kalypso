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
package org.kalypso.ui.rrm.internal.results.view.tree;

import java.net.URL;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.model.channels.Channel;
import org.kalypso.model.hydrology.binding.model.channels.StorageChannel;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypso.ui.rrm.internal.results.view.base.HydrologyResultReference;
import org.kalypso.ui.rrm.internal.results.view.base.KalypsoHydrologyResults.CATCHMENT_RESULT_TYPE;
import org.kalypso.ui.rrm.internal.results.view.base.KalypsoHydrologyResults.NODE_RESULT_TYPE;
import org.kalypso.ui.rrm.internal.results.view.base.KalypsoHydrologyResults.STORAGE_RESULT_TYPE;
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
            virtualRootNode.addChild( buildCalculationCaseNodes( virtualRootNode, (IFolder) resource ) );
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

  protected TreeNode buildCalculationCaseNodes( final TreeNode parent, final IFolder calcCaseFolder )
  {
    final TreeNode calcCase = new TreeNode( parent, new HydrologyCalculationCaseGroupUiHandler( calcCaseFolder ), calcCaseFolder );
    calcCase.addChild( buildHydrologyNodes( calcCase, calcCaseFolder ) );
    calcCase.addChild( buildHydrologyCatchments( calcCase, calcCaseFolder ) );
    calcCase.addChild( buildHydrologyStorages( calcCase, calcCaseFolder ) );

    return calcCase;
  }

  private TreeNode buildHydrologyStorages( final TreeNode parent, final IFolder calcCaseFolder )
  {
    final IFeatureBindingCollection<Channel> channels = m_model.getChannels();

    final TreeNode base = new TreeNode( parent, new HydrologyStorageChannelsGroupUiHandler(), channels );
    channels.accept( new IFeatureBindingCollectionVisitor<Channel>()
    {
      @Override
      public void visit( final Channel channel )
      {
        if( channel instanceof StorageChannel )
          base.addChild( toTreeNode( base, (StorageChannel) channel, calcCaseFolder ) );
      }
    } );

    return base;
  }

  protected TreeNode toTreeNode( final TreeNode parent, final StorageChannel channel, final IFolder calcCaseFolder )
  {
    final TreeNode node = new TreeNode( parent, new HydrologyStorageChannelUiHandler( channel ), channel );
    node.addChild( new TreeNode( node, new HydrologyStorageParameterUiHandler( channel, STORAGE_RESULT_TYPE.eFuellvolumen ), new HydrologyResultReference( calcCaseFolder, channel, STORAGE_RESULT_TYPE.eFuellvolumen.getFileName() ) ) );
    node.addChild( new TreeNode( node, new HydrologyStorageParameterUiHandler( channel, STORAGE_RESULT_TYPE.eSpeicherUeberlauf ), new HydrologyResultReference( calcCaseFolder, channel, STORAGE_RESULT_TYPE.eSpeicherUeberlauf.getFileName() ) ) );

    return node;
  }

  private TreeNode buildHydrologyCatchments( final TreeNode parent, final IFolder calcCaseFolder )
  {
    final IFeatureBindingCollection<Catchment> catchments = m_model.getCatchments();

    final TreeNode base = new TreeNode( parent, new HydrologyCatchmentsGroupUiHandler(), catchments );
    catchments.accept( new IFeatureBindingCollectionVisitor<Catchment>()
    {
      @Override
      public void visit( final Catchment catchment )
      {
        if( catchment.isGenerateResults() )
          base.addChild( toTreeNode( base, catchment, calcCaseFolder ) );
      }
    } );

    return base;
  }

  protected TreeNode toTreeNode( final TreeNode parent, final Catchment catchment, final IFolder calcCaseFolder )
  {
    final TreeNode nodeCatchment = new TreeNode( parent, new HydrologyCatchmentUiHandler( catchment ), catchment );
    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, CATCHMENT_RESULT_TYPE.eTemperature ), new HydrologyResultReference( calcCaseFolder, catchment, CATCHMENT_RESULT_TYPE.eTemperature.getFileName() ) ) );
    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, CATCHMENT_RESULT_TYPE.eNiederschlag ), new HydrologyResultReference( calcCaseFolder, catchment, CATCHMENT_RESULT_TYPE.eNiederschlag.getFileName() ) ) );
    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, CATCHMENT_RESULT_TYPE.eSchneehoehe ), new HydrologyResultReference( calcCaseFolder, catchment, CATCHMENT_RESULT_TYPE.eSchneehoehe.getFileName() ) ) );
    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, CATCHMENT_RESULT_TYPE.eGesamtTeilgebietsQ ), new HydrologyResultReference( calcCaseFolder, catchment, CATCHMENT_RESULT_TYPE.eGesamtTeilgebietsQ.getFileName() ) ) );
    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, CATCHMENT_RESULT_TYPE.eOberflaechenQNatuerlich ), new HydrologyResultReference( calcCaseFolder, catchment, CATCHMENT_RESULT_TYPE.eOberflaechenQNatuerlich.getFileName() ) ) );
    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, CATCHMENT_RESULT_TYPE.eOberflaechenQVersiegelt ), new HydrologyResultReference( calcCaseFolder, catchment, CATCHMENT_RESULT_TYPE.eOberflaechenQVersiegelt.getFileName() ) ) );
    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, CATCHMENT_RESULT_TYPE.eInterflow ), new HydrologyResultReference( calcCaseFolder, catchment, CATCHMENT_RESULT_TYPE.eInterflow.getFileName() ) ) );
    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, CATCHMENT_RESULT_TYPE.eBasisQ ), new HydrologyResultReference( calcCaseFolder, catchment, CATCHMENT_RESULT_TYPE.eBasisQ.getFileName() ) ) );
    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, CATCHMENT_RESULT_TYPE.eGrundwasserQ ), new HydrologyResultReference( calcCaseFolder, catchment, CATCHMENT_RESULT_TYPE.eGrundwasserQ.getFileName() ) ) );
    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, CATCHMENT_RESULT_TYPE.eGrundwasserstand ), new HydrologyResultReference( calcCaseFolder, catchment, CATCHMENT_RESULT_TYPE.eGrundwasserstand.getFileName() ) ) );
    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, CATCHMENT_RESULT_TYPE.eEvapotranspiration ), new HydrologyResultReference( calcCaseFolder, catchment, CATCHMENT_RESULT_TYPE.eEvapotranspiration.getFileName() ) ) );

    return nodeCatchment;
  }

  private TreeNode buildHydrologyNodes( final TreeNode parent, final IFolder calcCaseFolder )
  {
    final IFeatureBindingCollection<Node> nodes = m_model.getNodes();

    final TreeNode base = new TreeNode( parent, new HydrologyNodesGroupUiHandler(), nodes );
    nodes.accept( new IFeatureBindingCollectionVisitor<Node>()
    {
      @Override
      public void visit( final Node node )
      {
        if( node.isGenerateResults() )
        {
          base.addChild( toTreeNode( base, node, calcCaseFolder ) );
        }
      }
    } );

    return base;
  }

  protected TreeNode toTreeNode( final TreeNode parent, final Node hydrologyNode, final IFolder calcCaseFolder )
  {
    final TreeNode node = new TreeNode( parent, new HydrologyNodeUiHandler( hydrologyNode ), hydrologyNode );
    node.addChild( new TreeNode( node, new HydrologyNodeParameterUiHandler( hydrologyNode, NODE_RESULT_TYPE.eGesamtknotenAbfluss ), new HydrologyResultReference( calcCaseFolder, hydrologyNode, NODE_RESULT_TYPE.eGesamtknotenAbfluss.getFileName() ) ) );

    return node;
  }
}
