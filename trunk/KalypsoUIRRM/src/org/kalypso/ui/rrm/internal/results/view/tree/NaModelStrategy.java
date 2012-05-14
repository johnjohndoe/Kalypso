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
package org.kalypso.ui.rrm.internal.results.view.tree;

import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.model.channels.Channel;
import org.kalypso.model.hydrology.binding.model.channels.StorageChannel;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.rrm.internal.results.view.base.KalypsoHydrologyResults;
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

    virtualRootNode.addChild( buildHydrologyNodes( virtualRootNode ) );
    virtualRootNode.addChild( buildHydrologyCatchments( virtualRootNode ) );
    virtualRootNode.addChild( buildHydrologyStorages( virtualRootNode ) );

    return virtualRootNode;
  }

  private TreeNode buildHydrologyStorages( final TreeNode parent )
  {
    final IFeatureBindingCollection<Channel> channels = m_model.getChannels();

    final TreeNode base = new TreeNode( parent, new HydrologyStorageChannelsGroupUiHandler(), channels );
    channels.accept( new IFeatureBindingCollectionVisitor<Channel>()
    {
      @Override
      public void visit( final Channel channel )
      {
        if( channel instanceof StorageChannel )
          base.addChild( toTreeNode( base, (StorageChannel) channel ) );
      }
    } );

    return base;
  }

  protected TreeNode toTreeNode( final TreeNode parent, final StorageChannel channel )
  {
    return new TreeNode( parent, new HydrologyStorageChannelUiHandler( channel ), channel );
  }

  private TreeNode buildHydrologyCatchments( final TreeNode parent )
  {
    final IFeatureBindingCollection<Catchment> catchments = m_model.getCatchments();

    final TreeNode base = new TreeNode( parent, new HydrologyCatchmentsGroupUiHandler(), catchments );
    catchments.accept( new IFeatureBindingCollectionVisitor<Catchment>()
    {
      @Override
      public void visit( final Catchment catchment )
      {
        if( catchment.isGenerateResults() )
          base.addChild( toTreeNode( base, catchment ) );
      }
    } );

    return base;
  }

  protected TreeNode toTreeNode( final TreeNode parent, final Catchment catchment )
  {
    final TreeNode nodeCatchment = new TreeNode( parent, new HydrologyCatchmentUiHandler( catchment ), catchment );

    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, KalypsoHydrologyResults.CATCHMENT_RESULT_TYPE.eTemperature ), KalypsoHydrologyResults.CATCHMENT_RESULT_TYPE.eTemperature ) );
    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, KalypsoHydrologyResults.CATCHMENT_RESULT_TYPE.eNiederschlag ), KalypsoHydrologyResults.CATCHMENT_RESULT_TYPE.eNiederschlag ) );
    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, KalypsoHydrologyResults.CATCHMENT_RESULT_TYPE.eSchneehoehe ), KalypsoHydrologyResults.CATCHMENT_RESULT_TYPE.eSchneehoehe ) );
    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, KalypsoHydrologyResults.CATCHMENT_RESULT_TYPE.eGesamtTeilgebietsQ ), KalypsoHydrologyResults.CATCHMENT_RESULT_TYPE.eGesamtTeilgebietsQ ) );
    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, KalypsoHydrologyResults.CATCHMENT_RESULT_TYPE.eOberflaechenQNatuerlich ), KalypsoHydrologyResults.CATCHMENT_RESULT_TYPE.eOberflaechenQNatuerlich ) );
    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, KalypsoHydrologyResults.CATCHMENT_RESULT_TYPE.eOberflaechenQVersiegelt ), KalypsoHydrologyResults.CATCHMENT_RESULT_TYPE.eOberflaechenQVersiegelt ) );
    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, KalypsoHydrologyResults.CATCHMENT_RESULT_TYPE.eInterflow ), KalypsoHydrologyResults.CATCHMENT_RESULT_TYPE.eInterflow ) );
    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, KalypsoHydrologyResults.CATCHMENT_RESULT_TYPE.eBasisQ ), KalypsoHydrologyResults.CATCHMENT_RESULT_TYPE.eBasisQ ) );
    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, KalypsoHydrologyResults.CATCHMENT_RESULT_TYPE.eGrundwasserQ ), KalypsoHydrologyResults.CATCHMENT_RESULT_TYPE.eGrundwasserQ ) );
    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, KalypsoHydrologyResults.CATCHMENT_RESULT_TYPE.eGrundwasserstand ), KalypsoHydrologyResults.CATCHMENT_RESULT_TYPE.eGrundwasserstand ) );
    nodeCatchment.addChild( new TreeNode( nodeCatchment, new HydrologyCatchmentParameterUiHandler( catchment, KalypsoHydrologyResults.CATCHMENT_RESULT_TYPE.eEvapotranspiration ), KalypsoHydrologyResults.CATCHMENT_RESULT_TYPE.eEvapotranspiration ) );

    return nodeCatchment;
  }

  private TreeNode buildHydrologyNodes( final TreeNode parent )
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
          final ZmlLink lnkResult = node.getResultLink();
          if( lnkResult.isLinkSet() )
            base.addChild( toTreeNode( base, node ) );
        }
      }
    } );

    return base;
  }

  protected TreeNode toTreeNode( final TreeNode parent, final Node hydrologyNode )
  {
    return new TreeNode( parent, new HydrologyNodeUiHandler( hydrologyNode ), hydrologyNode );
  }
}
