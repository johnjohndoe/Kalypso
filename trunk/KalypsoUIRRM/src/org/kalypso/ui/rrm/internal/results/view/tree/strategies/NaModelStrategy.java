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

import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.model.channels.Channel;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.rrm.internal.UIRrmImages.DESCRIPTORS;
import org.kalypso.ui.rrm.internal.results.view.base.KalypsoHydrologyResults.RRM_RESULT;
import org.kalypso.ui.rrm.internal.results.view.tree.HydrologyCalculationFoldersCollector;
import org.kalypso.ui.rrm.internal.results.view.tree.handlers.HydrologyCalculationCaseGroupUiHandler;
import org.kalypso.ui.rrm.internal.results.view.tree.handlers.HydrologyGroupUiHandler;
import org.kalypso.ui.rrm.internal.results.view.tree.strategies.builders.Catchment2TreeNodeBuilder;
import org.kalypso.ui.rrm.internal.results.view.tree.strategies.builders.Channel2TreeNodeBuilder;
import org.kalypso.ui.rrm.internal.results.view.tree.strategies.builders.Node2TreeNodeBuilder;
import org.kalypso.ui.rrm.internal.results.view.tree.strategies.collector.CatchmentResultCategoriesCollector;
import org.kalypso.ui.rrm.internal.results.view.tree.strategies.collector.ChannelResultCategoriesCollector;
import org.kalypso.ui.rrm.internal.results.view.tree.strategies.collector.NodeResultCategoriesCollector;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeStrategy;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNode;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNodeModel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

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

      final Map<String, Set<Feature>> categories = new TreeMap<>();

      for( final IFolder calculationResultFolder : caluculationResultsFolders )
      {
        final TreeNode calculationResultNode = new TreeNode( calcCase, new HydrologyCalculationCaseGroupUiHandler( simulation, calculationResultFolder ), calculationResultFolder );

        calculationResultNode.addChild( addNodes( model.getNodes(), calculationResultNode, simulation, calculationResultFolder, categories ) );
        calculationResultNode.addChild( addCatchments( model.getCatchments(), calculationResultNode, simulation, calculationResultFolder, categories ) );
        calculationResultNode.addChild( addStorageChannels( model.getChannels(), calculationResultNode, simulation, calculationResultFolder, categories ) );

        calcCase.addChild( calculationResultNode );
      }

    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    return calcCase;
  }

  private TreeNode addStorageChannels( final IFeatureBindingCollection<Channel> channels, final TreeNode parent, final RrmSimulation simulation, final IFolder calculationFolder, final Map<String, Set<Feature>> categories )
  {
    final TreeNode base = new TreeNode( parent, new HydrologyGroupUiHandler( simulation, "Speicherstr‰nge", DESCRIPTORS.STORAGE_CHANNEL ), RRM_RESULT.class );
    channels.accept( new Channel2TreeNodeBuilder( simulation, calculationFolder, base ) );
    channels.accept( new ChannelResultCategoriesCollector( categories ) );

    return base;
  }

  private TreeNode addCatchments( final IFeatureBindingCollection<Catchment> catchments, final TreeNode parent, final RrmSimulation simulation, final IFolder calculationFolder, final Map<String, Set<Feature>> categories )
  {
    final TreeNode base = new TreeNode( parent, new HydrologyGroupUiHandler( simulation, "Einzugsgebiete", DESCRIPTORS.CATCHMENT ), RRM_RESULT.class );
    catchments.accept( new Catchment2TreeNodeBuilder( simulation, calculationFolder, base ) );
    catchments.accept( new CatchmentResultCategoriesCollector( categories ) );

    return base;
  }

  private TreeNode addNodes( final IFeatureBindingCollection<Node> nodes, final TreeNode parent, final RrmSimulation simulation, final IFolder calculationFolder, final Map<String, Set<Feature>> categories )
  {
    final TreeNode base = new TreeNode( parent, new HydrologyGroupUiHandler( simulation, "Knoten", DESCRIPTORS.NA_NODE ), RRM_RESULT.class );
    nodes.accept( new Node2TreeNodeBuilder( simulation, calculationFolder, base ) );
    nodes.accept( new NodeResultCategoriesCollector( categories ) );

    return base;
  }

}
