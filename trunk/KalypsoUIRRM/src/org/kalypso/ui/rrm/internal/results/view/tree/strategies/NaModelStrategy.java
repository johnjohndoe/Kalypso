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

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.model.channels.Channel;
import org.kalypso.model.hydrology.binding.model.channels.StorageChannel;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.rrm.internal.UIRrmImages.DESCRIPTORS;
import org.kalypso.ui.rrm.internal.results.view.base.KalypsoHydrologyResults.RRM_RESULT;
import org.kalypso.ui.rrm.internal.results.view.tree.HydrologyCalculationFoldersCollector;
import org.kalypso.ui.rrm.internal.results.view.tree.handlers.HydrologyCalculationCaseGroupUiHandler;
import org.kalypso.ui.rrm.internal.results.view.tree.handlers.HydrologyGroupUiHandler;
import org.kalypso.ui.rrm.internal.results.view.tree.handlers.ResultCategoryUiHandler;
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

import com.google.common.base.CharMatcher;
import com.google.common.base.Splitter;

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

      final HydrologyCalculationFoldersCollector visitor = new HydrologyCalculationFoldersCollector( simulation );
      simulation.getResultsFolder().accept( visitor, 1, false );

      final IFolder[] caluculationResultsFolders = visitor.getFolders();

      for( final IFolder calculationResultFolder : caluculationResultsFolders )
      {
        final Map<String, Set<Feature>> resultCategories = new TreeMap<>();

        final TreeNode calculationResultNode = new TreeNode( calcCase, new HydrologyCalculationCaseGroupUiHandler( simulation, calculationResultFolder ), calculationResultFolder );

        calculationResultNode.addChild( doAddNodes( model.getNodes(), calculationResultNode, simulation, calculationResultFolder, resultCategories ) );
        calculationResultNode.addChild( doAddCatchments( model.getCatchments(), calculationResultNode, simulation, calculationResultFolder, resultCategories ) );
        calculationResultNode.addChild( doAddStorageChannels( model.getChannels(), calculationResultNode, simulation, calculationResultFolder, resultCategories ) );

        if( !resultCategories.isEmpty() )
          calculationResultNode.addChild( doAddResultCategories( resultCategories, calculationResultNode, simulation, calculationResultFolder ) );

        calcCase.addChild( calculationResultNode );
      }

    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    return calcCase;
  }

  private TreeNode doAddResultCategories( final Map<String, Set<Feature>> categories, final TreeNode parent, final RrmSimulation simulation, final IFolder calculationResultFolder )
  {
    final TreeNode base = new TreeNode( parent, new ResultCategoryUiHandler( simulation, "Kategorien" ), "Kategorien" );

    final Map<Pair<Integer, String>, TreeNode> registry = new HashMap<>();

    final Set<Entry<String, Set<Feature>>> entries = categories.entrySet();
    for( final Entry<String, Set<Feature>> entry : entries )
    {
      final String path = entry.getKey();
      final Set<Feature> elements = entry.getValue();

      final TreeNode category = doAddResultCategries( simulation, base, path, registry );

      doAddResultCategories( simulation, category, calculationResultFolder, elements.toArray( new Feature[] {} ) );
    }

    return base;
  }

  private void doAddResultCategories( final RrmSimulation simulation, final TreeNode parent, final IFolder calculationFolder, final Feature[] elements )
  {
    final Catchment2TreeNodeBuilder catchmentBuilder = new Catchment2TreeNodeBuilder( simulation, calculationFolder, parent );
    final Channel2TreeNodeBuilder channelBuilder = new Channel2TreeNodeBuilder( simulation, calculationFolder, parent );
    final Node2TreeNodeBuilder nodeBuilder = new Node2TreeNodeBuilder( simulation, calculationFolder, parent );

    for( final Feature element : elements )
    {
      if( element instanceof Catchment )
        catchmentBuilder.visit( (Catchment) element );
      else if( element instanceof StorageChannel )
        channelBuilder.visit( (StorageChannel) element );
      else if( element instanceof Node )
        nodeBuilder.visit( (Node) element );
    }

  }

  private TreeNode doAddResultCategries( final RrmSimulation simulation, final TreeNode parent, final String path, final Map<Pair<Integer, String>, TreeNode> registry )
  {

    TreeNode ptr = parent;

    final CharMatcher matcher = new CharMatcher()
    {
      @Override
      public boolean matches( final char c )
      {
        if( '/' == c ) //$NON-NLS-1$
          return true;
        else if( '\\' == c ) //$NON-NLS-1$
          return true;
        else if( ';' == c ) //$NON-NLS-1$
          return true;

        return false;
      }
    };

    final Iterable<String> parts = Splitter.on( matcher ).trimResults().omitEmptyStrings().split( path ); //$NON-NLS-1$
    int count = 0;

    for( final String part : parts )
    {
      final Pair<Integer, String> index = Pair.of( count, part );
      TreeNode node = registry.get( index );
      if( Objects.isNull( node ) )
      {
        node = new TreeNode( ptr, new ResultCategoryUiHandler( simulation, part ), part );
        ptr.addChild( node );

        registry.put( index, node );
      }

      ptr = node;
      count++;
    }

    return ptr;
  }

  private TreeNode doAddStorageChannels( final IFeatureBindingCollection<Channel> channels, final TreeNode parent, final RrmSimulation simulation, final IFolder calculationFolder, final Map<String, Set<Feature>> categories )
  {
    final TreeNode base = new TreeNode( parent, new HydrologyGroupUiHandler( simulation, "Speicherstr‰nge", DESCRIPTORS.STORAGE_CHANNEL ), RRM_RESULT.class );
    channels.accept( new Channel2TreeNodeBuilder( simulation, calculationFolder, base ) );
    channels.accept( new ChannelResultCategoriesCollector( categories ) );

    return base;
  }

  private TreeNode doAddCatchments( final IFeatureBindingCollection<Catchment> catchments, final TreeNode parent, final RrmSimulation simulation, final IFolder calculationFolder, final Map<String, Set<Feature>> categories )
  {
    final TreeNode base = new TreeNode( parent, new HydrologyGroupUiHandler( simulation, "Einzugsgebiete", DESCRIPTORS.CATCHMENT ), RRM_RESULT.class );
    catchments.accept( new Catchment2TreeNodeBuilder( simulation, calculationFolder, base ) );
    catchments.accept( new CatchmentResultCategoriesCollector( categories ) );

    return base;
  }

  private TreeNode doAddNodes( final IFeatureBindingCollection<Node> nodes, final TreeNode parent, final RrmSimulation simulation, final IFolder calculationFolder, final Map<String, Set<Feature>> categories )
  {
    final TreeNode base = new TreeNode( parent, new HydrologyGroupUiHandler( simulation, "Knoten", DESCRIPTORS.NA_NODE ), RRM_RESULT.class );
    nodes.accept( new Node2TreeNodeBuilder( simulation, calculationFolder, base ) );
    nodes.accept( new NodeResultCategoriesCollector( categories ) );

    return base;
  }

}
