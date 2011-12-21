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
package org.kalypso.ui.rrm.internal.cm.view;

import java.util.Collection;
import java.util.LinkedList;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import org.kalypso.model.hydrology.cm.binding.ICatchmentModel;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeStrategy;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNode;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNodeModel;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Gernot Belger
 */
public class CatchmentsTreeStrategy implements ITreeNodeStrategy
{
  private final ICatchmentModel m_model;

  public CatchmentsTreeStrategy( final ICatchmentModel model )
  {
    m_model = model;
  }

  @Override
  public TreeNode buildNodes( final TreeNodeModel model )
  {
    final Map<String, Collection<IRainfallGenerator>> byParameterType = hashByParameterType();

    final TreeNode rootNode = new TreeNode( model, null, null, this );

    if( byParameterType.size() == 0 )
      buildEmptyNode( rootNode );
    else
      buildParameterNodes( rootNode, byParameterType );

    return rootNode;
  }

  private Map<String, Collection<IRainfallGenerator>> hashByParameterType( )
  {
    final Map<String, Collection<IRainfallGenerator>> byParameterType = new TreeMap<>();

    final IFeatureBindingCollection<IRainfallGenerator> generators = m_model.getGenerators();
    for( final IRainfallGenerator generator : generators )
    {
      final String parameterType = generator.getParameterType();
      if( parameterType != null )
      {
        if( !byParameterType.containsKey( parameterType ) )
          byParameterType.put( parameterType, new LinkedList<IRainfallGenerator>() );

        final Collection<IRainfallGenerator> generatorsOfType = byParameterType.get( parameterType );
        generatorsOfType.add( generator );
      }
    }

    return byParameterType;
  }

  private void buildEmptyNode( final TreeNode parent )
  {
    final TreeNode emptyNode = new TreeNode( parent, new EmptyNodeUiHandler( parent.getModel() ), new Object() );
    parent.addChild( emptyNode );
  }

  private void buildParameterNodes( final TreeNode parent, final Map<String, Collection<IRainfallGenerator>> byParameterType )
  {
    for( final Entry<String, Collection<IRainfallGenerator>> entry : byParameterType.entrySet() )
    {
      final String parameterType = entry.getKey();
      final Collection<IRainfallGenerator> generators = entry.getValue();
      final IRainfallGenerator[] allGenerators = generators.toArray( new IRainfallGenerator[generators.size()] );

      final TreeNode parameterNode = new TreeNode( parent, new ParameterGeneratorUiHandler( parent.getModel(), parameterType, allGenerators ), entry );

      buildGeneratorNodes( parameterNode, generators );

      parent.addChild( parameterNode );
    }
  }

  private void buildGeneratorNodes( final TreeNode parent, final Collection<IRainfallGenerator> generators )
  {
    for( final IRainfallGenerator generator : generators )
    {
      final TreeNode generatorNode = new TreeNode( parent, new GeneratorUiHandler( parent.getModel(), generator ), generator );
      parent.addChild( generatorNode );
    }
  }
}