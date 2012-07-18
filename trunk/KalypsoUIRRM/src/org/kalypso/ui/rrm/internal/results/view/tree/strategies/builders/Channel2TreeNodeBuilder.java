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
package org.kalypso.ui.rrm.internal.results.view.tree.strategies.builders;

import org.kalypso.model.hydrology.binding.model.channels.Channel;
import org.kalypso.model.hydrology.binding.model.channels.StorageChannel;
import org.kalypso.model.hydrology.project.RrmCalculationResult;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.results.view.ResultManagementView;
import org.kalypso.ui.rrm.internal.results.view.base.HydrologyResultReference;
import org.kalypso.ui.rrm.internal.results.view.base.KalypsoHydrologyResults.RRM_RESULT;
import org.kalypso.ui.rrm.internal.results.view.tree.strategies.ParameterSetBuilder;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNode;
import org.kalypsodeegree.model.feature.IFeatureBindingCollectionVisitor;

/**
 * @author Dirk Kuch
 */
public class Channel2TreeNodeBuilder implements IFeatureBindingCollectionVisitor<Channel>
{
  private final RrmSimulation m_simulation;

  private final RrmCalculationResult m_calculation;

  private final TreeNode m_parent;

  private final ResultManagementView m_view;

  public Channel2TreeNodeBuilder( final RrmSimulation simulation, final RrmCalculationResult calculationFolder, final TreeNode parent, final ResultManagementView view )
  {
    m_simulation = simulation;
    m_calculation = calculationFolder;
    m_parent = parent;
    m_view = view;
  }

  @Override
  public void visit( final Channel channel )
  {
    if( channel instanceof StorageChannel )
    {
      final ParameterSetBuilder builder = new ParameterSetBuilder( m_simulation, m_calculation, channel );
      builder.init( m_parent, UIRrmImages.DESCRIPTORS.STORAGE_CHANNEL, UIRrmImages.DESCRIPTORS.INVALID_MODEL_ELEMENT, UIRrmImages.DESCRIPTORS.EMPTY_STORAGE_CHANNEL, m_view );

      builder.doAddNode( new HydrologyResultReference( m_simulation, m_calculation, channel, RRM_RESULT.storageFuellvolumen ), m_view );
      builder.doAddNode( new HydrologyResultReference( m_simulation, m_calculation, channel, RRM_RESULT.storageSpeicherUeberlauf ), m_view );

      final StorageChannel storage = (StorageChannel) channel;

      builder.doAddNode( new HydrologyResultReference( m_simulation, m_calculation, storage, storage.getSeaEvaporationTimeseriesLink(), RRM_RESULT.inputEvaporation ), m_view );
    }
  }
}