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
package org.kalypso.ui.rrm.internal.results.view.tree.strategies;

import org.kalypso.model.hydrology.project.RrmCalculation;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ui.rrm.internal.UIRrmImages.DESCRIPTORS;
import org.kalypso.ui.rrm.internal.results.view.base.IHydrologyResultReference;
import org.kalypso.ui.rrm.internal.results.view.tree.handlers.HydrologyParameterSetUiHandler;
import org.kalypso.ui.rrm.internal.results.view.tree.handlers.HydrologyResultReferenceUiHandler;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNode;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Dirk Kuch
 */
public class ParameterSetBuilder
{

  private final Feature m_feature;

  private final RrmSimulation m_simulation;

  private HydrologyParameterSetUiHandler m_handler;

  private TreeNode m_node;

  private final RrmCalculation m_calculation;

  public ParameterSetBuilder( final RrmSimulation simulation, final RrmCalculation calculation, final Feature feature )
  {
    m_simulation = simulation;
    m_calculation = calculation;
    m_feature = feature;
  }

  public void init( final TreeNode base, final DESCRIPTORS imgExisting, final DESCRIPTORS imgMissing )
  {
    m_handler = new HydrologyParameterSetUiHandler( m_simulation, m_calculation, m_feature, imgExisting, imgMissing );
    m_node = new TreeNode( base, m_handler, m_feature );

    base.addChild( m_node );
  }

  public void doAddNode( final IHydrologyResultReference reference )
  {
    m_node.addChild( new TreeNode( m_node, new HydrologyResultReferenceUiHandler( m_simulation, m_calculation, reference ), reference ) );
    m_handler.addReferences( reference );
  }

}
