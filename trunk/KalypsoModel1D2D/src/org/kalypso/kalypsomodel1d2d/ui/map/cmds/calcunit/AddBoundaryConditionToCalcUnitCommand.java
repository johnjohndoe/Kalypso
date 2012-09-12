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
package org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.IFeatureChangeCommand;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * @author madanago
 */
public class AddBoundaryConditionToCalcUnitCommand implements IFeatureChangeCommand
{
  private final IBoundaryCondition m_boundaryConditionToAdd;

  private final ICalculationUnit m_calculationUnit;

  private final boolean m_done = false;

  public AddBoundaryConditionToCalcUnitCommand( final ICalculationUnit calculationUnit, final IBoundaryCondition boundaryConditionToAdd )
  {
    Assert.throwIAEOnNullParam( calculationUnit, "calculationUnit" ); //$NON-NLS-1$
    Assert.throwIAEOnNullParam( boundaryConditionToAdd, "boundaryConditionToAdd" ); //$NON-NLS-1$

    m_calculationUnit = calculationUnit;
    m_boundaryConditionToAdd = boundaryConditionToAdd;
  }

  @Override
  public Feature[] getChangedFeatures( )
  {
    if( m_done )
      return new Feature[] { m_calculationUnit, m_boundaryConditionToAdd };
    else
      return new Feature[] {};
  }

  @Override
  public String getDescription( )
  {
    return null;
  }

  @Override
  public boolean isUndoable( )
  {
    return false;
  }

  @Override
  public void process( ) throws Exception
  {
    try
    {
      if( !m_done )
      {
        final List calculationUnitID = (List)m_boundaryConditionToAdd.getProperty( Kalypso1D2DSchemaConstants.OP1D2D_PROP_PARENT_CALCUNIT );
        calculationUnitID.add( m_calculationUnit.getId() );
        fireProcessChanges();
      }
    }
    catch( final Throwable th )
    {
      th.printStackTrace();
    }

  }

  private void fireProcessChanges( )
  {
    final Feature calcUnitFeature = m_calculationUnit;
    final List<Feature> features = new ArrayList<>();
    features.add( calcUnitFeature );
    features.add( m_boundaryConditionToAdd );

    final GMLWorkspace calcUnitWorkspace = calcUnitFeature.getWorkspace();
    final FeatureStructureChangeModellEvent event = new FeatureStructureChangeModellEvent( calcUnitWorkspace, calcUnitFeature.getOwner(), features.toArray( new Feature[features.size()] ), FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE );
    calcUnitWorkspace.fireModellEvent( event );

    final Feature bcFeature = m_boundaryConditionToAdd;
    final GMLWorkspace bcWorkspace = bcFeature.getWorkspace();
    final FeatureStructureChangeModellEvent bcEvent = new FeatureStructureChangeModellEvent( bcWorkspace, bcFeature.getOwner(), new Feature[] { bcFeature }, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE );
    bcWorkspace.fireModellEvent( bcEvent );
  }

  @Override
  public void redo( ) throws Exception
  {

  }

  @Override
  public void undo( ) throws Exception
  {

  }
}
