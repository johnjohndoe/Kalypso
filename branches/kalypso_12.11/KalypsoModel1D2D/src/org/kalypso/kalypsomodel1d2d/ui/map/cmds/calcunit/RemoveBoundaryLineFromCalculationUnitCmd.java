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

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.IFeatureChangeCommand;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * Removes a boundary line from a calculation unit.
 * 
 * @author Patrice Congo
 */
public class RemoveBoundaryLineFromCalculationUnitCmd implements IFeatureChangeCommand
{
  private final IFELine m_bLineToRemove;

  private final ICalculationUnit m_calculationUnit;

  private final IFEDiscretisationModel1d2d m_model1d2d;

  private boolean m_done = false;

  public RemoveBoundaryLineFromCalculationUnitCmd( final ICalculationUnit calculationUnit, final IFELine elementsToRemove, final IFEDiscretisationModel1d2d model1d2d )
  {
    Assert.throwIAEOnNullParam( calculationUnit, "calculationUnit" ); //$NON-NLS-1$
    Assert.throwIAEOnNullParam( elementsToRemove, "elementsToRemove" ); //$NON-NLS-1$
    Assert.throwIAEOnNullParam( model1d2d, "model1d2d" ); //$NON-NLS-1$

    this.m_calculationUnit = calculationUnit;
    this.m_bLineToRemove = elementsToRemove;
    this.m_model1d2d = model1d2d;
  }

  @Override
  public Feature[] getChangedFeatures( )
  {
    if( m_done )
    {
      return new Feature[] { m_calculationUnit, m_bLineToRemove };
    }
    else
    {
      return new Feature[] {};
    }
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
        m_calculationUnit.removeLinkedItems( new IFELine[] { m_bLineToRemove } );

        fireProcessChanges();
        m_done = true;
      }
    }
    catch( final Throwable th )
    {
      th.printStackTrace();
    }
  }

  private final void fireProcessChanges( )
  {
    final Feature calUnitFeature = m_calculationUnit;
    final Feature model1d2dFeature = m_model1d2d;
    final List<Feature> features = new ArrayList<>();
    features.add( calUnitFeature );
    features.add( m_bLineToRemove );

    final GMLWorkspace workspace = calUnitFeature.getWorkspace();
    final FeatureStructureChangeModellEvent event = new FeatureStructureChangeModellEvent( workspace,// final GMLWorkspace
    // workspace,
    model1d2dFeature,// Feature parentFeature,
    features.toArray( new Feature[features.size()] ),// final Feature[] changedFeature,
    FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE// final int changeType
    );
    workspace.fireModellEvent( event );
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
