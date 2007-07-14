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

import org.kalypso.kalypsomodel1d2d.ops.CalUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;

/**
 * @author madanago
 * 
 */
@SuppressWarnings( { "unchecked", "hiding" })
public class AddBoundaryConditionToCalculationUnitCmd implements IDiscrModel1d2dChangeCommand
{
  private final IBoundaryCondition m_boundaryConditionToAdd;

  private final ICalculationUnit m_calculationUnit;

  private boolean done = false;

  private final double m_grabDistance;

  public AddBoundaryConditionToCalculationUnitCmd( final ICalculationUnit calculationUnit, final IBoundaryCondition boundaryConditionToAdd, final double grabDistance )
  {
    Assert.throwIAEOnNullParam( calculationUnit, "calculationUnit" );
    Assert.throwIAEOnNullParam( boundaryConditionToAdd, "boundaryConditionToAdd" );

    m_calculationUnit = calculationUnit;
    m_boundaryConditionToAdd = boundaryConditionToAdd;
    m_grabDistance = grabDistance;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand#getChangedFeature()
   */
  public IFeatureWrapper2[] getChangedFeature( )
  {
    if( done )
    {
      return new IFeatureWrapper2[] { m_calculationUnit, m_boundaryConditionToAdd };
    }
    else
    {
      return new IFeatureWrapper2[] {};
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand#getDiscretisationModel1d2d()
   */
  public IFEDiscretisationModel1d2d getDiscretisationModel1d2d( )
  {
    return null;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return null;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  public boolean isUndoable( )
  {
    return false;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  public void process( ) throws Exception
  {
    try
    {
      if( !done )
      {
        CalUnitOps.markAsBoundaryCondition( m_calculationUnit, m_boundaryConditionToAdd, m_grabDistance );
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
    final Feature bcFeature = m_boundaryConditionToAdd.getWrappedFeature();
    final List<Feature> features = new ArrayList<Feature>();
    features.add( bcFeature );

    final GMLWorkspace workspace = bcFeature.getWorkspace();
    final FeaturesChangedModellEvent event = new FeaturesChangedModellEvent( workspace, features.toArray( new Feature[features.size()] ) );
    workspace.fireModellEvent( event );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {

  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {

  }

}
