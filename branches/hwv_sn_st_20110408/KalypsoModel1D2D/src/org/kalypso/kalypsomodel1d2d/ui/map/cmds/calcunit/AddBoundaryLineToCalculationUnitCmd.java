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
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * Command for adding continuity line to the calculation unit
 * 
 * @author Patrice Congo
 * @author Dejan Antanaskovic
 */
public class AddBoundaryLineToCalculationUnitCmd implements IDiscrModel1d2dChangeCommand
{
  private final IFELine m_feLine;

  private final ICalculationUnit m_calculationUnit;

  private final IFEDiscretisationModel1d2d m_model1d2d;

  private boolean m_commandProcessed = false;

  /**
   * Denotes the relation, which goes beyond simple elements inclusion, of the boundary line to added to the calculation
   * unit
   */
  public AddBoundaryLineToCalculationUnitCmd( final ICalculationUnit calculationUnit, final IFELine continuityLine, final IFEDiscretisationModel1d2d model1d2d )
  {
    m_calculationUnit = calculationUnit;
    m_feLine = continuityLine;
    m_model1d2d = model1d2d;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand#getChangedFeature()
   */
  @Override
  public IFeatureWrapper2[] getChangedFeature( )
  {
    if( m_commandProcessed )
      return new IFeatureWrapper2[] { m_calculationUnit, m_feLine };
    else
      return new IFeatureWrapper2[] {};
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand#getDiscretisationModel1d2d()
   */
  @Override
  public IFEDiscretisationModel1d2d getDiscretisationModel1d2d( )
  {
    return m_model1d2d;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  @Override
  public String getDescription( )
  {
    return "Command for adding continuity line to the calculation unit"; //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  @Override
  public boolean isUndoable( )
  {
    return false;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  @Override
  public void process( ) throws Exception
  {
    try
    {
      if( !m_commandProcessed )
      {
        if( m_calculationUnit.addElementAsRef( m_feLine ) )
          m_feLine.getContainers().addRef( m_calculationUnit );
        fireProcessChanges();
      }
    }
    catch( Throwable th )
    {
      th.printStackTrace();
    }
  }

  private final void fireProcessChanges( )
  {
    final List<Feature> features = new ArrayList<Feature>();
    features.add( m_calculationUnit.getFeature() );
    features.add( m_feLine.getFeature() );
    final GMLWorkspace workspace = m_calculationUnit.getFeature().getWorkspace();
    final FeatureStructureChangeModellEvent event = new FeatureStructureChangeModellEvent( workspace, m_model1d2d.getFeature(), features.toArray( new Feature[features.size()] ), FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE );
    workspace.fireModellEvent( event );
    m_commandProcessed = true;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  @Override
  public void redo( ) throws Exception
  {
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  @Override
  public void undo( ) throws Exception
  {
  }
}
