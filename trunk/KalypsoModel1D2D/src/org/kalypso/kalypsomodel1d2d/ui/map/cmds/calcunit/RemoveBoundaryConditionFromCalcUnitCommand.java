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
package org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * @author madanagopal
 * 
 */
@SuppressWarnings( { "hiding", "unchecked" }) //$NON-NLS-1$ //$NON-NLS-2$
public class RemoveBoundaryConditionFromCalcUnitCommand implements IDiscrModel1d2dChangeCommand
{
  private ICalculationUnit m_calculationUnit;

  private IBoundaryCondition m_boundaryCondition;

  private IFEDiscretisationModel1d2d m_model1d2d;

  private boolean m_done = false;

  public RemoveBoundaryConditionFromCalcUnitCommand( IBoundaryCondition boundaryCondition, ICalculationUnit calculationUnit, IFEDiscretisationModel1d2d model1d2d )
  {
    m_calculationUnit = calculationUnit;
    m_boundaryCondition = boundaryCondition;
    m_model1d2d = model1d2d;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand#getChangedFeature()
   */
  @Override
  public IFeatureWrapper2[] getChangedFeature( )
  {
    return null;
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
    return "remove boundary condition from calculation unit"; //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
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
        final List parentCalcUnits = (List) m_boundaryCondition.getFeature().getProperty( Kalypso1D2DSchemaConstants.OP1D2D_PROP_PARENT_CALCUNIT );
        m_done = parentCalcUnits.remove( m_calculationUnit.getGmlID() );
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
    final Feature calUnitFeature = m_calculationUnit.getFeature();
    final Feature model1d2dFeature = m_model1d2d.getFeature();
    List<Feature> features = new ArrayList<Feature>();
    features.add( calUnitFeature );
    features.add( m_boundaryCondition.getFeature() );

    GMLWorkspace calUnitWorkspace = calUnitFeature.getWorkspace();
    FeatureStructureChangeModellEvent event = new FeatureStructureChangeModellEvent( calUnitWorkspace, model1d2dFeature, features.toArray( new Feature[features.size()] ), FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE );
    calUnitWorkspace.fireModellEvent( event );

    final Feature bcFeature = m_boundaryCondition.getFeature();
    GMLWorkspace bcWorkspace = bcFeature.getWorkspace();
    FeatureStructureChangeModellEvent bcEvent = new FeatureStructureChangeModellEvent( bcWorkspace, bcFeature.getParent(), new Feature[] { bcFeature }, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE );
    bcWorkspace.fireModellEvent( bcEvent );
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
