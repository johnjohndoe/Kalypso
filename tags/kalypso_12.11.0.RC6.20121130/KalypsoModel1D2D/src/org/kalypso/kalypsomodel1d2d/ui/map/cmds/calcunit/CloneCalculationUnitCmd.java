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

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2DCollection;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModelGroup;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.IFeatureChangeCommand;
import org.kalypso.kalypsosimulationmodel.core.modeling.IControlModel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Command to create new calculation unit
 * 
 * @author Gernot Belger
 */
public class CloneCalculationUnitCmd implements IFeatureChangeCommand
{
  /**
   * the created calculation unit
   */
  private ICalculationUnit m_calculationUnit;

  /**
   * the original calculation unit, that should be cloned to the new one
   */
  private ICalculationUnit1D2D m_calculationUnitOrig = null;

  /**
   * the discretisation model holding the calculation unit
   */
  private final IFEDiscretisationModel1d2d m_model1d2d;

  /**
   * the name the calculation unit will be assigned to
   */
  private final String m_calcUnitName;

  private final IControlModelGroup m_controlModels;

  private final String m_description;

  /**
   * Creates a Calculation unit of the given q-name
   * 
   * @param cuFeatureQName
   *          the q-name of the calculation unit to create
   * @param model1d2d
   *          the model that should hold the new calculation unit
   * @param name
   *          a name for the calculation unit if one has to be set or null
   * @param calcUnitToClone
   *          original calculation unit that will be cloned in to new one
   * @throws IllegalArgumentException
   *           if cuFeatureQName or model1d2d is null
   */
  public CloneCalculationUnitCmd( final IFEDiscretisationModel1d2d model1d2d, final IControlModelGroup controlModels, final String name, final String description, final ICalculationUnit1D2D calcUnitToClone )
  {
    m_model1d2d = model1d2d;
    m_controlModels = controlModels;
    m_calcUnitName = name;
    m_description = description;
    m_calculationUnitOrig = calcUnitToClone;
  }

  @Override
  public Feature[] getChangedFeatures( )
  {
    if( m_calculationUnit != null )
      return new Feature[] { m_model1d2d, m_calculationUnit };
    else
      return new Feature[] {};
  }

  @Override
  public String getDescription( )
  {
    return Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.CreateCalculationUnitCmd.0" ); //$NON-NLS-1$
  }

  @Override
  public boolean isUndoable( )
  {
    return true;
  }

  @Override
  public void process( ) throws Exception
  {
    m_calculationUnit = (ICalculationUnit)FeatureHelper.cloneFeature( m_calculationUnitOrig.getOwner(), m_calculationUnitOrig.getParentRelation(), m_calculationUnitOrig );

    m_calculationUnit.setName( m_calcUnitName );
    m_calculationUnit.setDescription( m_description );

    copyDataControlModel();

    fireProcessChanges( m_calculationUnit );
  }

  private void copyDataControlModel( ) throws Exception
  {
    final IControlModel1D2DCollection controlModel1D2DCollection = m_controlModels.getModel1D2DCollection();

    final IControlModel controlModelOrig = controlModel1D2DCollection.findControlModel( m_calculationUnitOrig );
    if( controlModelOrig == null )
    {
      // error message or can this actually happen?
      return;
    }

    /* create clone of that model */
    final Feature controlModelFeature = FeatureHelper.cloneFeature( controlModelOrig.getOwner(), controlModelOrig.getParentRelation(), controlModelOrig );
    final IControlModel1D2D newControlModel = (IControlModel1D2D)controlModelFeature.getAdapter( IControlModel1D2D.class );

    newControlModel.setCalculationUnit( m_calculationUnit );

    // TODO: what for ?? :
    controlModel1D2DCollection.setActiveControlModel( newControlModel );
  }

  /**
   * @param calculationUnit
   *          the added or removed calculation unit
   * @param added
   *          true if the calculation unit was added false otherwise
   */
  private final void fireProcessChanges( final ICalculationUnit calculationUnit )
  {
    final GMLWorkspace workspace = calculationUnit.getWorkspace();
    final FeatureStructureChangeModellEvent event = new FeatureStructureChangeModellEvent( workspace, m_model1d2d, calculationUnit, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD );
    workspace.fireModellEvent( event );
  }

  @Override
  public void redo( ) throws Exception
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public void undo( ) throws Exception
  {
    throw new UnsupportedOperationException();
  }

  public ICalculationUnit getCreatedCalculationUnit( )
  {
    return m_calculationUnit;
  }
}