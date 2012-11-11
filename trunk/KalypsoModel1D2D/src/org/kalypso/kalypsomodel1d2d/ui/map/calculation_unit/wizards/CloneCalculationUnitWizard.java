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
package org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.wizards;

import java.util.List;

import org.eclipse.jface.wizard.Wizard;
import org.kalypso.afgui.model.Util;
import org.kalypso.kalypsomodel1d2d.ops.CalcUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModelGroup;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.CloneCalculationUnitCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelUtil;

public class CloneCalculationUnitWizard extends Wizard
{
  private CloneCalculationUnitWizardPage m_page;

  private final ICalculationUnit1D2D m_calcUnitToClone;

  private final KeyBasedDataModel m_dataModel;

  private final CalculationUnitDataModel m_calcUnitDataModel;

  public CloneCalculationUnitWizard( final KeyBasedDataModel dataModel, final ICalculationUnit1D2D calcUnitToClone, final CalculationUnitDataModel calcUnitDataModel )
  {
    m_dataModel = dataModel;
    m_calcUnitToClone = calcUnitToClone;
    m_calcUnitDataModel = calcUnitDataModel;

    super.setWindowTitle( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.wizards.CloneCalculationUnitWizard.0" ) ); //$NON-NLS-1$
  }

  @Override
  public void addPages( )
  {
    m_page = new CloneCalculationUnitWizardPage( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.wizards.CloneCalculationUnitWizard.1" ), m_calcUnitDataModel ); //$NON-NLS-1$ //$NON-NLS-2$
    addPage( m_page );
  }

  @Override
  public boolean performFinish( )
  {
    final String calcUnitName = m_page.getCalculationUnitName();
    final String description = m_page.getCalculationUnitDescription();

    final KeyBasedDataModel dataModel = m_dataModel;

    final IFEDiscretisationModel1d2d model = Util.getModel( IFEDiscretisationModel1d2d.class.getName() );
    final IControlModelGroup controlModels = Util.getModel( IControlModelGroup.class.getName() );

    final CloneCalculationUnitCmd cmd = new CloneCalculationUnitCmd( model, controlModels, calcUnitName, description, m_calcUnitToClone );

    KeyBasedDataModelUtil.postCommand( m_dataModel, cmd, ICommonKeys.KEY_COMMAND_MANAGER_DISC_MODEL );

    // reset list of calculation units
    final IFEDiscretisationModel1d2d model1d2d = (IFEDiscretisationModel1d2d)dataModel.getData( ICommonKeys.KEY_DISCRETISATION_MODEL );
    final List<ICalculationUnit> calcUnits = CalcUnitOps.getModelCalculationUnits( model1d2d );
    dataModel.setData( ICommonKeys.KEY_FEATURE_WRAPPER_LIST, calcUnits );

    // set the create unit as selected
    dataModel.setData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER, cmd.getCreatedCalculationUnit() );

    return true;
  }
}