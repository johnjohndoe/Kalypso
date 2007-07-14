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

import java.util.ArrayList;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.AddSubCalcUnitsToCalcUnit1D2DCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.RemoveSubCalcUnitsFromCalcUnit1D2DCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelUtil;
import org.kalypso.kalypsosimulationmodel.core.Util;

/**
 * @author Madanagopal
 * 
 */
public class CreateSubCalculationUnitCopyWizard extends Wizard implements INewWizard
{

  private final CalculationUnitDataModel dataModel;

  private IStructuredSelection selection;

  private CreateSubCalculationUnitCopyWizardPage firstPage;

  private ICalculationUnit1D2D calculation1D2D;

  public CreateSubCalculationUnitCopyWizard( final CalculationUnitDataModel dataModel )
  {
    this.dataModel = dataModel;
  }

  @Override
  public void addPages( )
  {
    firstPage = new CreateSubCalculationUnitCopyWizardPage( dataModel );
    addPage( firstPage );
    firstPage.init( selection );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    calculation1D2D = (ICalculationUnit1D2D) dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
    final RemoveSubCalcUnitsFromCalcUnit1D2DCmd cmdToRemove = new RemoveSubCalcUnitsFromCalcUnit1D2DCmd( new ArrayList<ICalculationUnit>( calculation1D2D.getSubUnits() ), calculation1D2D, Util.getModel( IFEDiscretisationModel1d2d.class ) )
    {
      @Override
      public void process( )
      {
        try
        {
          super.process();
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }
        dataModel.setData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER, dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER ) );
      }

    };
    KeyBasedDataModelUtil.postCommand( dataModel, cmdToRemove, ICommonKeys.KEY_COMMAND_MANAGER_DISC_MODEL );
    final AddSubCalcUnitsToCalcUnit1D2DCmd cmdToAdd = new AddSubCalcUnitsToCalcUnit1D2DCmd( firstPage.getInputListCalSubUnits(), calculation1D2D, Util.getModel( IFEDiscretisationModel1d2d.class ) )
    {

      @Override
      public void process( )
      {
        try
        {
          super.process();
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }
        dataModel.setData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER, dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER ) );
      }
    };
    KeyBasedDataModelUtil.postCommand( dataModel, cmdToAdd, ICommonKeys.KEY_COMMAND_MANAGER_DISC_MODEL );
    return true;
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    this.selection = selection;
  }

}
