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

import javax.xml.namespace.QName;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.kalypsomodel1d2d.ops.CalUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.CreateCalculationUnitCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelUtil;
import org.kalypso.kalypsosimulationmodel.core.Util;

/**
 * @author Madanagopal
 * 
 */
public class CreateCalculationUnitWizard extends Wizard implements INewWizard
{
  private CreateCalculationUnitWizardPage firstPage;

  private IStructuredSelection initialSelection;

  private KeyBasedDataModel dataModel;

  private static final String QNAME_KEY_1D2D = "1D/2D";

  private static final String QNAME_KEY_2D = "2D";

  private static final String QNAME_KEY_1D = "1D";
  private ICalculationUnit createdCalculationUnit;

  public CreateCalculationUnitWizard( KeyBasedDataModel dataModel )
  {
    this.dataModel = dataModel;
  }

  public void addPages( )
  {

    firstPage = new CreateCalculationUnitWizardPage( "windowTitle", dataModel );
    addPage( firstPage );
    firstPage.init( initialSelection );
  }

  @Override
  public boolean performFinish( )
  {
    final String name = firstPage.getNameField();
    final String qNameKey = firstPage.getTypeCombo();
    final String desc = firstPage.getDescriptionText();
    if (name.trim().equals(""))
    {
      firstPage.setMessage( null );
      firstPage.setErrorMessage( "Provide a Name" );
      firstPage.setPageComplete( false );
      return false;
    }
    
    if (qNameKey.trim().equals(""))
    {
      firstPage.setMessage( null );
      firstPage.setErrorMessage( "Select the Calculation Unit Type" );
      firstPage.setPageComplete( false );
      return false;
    }
    
    CreateCalculationUnitCmd cmd = 
        new CreateCalculationUnitCmd(
                getCUnitQName( qNameKey ),
        Util.getModel( IFEDiscretisationModel1d2d.class ),
        name,
        desc )
    {
      /**
       * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.CreateCalculationUnitCmd#process()
       */
      @SuppressWarnings({"unchecked","synthetic-access"})
      @Override
      public void process( ) throws Exception
      {
        super.process();
        //reset list of calculation units
        IFEDiscretisationModel1d2d model1d2d =
          (IFEDiscretisationModel1d2d) 
          dataModel.getData( ICommonKeys.KEY_DISCRETISATION_MODEL );
        List<ICalculationUnit> calUnits = 
          CalUnitOps.getModelCalculationUnits( model1d2d );
        dataModel.setData( ICommonKeys.KEY_FEATURE_WRAPPER_LIST, calUnits );
        
        //set the create unit as selected
        dataModel.setData( 
              ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER, 
              getCreatedCalculationUnit() );        
      }
    };
    KeyBasedDataModelUtil.postCommand( dataModel, cmd );
    
    this.createdCalculationUnit = cmd.getCreatedCalculationUnit();
    return true;
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( IWorkbench workbench, IStructuredSelection selection )
  {
    initialSelection = selection;
  }

  private static final QName getCUnitQName( String qNameKey ) throws RuntimeException
  {
    if( QNAME_KEY_1D.equals( qNameKey ) )
    {
      return Kalypso1D2DSchemaConstants.WB1D2D_F_CALC_UNIT_1D;
    }
    else if( QNAME_KEY_2D.equals( qNameKey ) )
    {

      return Kalypso1D2DSchemaConstants.WB1D2D_F_CALC_UNIT_2D;
    }
    else if( QNAME_KEY_1D2D.equals( qNameKey ) )
    {
      return Kalypso1D2DSchemaConstants.WB1D2D_F_CALC_UNIT_1D2D;
    }
    else
    {
      throw new RuntimeException( "Unknown qNameKey:" + qNameKey );
    }
  }
}
