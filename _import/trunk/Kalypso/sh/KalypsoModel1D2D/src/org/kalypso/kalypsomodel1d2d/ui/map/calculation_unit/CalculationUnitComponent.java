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
package org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit;

import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.ICommand;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.kalypsomodel1d2d.ops.CalUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.editor.FeatureWrapperListEditor;
import org.kalypso.kalypsomodel1d2d.ui.map.editor.IButtonConstants;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModel;

/**
 * 
 * @author Madanagopal
 * @author Patrice Congo
 *
 */
public class CalculationUnitComponent 
      extends FeatureWrapperListEditor 
      implements IButtonConstants
{

  public CalculationUnitComponent()
  {
    super(null,null,null);
    setRequiredButtons( BTN_CLICK_TO_RUN,
                        BTN_REMOVE,
                        BTN_ADD);
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.editor.FeatureWrapperListEditor#templateDialog()
   */
  @Override
  public void createFeatureWrapper()
  {
      final Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
      CreateCalculationUnitDialog calculationDialog = 
                    new CreateCalculationUnitDialog( shell, getDataModel() );
      int answer = calculationDialog.open();
      if( answer == Window.OK )
      {
        KeyBasedDataModel dataModel = getDataModel();
        dataModel.setData( 
            ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER, 
            calculationDialog.getCreatedCalculationUnit() );
        IFEDiscretisationModel1d2d model1d2d =
          (IFEDiscretisationModel1d2d) 
              dataModel.getData( ICommonKeys.KEY_DISCRETISATION_MODEL );
        List<ICalculationUnit> calUnits = 
                          CalUnitOps.getModelCalculationUnits( model1d2d );
        dataModel.setData( ICommonKeys.KEY_FEATURE_WRAPPER_LIST, calUnits );
      }
  }  
}
