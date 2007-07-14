/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.kalypsomodel1d2d.ui.calculationUnitView;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DUIImages;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.sim.CalculationUnitSimMode1D2DCalcJob;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitViewerLabelProvider;
import org.kalypso.kalypsomodel1d2d.ui.map.editor.FeatureWrapperListEditor;
import org.kalypso.kalypsomodel1d2d.ui.map.editor.IButtonConstants;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

/**
 * @author Patrice Congo
 * @author Madanagopal
 * 
 */
@SuppressWarnings("unchecked")
public class CalculationUnitPerformComponent extends FeatureWrapperListEditor implements IButtonConstants
{
  private final CalculationUnitDataModel dataModel;

  private final Map<String, String> btnDescription = new HashMap<String, String>();

  private final Action performCalButton = new Action( "Perform", KalypsoModel1D2DUIImages.IMG_RUN_SIM )
  {
    /**
     * @see org.eclipse.jface.action.Action#runWithEvent(org.eclipse.swt.widgets.Event)
     */
    @Override
    public void runWithEvent( final Event event )
    {
      final Shell shell = event.display.getActiveShell();

      final ICalculationUnit unit = dataModel.getSelectedCalculationUnit();
      if( unit != null )
      {
        final IWorkbench workbench = PlatformUI.getWorkbench();
        final IStatus result = CalculationUnitSimMode1D2DCalcJob.startCalculation( unit, workbench );
        ErrorDialog.openError( shell, "Berechnung durchführen", "Berechnung konnte nicht durchgeführt werden.", result );
      }

      // TODO: message if else

    }

    /**
     * @see org.eclipse.jface.action.Action#getToolTipText()
     */
    @Override
    public String getToolTipText( )
    {
      String toolTipText2 = super.getToolTipText();
      if( toolTipText2 == null )
      {
        toolTipText2 = "Berechnung Starten";
      }
      return toolTipText2;
    }
  };

  public CalculationUnitPerformComponent( final CalculationUnitDataModel dataModel )
  {
    super( null, null, null );
    setRequiredButtons( BTN_SHOW_AND_MAXIMIZE
    /* BTN_CLICK_TO_CALCULATE */);

    btnDescription.put( "SHOW_AND_MAXIMIZE", "Berechnungseinheit anzeigen und maximieren" );
    setNonGenericActions( new IAction[] { performCalButton } );
    this.dataModel = dataModel;
  }

  @Override
  protected String getBtnDescription( final String key )
  {
    if( btnDescription.get( key ) != null )
      return btnDescription.get( key );
    else
      return null;
  }

  @Override
  protected ILabelProvider getLabelProvider( final Display display )
  {
    return new CalculationUnitViewerLabelProvider( display );
  }

  @Override
  protected boolean showDescription( )
  {
    return false;
  }

  @Override
  protected List<ICalculationUnit> setInputContentProvider( )
  {
    Object inputData = dataModel.getData( ICommonKeys.KEY_FEATURE_WRAPPER_LIST );
    if( inputData == null )
    {
      inputData = new ArrayList<IFeatureWrapper2>();
      return (List<ICalculationUnit>) inputData;
    }
    final List<ICalculationUnit> calcList = (List<ICalculationUnit>) dataModel.getData( ICommonKeys.KEY_FEATURE_WRAPPER_LIST );
    return calcList;
  }
}
