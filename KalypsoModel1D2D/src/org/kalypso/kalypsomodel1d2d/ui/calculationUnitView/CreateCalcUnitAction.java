/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.kalypsomodel1d2d.ui.calculationUnitView;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.wizards.CreateCalculationUnitWizard;

/**
 * @author Gernot Belger
 */
class CreateCalcUnitAction extends Action
{
  private final CalculationUnitDataModel m_dataModel;

  public CreateCalcUnitAction( final CalculationUnitDataModel dataModel )
  {
    m_dataModel = dataModel;

    setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.CalculationUnitMetaTable.Tooltip.BTN_ADD" ) ); //$NON-NLS-1$

    setImageDescriptor( AbstractUIPlugin.imageDescriptorFromPlugin( KalypsoModel1D2DPlugin.PLUGIN_ID, "icons/elcl16/18_add_calculationunit.gif" ) );//$NON-NLS-1$
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final CreateCalculationUnitWizard calculationWizard = new CreateCalculationUnitWizard( m_dataModel );

    final Shell shell = event.widget.getDisplay().getActiveShell();

    final WizardDialog wizardDialog = new WizardDialog( shell, calculationWizard );
    wizardDialog.open();
  }
}
