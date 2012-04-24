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
package org.kalypso.ui.rrm.internal.cm.view.action;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.rcm.binding.ILinearSumGenerator;
import org.kalypso.ui.rrm.internal.IUiRrmWorkflowConstants;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.UIRrmImages.DESCRIPTORS;
import org.kalypso.ui.rrm.internal.calccase.CatchmentModelHelper;
import org.kalypso.ui.rrm.internal.cm.idw.IdwLinearSumHelper;
import org.kalypso.ui.rrm.internal.cm.view.LinearSumBean;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeModel;

/**
 * @author Gernot Belger
 * @author Holger Albert
 */
public class EditLinearSumIdwAction extends Action
{
  private final ITreeNodeModel m_model;

  private final ILinearSumGenerator m_generator;

  public EditLinearSumIdwAction( final ITreeNodeModel model, final ILinearSumGenerator generator )
  {
    m_model = model;
    m_generator = generator;

    setText( Messages.getString( "EditLinearSumIdwAction_0" ) ); //$NON-NLS-1$
    setToolTipText( Messages.getString( "EditLinearSumIdwAction_1" ) ); //$NON-NLS-1$
    setImageDescriptor( UIRrmImages.id( DESCRIPTORS.GENERATOR_EDIT_IDW ) );
  }

  @Override
  public void runWithEvent( final Event event )
  {
    /* Get the shell. */
    final Shell shell = event.widget.getDisplay().getActiveShell();

    try
    {
      /* Get the data provider. */
      final SzenarioDataProvider dataProvider = ScenarioHelper.getScenarioDataProvider();
      final NaModell model = dataProvider.getModel( IUiRrmWorkflowConstants.SCENARIO_DATA_MODEL );

      /* Compare the catchments of the model and the catchments of the generator. */
      final IStatus status = CatchmentModelHelper.compareCatchments( model, m_generator );

      /* The linear sum bean. */
      LinearSumBean bean = null;

      /* Check the status. */
      if( status.isOK() )
      {
        /* Create the linear sum bean. */
        bean = new LinearSumBean( m_generator );
      }
      else
      {
        /* Show a warning and ask the user, if the catchments should be rearranged. */
        final String message = String.format( "%s. This could have happend because the model has changed. The catchments will be newly initialized...", status.getMessage() );
        MessageDialog.openInformation( shell, getText(), message );

        /* Create the linear sum bean. */
        bean = LinearSumBean.reinitFromModel( model, m_generator );
      }

      /* Show the wizard. */
      IdwLinearSumHelper.showWizard( shell, bean, m_model, getText() );
    }
    catch( final CoreException ex )
    {
      ex.printStackTrace();
      ErrorDialog.openError( shell, getText(), "Failed to edit the catchment model...", ex.getStatus() );
    }
  }
}