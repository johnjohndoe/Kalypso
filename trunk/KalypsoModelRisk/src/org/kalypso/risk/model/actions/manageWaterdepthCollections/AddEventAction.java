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
package org.kalypso.risk.model.actions.manageWaterdepthCollections;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.plugin.KalypsoRiskPlugin;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Gernot Belger
 */
public class AddEventAction extends Action implements IUpdateable
{
  private final IRasterDataModel m_model;

  private final TreeViewer m_eventViewer;

  private final IScenarioDataProvider m_dataProvider;

  public AddEventAction( final IRasterDataModel model, final IScenarioDataProvider dataProvider, final TreeViewer eventViewer )
  {
    super( Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.WaterdepthCollectionsManagementWidget.56" ) ); //$NON-NLS-1$

    m_model = model;
    m_dataProvider = dataProvider;
    m_eventViewer = eventViewer;

    setImageDescriptor( KalypsoRiskPlugin.getImageProvider().getImageDescriptor( "icons/etool16/event_add.gif" ) ); //$NON-NLS-1$
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final IRasterDataModel model = m_model;
    final IInputValidator inputValidator = new IInputValidator()
    {
      @Override
      public String isValid( final String newText )
      {
        if( StringUtils.isEmpty( newText ) )
          return Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.WaterdepthCollectionsManagementWidget.25" ); //$NON-NLS-1$

        try
        {
          final int i = Integer.parseInt( newText );
          if( i <= 0 )
            return Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.WaterdepthCollectionsManagementWidget.26" ); //$NON-NLS-1$
          for( final IAnnualCoverageCollection collection : model.getWaterlevelCoverageCollection() )
            if( collection.getReturnPeriod() == i )
              return Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.WaterdepthCollectionsManagementWidget.27" ); //$NON-NLS-1$
        }
        catch( final NumberFormatException e )
        {
          return Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.WaterdepthCollectionsManagementWidget.28" ); //$NON-NLS-1$
        }

        return null;
      }
    };

    // show input dialog
    final Shell shell = event.display.getActiveShell();
    final InputDialog dialog = new InputDialog( shell, Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.WaterdepthCollectionsManagementWidget.30" ), Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.WaterdepthCollectionsManagementWidget.31" ), "", inputValidator ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    if( dialog.open() != Window.OK )
      return;

    final String eventName = "HQ " + dialog.getValue(); //$NON-NLS-1$

    final AddCollectionOperation operation = new AddCollectionOperation( eventName, Integer.parseInt( dialog.getValue() ), model, m_dataProvider );

    final IStatus resultStatus = ProgressUtilities.busyCursorWhile( operation );
    if( resultStatus.isOK() )
    {
      /* Select newly created event */
      final StructuredSelection structuredSelection = new StructuredSelection( operation.getNewFeature() );
      m_eventViewer.setSelection( structuredSelection );
    }
    else
    {
      KalypsoRiskPlugin.getDefault().getLog().log( resultStatus );
      ErrorDialog.openError( shell, Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.WaterdepthCollectionsManagementWidget.36" ), Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.WaterdepthCollectionsManagementWidget.37" ), resultStatus ); //$NON-NLS-1$ //$NON-NLS-2$
    }
  }

  @Override
  public void update( )
  {
    // always enabled

    // final IStructuredSelection selection = (IStructuredSelection) m_eventViewer.getSelection();
    //
    // final boolean enabled = selection.getFirstElement() instanceof IAnnualCoverageCollection;
    //
    // setEnabled( enabled );
  }
}
