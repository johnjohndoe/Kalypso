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

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.ogc.gml.IKalypsoCascadingTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypso.risk.plugin.KalypsoRiskPlugin;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Gernot Belger
 */
public class ChangeEventAction extends Action implements IUpdateable
{
  private final IRasterDataModel m_model;

  private final IScenarioDataProvider m_dataProvider;

  private final TreeViewer m_eventViewer;

  private final IMapPanel m_mapPanel;

  public ChangeEventAction( final IRasterDataModel model, final IScenarioDataProvider dataProvider, final TreeViewer eventViewer, final IMapPanel mapPanel )
  {
    super( Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.WaterdepthCollectionsManagementWidget.12" ) ); //$NON-NLS-1$

    m_mapPanel = mapPanel;

    setImageDescriptor( KalypsoRiskPlugin.getImageProvider().getImageDescriptor( "icons/etool16/event_edit.gif" ) ); //$NON-NLS-1$

    m_model = model;
    m_dataProvider = dataProvider;
    m_eventViewer = eventViewer;
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
        if( newText == null || newText.length() == 0 )
          return Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.WaterdepthCollectionsManagementWidget.38" ); //$NON-NLS-1$

        try
        {
          final int i = Integer.parseInt( newText );
          if( i <= 0 )
            return Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.WaterdepthCollectionsManagementWidget.39" ); //$NON-NLS-1$

          for( final IAnnualCoverageCollection collection : model.getWaterlevelCoverageCollection() )
          {
            if( collection.getReturnPeriod() == i )
              return Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.WaterdepthCollectionsManagementWidget.40" ); //$NON-NLS-1$
          }
        }
        catch( final NumberFormatException e )
        {
          return Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.WaterdepthCollectionsManagementWidget.41" ); //$NON-NLS-1$
        }

        return null;
      }
    };

    // show input dialog
    final Shell shell = event.display.getActiveShell();
    final InputDialog dialog = new InputDialog( shell, Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.WaterdepthCollectionsManagementWidget.43" ), Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.WaterdepthCollectionsManagementWidget.44" ), "", inputValidator ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    if( dialog.open() != Window.OK )
      return;

    final IKalypsoCascadingTheme wspThemes = RiskModelHelper.getHQiTheme( m_mapPanel.getMapModell() );
    Assert.isNotNull( wspThemes, Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.WaterdepthCollectionsManagementWidget.47" ) ); //$NON-NLS-1$

    final IStructuredSelection selection = (IStructuredSelection) m_eventViewer.getSelection();
    final IAnnualCoverageCollection selectedElement = (IAnnualCoverageCollection) selection.getFirstElement();

    final ICoreRunnableWithProgress operation = new ChangeAnnualityOperation( selectedElement, Integer.parseInt( dialog.getValue() ), model, wspThemes, m_dataProvider );

    final IStatus resultStatus = ProgressUtilities.busyCursorWhile( operation );
    if( !resultStatus.isOK() )
      KalypsoRiskPlugin.getDefault().getLog().log( resultStatus );

    if( !resultStatus.isOK() )
      ErrorDialog.openError( shell, Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.WaterdepthCollectionsManagementWidget.48" ), Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.WaterdepthCollectionsManagementWidget.49" ), resultStatus ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public void update( )
  {
    final IStructuredSelection selection = (IStructuredSelection) m_eventViewer.getSelection();

    final boolean enabled = selection.getFirstElement() instanceof IAnnualCoverageCollection;

    setEnabled( enabled );
  }
}