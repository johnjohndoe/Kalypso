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

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.ogc.gml.IKalypsoCascadingTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypso.risk.plugin.KalypsoRiskPlugin;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Gernot Belger
 */
public class RemoveEventAction extends Action implements IUpdateable
{
  private final IScenarioDataProvider m_dataProvider;

  private final TreeViewer m_eventViewer;

  private final IMapPanel m_mapPanel;

  public RemoveEventAction( final IScenarioDataProvider dataProvider, final TreeViewer eventViewer, final IMapPanel mapPanel )
  {
    super( Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.WaterdepthCollectionsManagementWidget.14" ) ); //$NON-NLS-1$

    m_dataProvider = dataProvider;
    m_eventViewer = eventViewer;
    m_mapPanel = mapPanel;

    setImageDescriptor( KalypsoRiskPlugin.getImageProvider().getImageDescriptor( "icons/etool16/event_remove.gif" ) ); //$NON-NLS-1$
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final IStructuredSelection selection = (IStructuredSelection) m_eventViewer.getSelection();
    if( selection.isEmpty() )
      return;

    final IKalypsoCascadingTheme wspThemes = RiskModelHelper.getHQiTheme( m_mapPanel.getMapModell() );
    if( wspThemes == null )
      return;

    final Object[] selectedElements = selection.toArray();
    if( selectedElements.length != 1 )
      return;

    final Shell shell = event.display.getActiveShell();

    final String selectedLabel = ((ILabelProvider) m_eventViewer.getLabelProvider()).getText( selectedElements[0] );
    final String deleteQuestion = String.format( Messages.getString("RemoveEventAction.0"), selectedLabel ); //$NON-NLS-1$
    if( !MessageDialog.openConfirm( shell, getText(), deleteQuestion ) )
      return;

    final ICoreRunnableWithProgress operation = new RemoveCollectionOperation( selectedElements, m_dataProvider, wspThemes );

    final IStatus resultStatus = ProgressUtilities.busyCursorWhile( operation );
    if( !resultStatus.isOK() )
    {
      KalypsoRiskPlugin.getDefault().getLog().log( resultStatus );
      StatusDialog.open( shell, resultStatus, getText() );
    }
  }

  @Override
  public void update( )
  {
    final IStructuredSelection selection = (IStructuredSelection) m_eventViewer.getSelection();

    final boolean enabled = selection.getFirstElement() instanceof IAnnualCoverageCollection;

    setEnabled( enabled );
  }
}