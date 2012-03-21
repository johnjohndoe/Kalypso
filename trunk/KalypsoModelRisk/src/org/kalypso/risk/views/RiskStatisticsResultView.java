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
package org.kalypso.risk.views;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.ogc.gml.featureview.control.EmbeddedToolbarExecutionListener;
import org.kalypso.ogc.gml.om.table.command.ToolbarCommandUtils;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.plugin.KalypsoRiskPlugin;
import org.kalypso.risk.widget.StatisticResultComposite;

import de.renew.workflow.connector.cases.CaseHandlingSourceProvider;
import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Thomas Jung
 */
public class RiskStatisticsResultView extends ViewPart
{
  private Control m_control;

  private EmbeddedToolbarExecutionListener m_executionListener;

  @Override
  public void createPartControl( final Composite parent )
  {
    try
    {
      final IHandlerService handlerService = (IHandlerService) getSite().getService( IHandlerService.class );
      final IEvaluationContext context = handlerService.getCurrentState();
      final IScenarioDataProvider modelProvider = (IScenarioDataProvider) context.getVariable( CaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
      final IRasterizationControlModel model = modelProvider.getModel( IRasterizationControlModel.class.getName() );

      if( model == null )
      {
        final Label label = new Label( parent, SWT.NONE );
        label.setText( Messages.getString( "org.kalypso.risk.views.RiskStatisticsResultView.0" ) ); //$NON-NLS-1$
        m_control = label;
      }
      else
      {
        final StatisticResultComposite resultComposite = new StatisticResultComposite( model, parent, SWT.BORDER );
        m_control = resultComposite;

        final IWorkbench serviceLocator = PlatformUI.getWorkbench();

        final IActionBars actionBars = getViewSite().getActionBars();
        final IToolBarManager toolBarManager = actionBars.getToolBarManager();

        final TableViewer tableViewer = resultComposite.getTableViewer();
        m_executionListener = new EmbeddedToolbarExecutionListener( toolBarManager, serviceLocator );
        m_executionListener.addContextVariable( ToolbarCommandUtils.ACTIVE_TUPLE_RESULT_TABLE_VIEWER_NAME, tableViewer );
      }
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
      final StatusComposite statusComposite = new StatusComposite( parent, StatusComposite.DETAILS );
      final IStatus status = new Status( IStatus.ERROR, KalypsoRiskPlugin.PLUGIN_ID, "Failed to initialize statistic result view", e );
      statusComposite.setStatus( status );
      m_control = statusComposite;

    }
  }

  @Override
  public void setFocus( )
  {
    if( m_control != null )
      m_control.setFocus();
  }

  @Override
  public void dispose( )
  {
    if( m_control != null )
    {
      m_control.dispose();
      m_control = null;
    }

    if( m_executionListener != null )
    {
      m_executionListener.dispose();
      m_executionListener = null;
    }

    super.dispose();
  }
}