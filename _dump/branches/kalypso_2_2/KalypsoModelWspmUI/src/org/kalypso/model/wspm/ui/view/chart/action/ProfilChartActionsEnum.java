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
package org.kalypso.model.wspm.ui.view.chart.action;

import java.util.Collections;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.EvaluationContext;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.ISources;
import org.kalypso.chart.ui.editor.commandhandler.ChartHandlerUtilities;
import org.kalypso.chart.ui.editor.mousehandler.IAxisDragHandler;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIImages;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartView;

import de.openali.odysseus.chart.framework.view.IChartDragHandler;

public enum ProfilChartActionsEnum
{
  ZOOM_OUT(org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.view.chart.action.ProfilChartActionsEnum_0"), org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.view.chart.action.ProfilChartActionsEnum_1"), KalypsoModelWspmUIImages.ID_CHART_ZOOM_OUT, null, IAction.AS_RADIO_BUTTON), //$NON-NLS-1$ //$NON-NLS-2$
  ZOOM_IN(org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.view.chart.action.ProfilChartActionsEnum_2"), org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.view.chart.action.ProfilChartActionsEnum_3"), KalypsoModelWspmUIImages.ID_CHART_ZOOM_IN, null, IAction.AS_RADIO_BUTTON), //$NON-NLS-1$ //$NON-NLS-2$
  PAN(org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.view.chart.action.ProfilChartActionsEnum_4"), org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.view.chart.action.ProfilChartActionsEnum_5"), KalypsoModelWspmUIImages.ID_CHART_PAN, null, IAction.AS_RADIO_BUTTON), //$NON-NLS-1$ //$NON-NLS-2$
  EDIT(org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.view.chart.action.ProfilChartActionsEnum_6"), org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.view.chart.action.ProfilChartActionsEnum_7"), KalypsoModelWspmUIImages.ID_CHART_EDIT, null, IAction.AS_RADIO_BUTTON), //$NON-NLS-1$ //$NON-NLS-2$
  MAXIMIZE(org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.view.chart.action.ProfilChartActionsEnum_8"), org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.view.chart.action.ProfilChartActionsEnum_9"), KalypsoModelWspmUIImages.ID_CHART_MAXIMIZE, null, IAction.AS_PUSH_BUTTON), //$NON-NLS-1$ //$NON-NLS-2$
  EXPORT_IMAGE(org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.view.chart.action.ProfilChartActionsEnum_10"), org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.view.chart.action.ProfilChartActionsEnum_11"), KalypsoModelWspmUIImages.ID_CHART_SCREENSHOT, null, IAction.AS_PUSH_BUTTON); //$NON-NLS-1$ //$NON-NLS-2$

  private final String m_label;

  private final String m_tooltip;

  private final ImageDescriptor m_enabledImage;

  private final ImageDescriptor m_disabledImage;

  private final int m_style;

  private final String m_menuPath;

  ProfilChartActionsEnum( final String label, final String tooltip, final ImageDescriptor enabledImage, final ImageDescriptor disabledImage, final int style )
  {
    this( label, tooltip, enabledImage, disabledImage, style, null );
  }

  ProfilChartActionsEnum( final String label, final String tooltip, final ImageDescriptor enabledImage, final ImageDescriptor disabledImage, final int style, final String menuPath )
  {
    m_label = label;
    m_tooltip = tooltip;
    m_enabledImage = enabledImage;
    m_disabledImage = disabledImage;
    m_style = style;
    m_menuPath = menuPath;
  }

  public ImageDescriptor getDisabledImage( )
  {
    return m_disabledImage;
  }

  public String getMenuPath( )
  {
    return m_menuPath;
  }

  public ImageDescriptor getEnabledImage( )
  {
    return m_enabledImage;
  }

  public int getStyle( )
  {
    return m_style;
  }

  public String getTooltip( )
  {
    return m_tooltip;
  }

  @Override
  public String toString( )
  {
    return m_label;
  }

  public static IAction createAction( final ProfilChartView profilView, final ProfilChartActionsEnum chartAction, final IChartDragHandler chartHandler, final IAxisDragHandler axisHandler )
  {
    final int style = chartAction.getStyle();
    final String label = chartAction.toString();

    final IAction action = new Action( label, style )
    {
      /**
       * @see org.eclipse.jface.action.Action#run()
       */
      @Override
      public void run( )
      {
        if( chartHandler != null )
          profilView.getPlotDragHandler().setActiveHandler( chartHandler );
        if( axisHandler != null )
          profilView.getAxisDragHandler().setActiveHandler( axisHandler );
      }
    };

    action.setToolTipText( chartAction.getTooltip() );
    action.setImageDescriptor( chartAction.getEnabledImage() );
    action.setDisabledImageDescriptor( chartAction.getDisabledImage() );

    return action;
  }

  public static IAction createAction( final ProfilChartView profilView, final ProfilChartActionsEnum chartAction, final AbstractHandler chartHandler )
  {
    final int style = chartAction.getStyle();
    final String label = chartAction.toString();

    final IAction action = new Action( label, style )
    {
      /**
       * @see org.eclipse.jface.action.Action#run()
       */
      @Override
      public void run( )
      {
        final EvaluationContext context = new EvaluationContext( null, this );
        context.addVariable( ChartHandlerUtilities.ACTIVE_CHART_PART_NAME, profilView );
        if( profilView != null && profilView.getChart()!=null)
          context.addVariable( ISources.ACTIVE_SHELL_NAME, profilView.getChart().getDisplay().getActiveShell() );
        final ExecutionEvent event = new ExecutionEvent( null, Collections.EMPTY_MAP, null, context );

        try
        {
          chartHandler.execute( event );
        }
        catch( ExecutionException e )
        {
          throw new RuntimeException( e );
        }
      }
    };

    action.setToolTipText( chartAction.getTooltip() );
    action.setImageDescriptor( chartAction.getEnabledImage() );
    action.setDisabledImageDescriptor( chartAction.getDisabledImage() );

    return action;
  }
}