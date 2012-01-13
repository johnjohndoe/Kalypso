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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit.action;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.resource.ImageDescriptor;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.ProfilChartView;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIImages;

import de.openali.odysseus.chart.framework.view.IChartComposite;
import de.openali.odysseus.chart.framework.view.IChartHandler;

public enum ProfilChartActionsEnum
{
  ZOOM_OUT(
      Messages.getString( "org.kalypso.model.wspm.ui.view.chart.action.ProfilChartActionsEnum_0" ), Messages.getString( "org.kalypso.model.wspm.ui.view.chart.action.ProfilChartActionsEnum_1" ), KalypsoModelWspmUIImages.ID_CHART_ZOOM_OUT, null, IAction.AS_RADIO_BUTTON), //$NON-NLS-1$ //$NON-NLS-2$
      ZOOM_IN(
          Messages.getString( "org.kalypso.model.wspm.ui.view.chart.action.ProfilChartActionsEnum_2" ), Messages.getString( "org.kalypso.model.wspm.ui.view.chart.action.ProfilChartActionsEnum_3" ), KalypsoModelWspmUIImages.ID_CHART_ZOOM_IN, null, IAction.AS_RADIO_BUTTON), //$NON-NLS-1$ //$NON-NLS-2$
          PAN(
              Messages.getString( "org.kalypso.model.wspm.ui.view.chart.action.ProfilChartActionsEnum_4" ), Messages.getString( "org.kalypso.model.wspm.ui.view.chart.action.ProfilChartActionsEnum_5" ), KalypsoModelWspmUIImages.ID_CHART_PAN, null, IAction.AS_RADIO_BUTTON), //$NON-NLS-1$ //$NON-NLS-2$
              EDIT(
                  Messages.getString( "org.kalypso.model.wspm.ui.view.chart.action.ProfilChartActionsEnum_6" ), Messages.getString( "org.kalypso.model.wspm.ui.view.chart.action.ProfilChartActionsEnum_7" ), KalypsoModelWspmUIImages.ID_CHART_EDIT, null, IAction.AS_RADIO_BUTTON), //$NON-NLS-1$ //$NON-NLS-2$
                  MAXIMIZE(
                      Messages.getString( "org.kalypso.model.wspm.ui.view.chart.action.ProfilChartActionsEnum_8" ), Messages.getString( "org.kalypso.model.wspm.ui.view.chart.action.ProfilChartActionsEnum_9" ), KalypsoModelWspmUIImages.ID_CHART_MAXIMIZE, null, IAction.AS_PUSH_BUTTON), //$NON-NLS-1$ //$NON-NLS-2$
                      EXPORT_IMAGE(
                          Messages.getString( "org.kalypso.model.wspm.ui.view.chart.action.ProfilChartActionsEnum_10" ), Messages.getString( "org.kalypso.model.wspm.ui.view.chart.action.ProfilChartActionsEnum_11" ), KalypsoModelWspmUIImages.ID_CHART_SCREENSHOT, null, IAction.AS_PUSH_BUTTON); //$NON-NLS-1$ //$NON-NLS-2$

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

  public static IAction createAction( final ProfilChartView profilView, final ProfilChartActionsEnum chartAction, final IChartHandler chartHandler )
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
        {
          final IChartComposite chart = profilView.getChartComposite();
          if( chart != null )
            chart.getPlotHandler().activatePlotHandler( chartHandler );
        }
      }
    };

    action.setToolTipText( chartAction.getTooltip() );
    action.setImageDescriptor( chartAction.getEnabledImage() );
    action.setDisabledImageDescriptor( chartAction.getDisabledImage() );

    return action;
  }
}