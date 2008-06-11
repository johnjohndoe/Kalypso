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
package org.kalypso.model.wspm.ui.view.chart.action;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.resource.ImageDescriptor;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIImages;
import org.kalypso.model.wspm.ui.Messages;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartView;

public enum ProfilChartActionsEnum
{
  ZOOM_OUT(Messages.ProfilChartActionsEnum_0, Messages.ProfilChartActionsEnum_1, KalypsoModelWspmUIImages.ID_CHART_ZOOM_OUT, null, IAction.AS_RADIO_BUTTON),
  ZOOM_IN(Messages.ProfilChartActionsEnum_2, Messages.ProfilChartActionsEnum_3, KalypsoModelWspmUIImages.ID_CHART_ZOOM_IN, null, IAction.AS_RADIO_BUTTON),
  PAN(Messages.ProfilChartActionsEnum_4, Messages.ProfilChartActionsEnum_5, KalypsoModelWspmUIImages.ID_CHART_PAN, null, IAction.AS_RADIO_BUTTON),
// TODO: KIM Ansicht Seitenverhältnis überarbeiten
// FIX_RATIO("Seitenverhältnis", "Seitenverhältnis fixieren", KalypsoModelWspmUIImages.ID_CHART_FIX_RATIO, null,
// IAction.AS_DROP_DOWN_MENU),
// FIX_RATIO_0("auto", "Seitenverhältnis anpassen", KalypsoModelWspmUIImages.ID_CHART_FIX_RATIO, null,
// IAction.AS_RADIO_BUTTON, "FIX_RATIO"),
// FIX_RATIO_1("1:1", "Seitenverhältnis fixieren", KalypsoModelWspmUIImages.ID_CHART_FIX_RATIO, null,
// IAction.AS_RADIO_BUTTON, "FIX_RATIO"),
// FIX_RATIO_2("1:2", "Seitenverhältnis fixieren", KalypsoModelWspmUIImages.ID_CHART_FIX_RATIO, null,
// IAction.AS_RADIO_BUTTON, "FIX_RATIO"),
// FIX_RATIO_3("1:5", "Seitenverhältnis fixieren", KalypsoModelWspmUIImages.ID_CHART_FIX_RATIO, null,
// IAction.AS_RADIO_BUTTON, "FIX_RATIO"),

  EDIT(Messages.ProfilChartActionsEnum_6, Messages.ProfilChartActionsEnum_7, KalypsoModelWspmUIImages.ID_CHART_EDIT, null, IAction.AS_CHECK_BOX),
  MAXIMIZE(Messages.ProfilChartActionsEnum_8, Messages.ProfilChartActionsEnum_9, KalypsoModelWspmUIImages.ID_CHART_MAXIMIZE, null, IAction.AS_PUSH_BUTTON),
  EXPORT_IMAGE(Messages.ProfilChartActionsEnum_10, Messages.ProfilChartActionsEnum_11, KalypsoModelWspmUIImages.ID_CHART_SCREENSHOT, null, IAction.AS_PUSH_BUTTON);

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

  public static IAction createAction( final ProfilChartView profilView, final ProfilChartActionsEnum chartAction )
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
        profilView.runChartAction( chartAction );
      }
    };

    action.setToolTipText( chartAction.getTooltip() );
    action.setImageDescriptor( chartAction.getEnabledImage() );
    action.setDisabledImageDescriptor( chartAction.getDisabledImage() );

    return action;
  }

}