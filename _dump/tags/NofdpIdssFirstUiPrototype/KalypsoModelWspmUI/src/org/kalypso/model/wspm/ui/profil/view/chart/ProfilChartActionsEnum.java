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
package org.kalypso.model.wspm.ui.profil.view.chart;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.resource.ImageDescriptor;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIImages;

public enum ProfilChartActionsEnum
{
  ZOOM_OUT("Verkleinern", "Ausschnit verkleinern", KalypsoModelWspmUIImages.ID_CHART_ZOOM_OUT, null, IAction.AS_RADIO_BUTTON),
  ZOOM_IN("Vergr�ssern", "Ausschnitt vergr�ssern", KalypsoModelWspmUIImages.ID_CHART_ZOOM_IN, null, IAction.AS_RADIO_BUTTON),
  PAN("Verschieben", "Ausschnitt verschieben", KalypsoModelWspmUIImages.ID_CHART_PAN, null, IAction.AS_RADIO_BUTTON),

  FIX_RATIO("Seitenverh�ltnis", "Seitenverh�ltnis fixieren", KalypsoModelWspmUIImages.ID_CHART_FIX_RATIO, null, IAction.AS_DROP_DOWN_MENU),
  FIX_RATIO_0("auto", "Seitenverh�ltnis anpassen", KalypsoModelWspmUIImages.ID_CHART_FIX_RATIO, null, IAction.AS_RADIO_BUTTON, "FIX_RATIO"),
  FIX_RATIO_1("1:1", "Seitenverh�ltnis fixieren", KalypsoModelWspmUIImages.ID_CHART_FIX_RATIO, null, IAction.AS_RADIO_BUTTON, "FIX_RATIO"),
  FIX_RATIO_2("1:2", "Seitenverh�ltnis fixieren", KalypsoModelWspmUIImages.ID_CHART_FIX_RATIO, null, IAction.AS_RADIO_BUTTON, "FIX_RATIO"),
  FIX_RATIO_3("1:5", "Seitenverh�ltnis fixieren", KalypsoModelWspmUIImages.ID_CHART_FIX_RATIO, null, IAction.AS_RADIO_BUTTON, "FIX_RATIO"),

  EDIT("Editeren", "Daten editieren", KalypsoModelWspmUIImages.ID_CHART_EDIT, null, IAction.AS_CHECK_BOX),
  MAXIMIZE("Maximieren", "Ausschnit maximieren", KalypsoModelWspmUIImages.ID_CHART_MAXIMIZE, null, IAction.AS_PUSH_BUTTON),
  EXPORT_IMAGE("Bild exportieren...", "Aktuellen Ausschnitt als Bild speichern", KalypsoModelWspmUIImages.ID_CHART_EXPORT, null, IAction.AS_PUSH_BUTTON);

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
}