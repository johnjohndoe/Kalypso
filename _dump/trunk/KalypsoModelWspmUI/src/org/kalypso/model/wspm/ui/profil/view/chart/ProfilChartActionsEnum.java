/**
 * 
 */
package org.kalypso.model.wspm.ui.profil.view.chart;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.resource.ImageDescriptor;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIImages;

public enum ProfilChartActionsEnum
{
  ZOOM_OUT("Verkleinern", "Ausschnit verkleinern", KalypsoModelWspmUIImages.ID_CHART_ZOOM_OUT, null, IAction.AS_RADIO_BUTTON),
  ZOOM_IN("Vergrössern", "Ausschnitt vergrössern", KalypsoModelWspmUIImages.ID_CHART_ZOOM_IN, null, IAction.AS_RADIO_BUTTON),
  PAN("Verschieben", "Ausschnitt verschieben", KalypsoModelWspmUIImages.ID_CHART_PAN, null, IAction.AS_RADIO_BUTTON),

  FIX_RATIO("Seitenverhältnis", "Seitenverhältnis fixieren", KalypsoModelWspmUIImages.ID_CHART_FIX_RATIO, null, IAction.AS_DROP_DOWN_MENU),
  FIX_RATIO_0("auto", "Seitenverhältnis anpassen", KalypsoModelWspmUIImages.ID_CHART_FIX_RATIO, null, IAction.AS_RADIO_BUTTON, "FIX_RATIO"),
  FIX_RATIO_1("1:1", "Seitenverhältnis fixieren", KalypsoModelWspmUIImages.ID_CHART_FIX_RATIO, null, IAction.AS_RADIO_BUTTON, "FIX_RATIO"),
  FIX_RATIO_2("1:2", "Seitenverhältnis fixieren", KalypsoModelWspmUIImages.ID_CHART_FIX_RATIO, null, IAction.AS_RADIO_BUTTON, "FIX_RATIO"),
  FIX_RATIO_3("1:5", "Seitenverhältnis fixieren", KalypsoModelWspmUIImages.ID_CHART_FIX_RATIO, null, IAction.AS_RADIO_BUTTON, "FIX_RATIO"),

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