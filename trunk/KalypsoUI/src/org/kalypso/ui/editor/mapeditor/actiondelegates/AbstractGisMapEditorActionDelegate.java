/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.editor.mapeditor.actiondelegates;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchPart;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypso.ui.editor.AbstractGisEditorActionDelegate;

/**
 * @author doemming
 */
public abstract class AbstractGisMapEditorActionDelegate extends AbstractGisEditorActionDelegate
{
  private final IWidget m_widget;

  public AbstractGisMapEditorActionDelegate( final IWidget widget )
  {
    m_widget = widget;
  }

  /**
   * @see org.kalypso.ui.editor.AbstractGisEditorActionDelegate#setActivePart(org.eclipse.jface.action.IAction,
   *      org.eclipse.ui.IWorkbenchPart)
   */
  @Override
  protected void setActivePart( final IAction action, final IWorkbenchPart part )
  {
    super.setActivePart( action, part );

    if( action != null && action.getStyle() == IAction.AS_RADIO_BUTTON )
    {
      final WidgetActionPart widgetPart = getPart();
      if( widgetPart != null && action != null )
      {
        final MapPanel mapPanel = widgetPart.getMapPanel();
        if( mapPanel != null )
        {
          final IWidget actualWidget = mapPanel.getWidgetManager().getActualWidget();
          action.setChecked( actualWidget == m_widget );

          // HACK: even reactivate the widget, as there is often only one widget per map
          // so we set the current mapPanel to the current widget
          if( actualWidget == m_widget )
            mapPanel.getWidgetManager().setActualWidget( actualWidget );
        }
      }
    }
  }

  protected final IWidget getWidget( )
  {
    return m_widget;
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    // activate my widget
    final WidgetActionPart part = getPart();
    if( part == null )
      return;

    final MapPanel mapPanel = part.getMapPanel();
    if( mapPanel == null )
      return;

    if( action.isChecked() )
      mapPanel.getWidgetManager().setActualWidget( getWidget() );
  }

  /**
   * The default implementation does nothing
   * 
   * @see org.kalypso.ui.editor.AbstractGisEditorActionDelegate#refreshAction(org.eclipse.jface.action.IAction)
   */
  @Override
  protected void refreshAction( final IAction action, final ISelection selection )
  {
    final WidgetActionPart part = getPart();
    final MapPanel mapPanel = part == null ? null : part.getMapPanel();

    final boolean isEnabled = getWidget().canBeActivated( selection, mapPanel );
    if( action != null )
      action.setEnabled( isEnabled );
  }
}