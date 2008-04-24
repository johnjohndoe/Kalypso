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
package org.kalypso.ui.editor.mapeditor.views;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.contribs.eclipse.ui.partlistener.AdapterPartListener;
import org.kalypso.contribs.eclipse.ui.partlistener.EditorFirstAdapterFinder;
import org.kalypso.contribs.eclipse.ui.partlistener.IAdapterEater;
import org.kalypso.contribs.eclipse.ui.partlistener.IAdapterFinder;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypso.ogc.gml.widgets.IWidgetChangeListener;

/**
 * ActionOptionsView is a view on the selected widget of an active map view. It provides a panel where the selected
 * widgets can place GUI elements for their options.
 * 
 * @author doemming
 */
public class MapWidgetView extends ViewPart
{
  public static final String ID = "org.kalypso.ui.editor.mapeditor.views.ActionOptionsView";

  private final static String DATA_PANEL = "panel";

  private final static class WidgetInfo
  {
    private Composite m_composite;

    private final IWidgetWithOptions m_widget;

    public WidgetInfo( final Composite composite, final IWidgetWithOptions widget )
    {
      m_composite = composite;
      m_widget = widget;
    }

    public Composite getComposite( )
    {
      return m_composite;
    }

    public IWidgetWithOptions getWidget( )
    {
      return m_widget;
    }

    public void setComposite( final Composite composite )
    {
      m_composite = composite;
    }
  }

  /**
   * A map to remember the active widgets per map-part. For each part, there is at most one widget for this widget-view.
   */
  private final Map<MapPanel, WidgetInfo> m_widgetInfos = new HashMap<MapPanel, WidgetInfo>();

  private final IAdapterEater<MapPanel> m_adapterEater = new IAdapterEater<MapPanel>()
  {
    public void setAdapter( final IWorkbenchPart part, final MapPanel panel )
    {
      mapActivated( panel );
    }
  };

  private final IAdapterFinder<MapPanel> m_adapterFinder = new EditorFirstAdapterFinder<MapPanel>();

  private final AdapterPartListener<MapPanel> m_partListener = new AdapterPartListener<MapPanel>( MapPanel.class, m_adapterEater, m_adapterFinder, m_adapterFinder )
  {
    /**
     * @see org.kalypso.contribs.eclipse.ui.partlistener.AdapterPartListener#partActivated(org.eclipse.ui.IWorkbenchPartReference)
     */
    @Override
    public void partActivated( final IWorkbenchPartReference partRef )
    {
      final IWorkbenchPart part = partRef.getPart( false );
      if( part == MapWidgetView.this )
        onThisActivated();

      super.partActivated( partRef );
    }
  };

  private final IWidgetChangeListener m_widgetListener = new IWidgetChangeListener()
  {
    public void widgetChanged( final IWidget newWidget )
    {
      onWidgetChanged( newWidget );
    }
  };

  private FormToolkit m_toolkit;

  /**
   * The top level control of this view.
   */
  private Group m_group;

  private StackLayout m_stackLayout;

  private Text m_noWidgetText;

  /*
   * TODO update view when model changes (on selected modellevents)
   */

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
   */
  @Override
  public void setFocus( )
  {
    if( m_group != null )
      m_group.setFocus();
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    m_toolkit = new FormToolkit( parent.getDisplay() );

    m_group = new Group( parent, SWT.NONE );
    m_toolkit.adapt( m_group );

    m_stackLayout = new StackLayout();
    m_group.setLayout( m_stackLayout );

    m_noWidgetText = m_toolkit.createText( m_group, "Kein Werkzeug ausgew‰hlt", SWT.READ_ONLY | SWT.WRAP | SWT.CENTER );

    m_stackLayout.topControl = m_noWidgetText;

    final IWorkbenchPage page = getSite().getPage();
    page.addPartListener( m_partListener );
    m_partListener.init( page );
  }

  /**
   * Sets to widget to show in this view for the given panel.
   * 
   * @param action
   *            If the given action is non-null, it will be activated if the widget is reactivated.
   */
  public void setWidgetForPanel( final MapPanel panel, final IWidgetWithOptions widget )
  {
    panel.getWidgetManager().addWidgetChangeListener( m_widgetListener );

    final WidgetInfo oldInfo = m_widgetInfos.get( panel );

    if( oldInfo != null )
    {
      final IWidgetWithOptions oldWidget = oldInfo.getWidget();
      final Composite composite = oldInfo.getComposite();

      disposeWidget( oldWidget );

      if( composite != null )
      {
        if( m_stackLayout.topControl == composite )
          m_stackLayout.topControl = m_noWidgetText;

        composite.dispose();
      }
    }

    final WidgetInfo newInfo = new WidgetInfo( null, widget );
    m_widgetInfos.put( panel, newInfo );

    // If it was the top control, this panel is active, so automatically activate this widget
    mapActivated( panel );
  }

  protected void mapActivated( final MapPanel panel )
  {
    if( m_group == null || m_group.isDisposed() )
      return;

    final WidgetInfo widgetInfo = m_widgetInfos.get( panel );

    Control controlToShow = widgetInfo == null ? null : widgetInfo.getComposite();
    if( controlToShow == null )
    {
      // was not yet created, create it now
      final IWidgetWithOptions widget = widgetInfo == null ? null : widgetInfo.getWidget();
      if( widget == null )
      {
        // no widget at all active, just show a message
        controlToShow = m_noWidgetText;
        m_group.setText( " - Kein Werkzeug ausgew‰lt - " );
        m_group.setToolTipText( null );
      }
      else
      {
        m_group.setText( widget.getName() );
        m_group.setToolTipText( widget.getToolTip() );

        final Composite widgetParent = m_toolkit.createComposite( m_group, SWT.NONE );
        widgetParent.setLayout( new FillLayout() );

        /* Also, of course, activate the widget */
        panel.getWidgetManager().setActualWidget( widget );

        widget.createControl( widgetParent, m_toolkit );

        widgetInfo.setComposite( widgetParent );

        controlToShow = widgetParent;
      }
    }

    m_stackLayout.topControl = controlToShow;
    controlToShow.setData( DATA_PANEL, panel );

    m_group.layout();
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#dispose()
   */
  @Override
  public void dispose( )
  {
    for( final Entry<MapPanel, WidgetInfo> entry : m_widgetInfos.entrySet() )
    {
      entry.getKey().getWidgetManager().removeWidgetChangeListener( m_widgetListener );
      final WidgetInfo info = entry.getValue();
      disposeWidget( info.getWidget() );
    }

    m_widgetInfos.clear();

    getSite().getPage().removePartListener( m_partListener );

    m_toolkit.dispose();

    super.dispose();
  }

  private void disposeWidget( final IWidgetWithOptions widget )
  {
    if( widget != null )
    {
      widget.finish();
      widget.disposeControl();
    }
  }

  protected void onWidgetChanged( final IWidget newWidget )
  {
    final MapPanel panel = (MapPanel) m_stackLayout.topControl.getData( DATA_PANEL );
    if( panel != null )
    {
      final WidgetInfo info = m_widgetInfos.get( panel );
      final IWidgetWithOptions widget = info == null ? null : info.getWidget();
      if( newWidget == null || !newWidget.equals( widget ) )
      {
        setContentDescription( "Deaktiviert, klicken Sie in diese Ansicht, um das Werkzeug zu reaktivieren." );
      }
    }
  }

  /**
   * Called if this view get activated. Make sure that the shown widget is reactivated in the panel.
   */
  protected void onThisActivated( )
  {
    final Control topControl = m_stackLayout.topControl;
    if( topControl.isDisposed() )
      return;

    final MapPanel panel = (MapPanel) topControl.getData( DATA_PANEL );
    if( panel != null )
    {
      final WidgetInfo info = m_widgetInfos.get( panel );
      if( info != null )
      {
        /* Activate my widget if not already done so */
        /* Check if already presetn, in order to suppress map repaint */
        final IWidgetWithOptions widget = info.getWidget();
        if( widget != null && widget != panel.getWidgetManager().getActualWidget() )
        {
          panel.getWidgetManager().setActualWidget( widget );
          setContentDescription( "" );
        }
      }
    }

  }
}
