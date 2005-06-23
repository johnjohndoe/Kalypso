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
package org.kalypso.ogc.sensor.tableview.swing;

import java.awt.event.MouseEvent;
import java.text.NumberFormat;
import java.util.Date;

import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.JTable;
import javax.swing.SwingUtilities;
import javax.swing.table.TableCellRenderer;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.internal.Workbench;
import org.kalypso.commons.runtime.args.DateRangeArgument;
import org.kalypso.contribs.java.lang.CatchRunnable;
import org.kalypso.contribs.java.swing.table.ExcelClipboardAdapter;
import org.kalypso.contribs.java.swing.table.SelectAllCellEditor;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.tableview.TableView;
import org.kalypso.ogc.sensor.tableview.TableViewColumn;
import org.kalypso.ogc.sensor.tableview.swing.editor.DoubleCellEditor;
import org.kalypso.ogc.sensor.tableview.swing.marker.ForecastLabelMarker;
import org.kalypso.ogc.sensor.tableview.swing.renderer.DateTableCellRenderer;
import org.kalypso.ogc.sensor.tableview.swing.renderer.MaskedNumberTableCellRenderer;
import org.kalypso.ogc.sensor.template.IObsViewEventListener;
import org.kalypso.ogc.sensor.template.ObsViewEvent;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;

/**
 * A JTable that can display observations.
 * 
 * @author schlienger
 */
public class ObservationTable extends JTable implements IObsViewEventListener
{
  private final ObservationTableModel m_model;

  private final TableView m_view;

  private final DateTableCellRenderer m_dateRenderer;

  private MaskedNumberTableCellRenderer m_nbRenderer;

  /**
   * when true, swing waits until model is updated, else swing continues normal processing
   * 
   * TODO: find out why this is usefull, I don't remember exactly, I believe it was due to an thread/update problem
   */
  private final boolean m_waitForSwing;

  private final PopupMenu m_popup;

  private ExcelClipboardAdapter m_excelCp;

  /**
   * Constructs a table based on the given template
   */
  public ObservationTable( final TableView template )
  {
    this( template, false, true );
  }

  /**
   * Constructs a table based on the given template
   */
  public ObservationTable( final TableView template, final boolean waitForSwing, final boolean useContextMenu )
  {
    super( new ObservationTableModel() );

    m_view = template;
    m_waitForSwing = waitForSwing;

    // for convenience
    m_model = (ObservationTableModel)getModel();
    m_model.setRules( template.getRules() );

    m_dateRenderer = new DateTableCellRenderer();

    m_nbRenderer = new MaskedNumberTableCellRenderer( m_model );

    setDefaultRenderer( Date.class, m_dateRenderer );
    setDefaultRenderer( Number.class, m_nbRenderer );
    setDefaultRenderer( Double.class, m_nbRenderer );
    setDefaultRenderer( Float.class, m_nbRenderer );

    final NumberFormat nf = NumberFormat.getNumberInstance();
    nf.setGroupingUsed( false );
    setDefaultEditor( Double.class, new SelectAllCellEditor( new DoubleCellEditor( nf, true, new Double( 0 ) ) ) );

    setCellSelectionEnabled( true );

    getTableHeader().setReorderingAllowed( false );

    if( useContextMenu )
    {
      m_popup = new PopupMenu( this );
      m_excelCp = new ExcelClipboardAdapter( this, nf );

      m_popup.add( new JPopupMenu.Separator() );
      m_popup.add( m_excelCp.getCopyAction() );
      m_popup.add( m_excelCp.getPasteAction() );
    }
    else
    {
      m_popup = null;
      m_excelCp = null;
    }

    // removed in this.dispose()
    m_view.addObsViewEventListener( this );
    //    for( final Iterator tIt = m_template.getThemes().iterator();
    // tIt.hasNext(); )
    //      m_template.fireTemplateChanged( new ObsViewEvent( m_template, tIt.next(),
    // ObsViewEvent.TYPE_ADD ) );
    //    for( final Iterator tIt = m_template.getThemes().iterator();
    // tIt.hasNext(); )
    //    {
    //      TableViewColumnXMLLoader theme = (TableViewColumnXMLLoader)tIt.next();
    //      
    //    }
  }

  public void dispose()
  {
    if( m_excelCp != null )
      m_excelCp.dispose();

    m_dateRenderer.clearMarkers();
    m_view.removeObsViewListener( this );

    m_model.clearColumns();
  }

  /**
   * @see org.kalypso.ogc.sensor.template.IObsViewEventListener#onObsViewChanged(org.kalypso.ogc.sensor.template.ObsViewEvent)
   */
  public final void onObsViewChanged( final ObsViewEvent evt )
  {
    // for runnable
    final ObservationTableModel model = m_model;
    final DateTableCellRenderer dateRenderer = m_dateRenderer;

    final CatchRunnable runnable = new CatchRunnable()
    {
      protected void runIntern() throws Throwable
      {
        // REFRESH ONE COLUMN
        if( evt.getType() == ObsViewEvent.TYPE_REFRESH && evt.getObject() instanceof TableViewColumn )
        {
          final TableViewColumn column = (TableViewColumn)evt.getObject();
          model.refreshColumn( column );

          checkForecast( column.getObservation(), true );
        }

        // ADD COLUMN
        if( evt.getType() == ObsViewEvent.TYPE_ADD && evt.getObject() instanceof TableViewColumn )
        {
          final TableViewColumn column = (TableViewColumn)evt.getObject();
          if( column.isShown() )
            model.addColumn( column );

          checkForecast( column.getObservation(), true );
        }

        // REMOVE COLUMN
        if( evt.getType() == ObsViewEvent.TYPE_REMOVE && evt.getObject() instanceof TableViewColumn )
        {
          final TableViewColumn column = (TableViewColumn)evt.getObject();
          model.removeColumn( column );

          checkForecast( column.getObservation(), false );
        }

        // REMOVE ALL
        if( evt.getType() == ObsViewEvent.TYPE_REMOVE_ALL )
        {
          model.clearColumns();
          dateRenderer.clearMarkers();
        }
      }
    };

    try
    {
      if( !SwingUtilities.isEventDispatchThread() )
      {
        if( m_waitForSwing )
          SwingUtilities.invokeAndWait( runnable );
        else
          SwingUtilities.invokeLater( runnable );
      }
      else
        runnable.run();

      if( runnable.getThrown() != null )
        throw runnable.getThrown();
    }
    catch( Throwable e )
    {
      final IWorkbenchWindow activeWorkbenchWindow = Workbench.getInstance().getActiveWorkbenchWindow();
      final Shell shell = activeWorkbenchWindow == null ? null : activeWorkbenchWindow.getShell();
      if( shell != null )
        MessageDialog.openError( shell, "Aktualisierungsfehler", e.toString() );
      else
        JOptionPane.showMessageDialog( null, e.toString(), "Aktualisierungsfehler", JOptionPane.ERROR_MESSAGE );
    }
  }

  /**
   * @see javax.swing.JTable#getCellRenderer(int, int)
   */
  public TableCellRenderer getCellRenderer( int row, int column )
  {
    final TableCellRenderer renderer = super.getCellRenderer( row, column );
    return renderer;
  }

  /**
   * Helper method that adds a marker to the date renderer for observations that are forecasts
   * 
   * @param adding
   */
  protected void checkForecast( final IObservation obs, final boolean adding )
  {
    // check if observation is a vorhersage
    if( obs != null )
    {
      final DateRangeArgument dr = TimeserieUtils.isForecast( obs );
      if( dr != null )
      {
        if( adding )
          m_dateRenderer.addMarker( new ForecastLabelMarker( dr ) );
        else
          m_dateRenderer.removeMarker( new ForecastLabelMarker( dr ) );
      }
    }
  }

  /**
   * @see java.awt.Component#processMouseEvent(java.awt.event.MouseEvent)
   */
  protected void processMouseEvent( MouseEvent e )
  {
    if( e.isPopupTrigger() && m_popup != null )
      m_popup.show( this, e.getX(), e.getY() );
    else
      super.processMouseEvent( e );
  }
}