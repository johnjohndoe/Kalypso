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

import java.awt.Color;
import java.text.NumberFormat;
import java.util.Date;

import javax.swing.JOptionPane;
import javax.swing.JTable;
import javax.swing.SwingUtilities;
import javax.swing.table.TableCellRenderer;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.internal.Workbench;
import org.kalypso.java.lang.CatchRunnable;
import org.kalypso.java.swing.table.SelectAllCellEditor;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.tableview.impl.TableViewColumn;
import org.kalypso.ogc.sensor.tableview.impl.TableViewTemplate;
import org.kalypso.ogc.sensor.tableview.impl.TableViewTheme;
import org.kalypso.ogc.sensor.tableview.swing.editor.DoubleCellEditor;
import org.kalypso.ogc.sensor.tableview.swing.marker.ForecastLabelMarker;
import org.kalypso.ogc.sensor.tableview.swing.renderer.DateTableCellRenderer;
import org.kalypso.ogc.sensor.tableview.swing.renderer.MaskedNumberTableCellRenderer;
import org.kalypso.ogc.sensor.template.ITemplateEventListener;
import org.kalypso.ogc.sensor.template.TemplateEvent;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.util.runtime.args.DateRangeArgument;

/**
 * A JTable that can display observations.
 * 
 * @author schlienger
 */
public class ObservationTable extends JTable implements ITemplateEventListener
{
  private final static NumberFormat NF = NumberFormat.getInstance();
  static
  {
    //NF.setMaximumFractionDigits( 3 );
    NF.setMinimumFractionDigits( 3 );
  }

  private final ObservationTableModel m_model;

  private final TableViewTemplate m_template;

  private final DateTableCellRenderer m_dateRenderer;

  private MaskedNumberTableCellRenderer m_nbRenderer;

  /**
   * Constructs a table based on the given template
   * 
   * @param template
   */
  public ObservationTable( final TableViewTemplate template )
  {
    super( new ObservationTableModel() );

    m_template = template;

    // for convenience
    m_model = (ObservationTableModel) getModel();
    m_model.setRules( template.getRules() );

    // removed in this.dispose()
    m_template.addTemplateEventListener( this );

    m_dateRenderer = new DateTableCellRenderer();

    m_nbRenderer = new MaskedNumberTableCellRenderer( NF );

    setDefaultRenderer( Date.class, m_dateRenderer );
    setDefaultRenderer( Number.class, m_nbRenderer );
    setDefaultRenderer( Double.class, m_nbRenderer );
    setDefaultRenderer( Float.class, m_nbRenderer );

    final NumberFormat nf = NumberFormat.getNumberInstance();
    nf.setGroupingUsed( false );
    setDefaultEditor( Double.class, new SelectAllCellEditor(
        new DoubleCellEditor( nf, true ) ) );

    setSelectionForeground( Color.BLACK );
    setSelectionBackground( Color.YELLOW );

    getTableHeader().setReorderingAllowed( false );
  }

  public void dispose( )
  {
    m_template.removeTemplateEventListener( this );
    m_model.clearColumns();
    m_dateRenderer.clearMarkers();
  }

  /**
   * @see org.kalypso.ogc.sensor.template.ITemplateEventListener#onTemplateChanged(org.kalypso.ogc.sensor.template.TemplateEvent)
   */
  public void onTemplateChanged( final TemplateEvent evt )
  {
    // for runnable
    final ObservationTableModel model = m_model;
    final DateTableCellRenderer dateRenderer = m_dateRenderer;

    final CatchRunnable runnable = new CatchRunnable()
    {
      protected void runIntern( ) throws Throwable
      {
        // REFRESH ONE THEME
        if( evt.getType() == TemplateEvent.TYPE_REFRESH
            && evt.getObject() instanceof TableViewTheme )
        {
          final TableViewTheme theme = (TableViewTheme) evt.getObject();

          model.refreshColumns( theme );

          checkForecast( theme, true );
        }

        // ADD THEME
        if( evt.getType() == TemplateEvent.TYPE_ADD
            && evt.getObject() instanceof TableViewTheme )
        {
          final TableViewTheme theme = (TableViewTheme) evt.getObject();
          model.addColumnsFor( theme );

          checkForecast( theme, true );
        }

        // SHOW/HIDE A COLUMN
        if( evt.isType( TemplateEvent.TYPE_SHOW_STATE )
            && evt.getObject() instanceof TableViewColumn )
        {
          final TableViewColumn col = (TableViewColumn) evt.getObject();

          if( col.isShown() )
            model.addColumn( col );
          else
            model.removeColumn( col );
        }

        // REMOVE THEME
        if( evt.getType() == TemplateEvent.TYPE_REMOVE
            && evt.getObject() instanceof TableViewTheme )
        {
          final TableViewTheme theme = (TableViewTheme) evt.getObject();
          model.removeColumnsFor( theme );

          checkForecast( theme, false );
        }

        // REMOVE ALL
        if( evt.getType() == TemplateEvent.TYPE_REMOVE_ALL )
        {
          model.clearColumns();
          dateRenderer.clearMarkers();
        }
      }
    };

    try
    {
      if( !SwingUtilities.isEventDispatchThread() )
        SwingUtilities.invokeAndWait( runnable );
      else
        runnable.run();

      if( runnable.getThrown() != null )
        throw runnable.getThrown();
    }
    catch( Throwable e )
    {
      final IWorkbenchWindow activeWorkbenchWindow = Workbench.getInstance()
          .getActiveWorkbenchWindow();
      final Shell shell = activeWorkbenchWindow == null ? null
          : activeWorkbenchWindow.getShell();
      if( shell != null )
        MessageDialog.openError( shell, "Aktualisierungsfehler", e.toString() );
      else
        JOptionPane.showMessageDialog( null, e.toString(),
            "Aktualisierungsfehler", JOptionPane.ERROR_MESSAGE );
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
   * Helper method that adds a marker to the date renderer for observations that
   * are forecasts
   * 
   * @param theme
   * @param adding
   */
  protected void checkForecast( final TableViewTheme theme, final boolean adding )
  {
    // check if observation is a vorhersage
    final IObservation obs = theme.getObservation();
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
}