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
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.ogc.sensor.tableview.ITableViewTheme;
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

  protected final ObservationTableModel m_model;

  protected final DateTableCellRenderer m_dateRenderer;

  private MaskedNumberTableCellRenderer m_nbRenderer;

  public ObservationTable( final ObservationTableModel model )
  {
    super( model );

    // for convenience
    m_model = model;

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

  /**
   * @see org.kalypso.ogc.sensor.template.ITemplateEventListener#onTemplateChanged(org.kalypso.ogc.sensor.template.TemplateEvent)
   */
  public void onTemplateChanged( final TemplateEvent evt )
  {
    final CatchRunnable runnable = new CatchRunnable()
    {
      protected void runIntern( ) throws Throwable
      {
        // ADD COLUMN
        if( evt.getType() == TemplateEvent.TYPE_ADD
            && evt.getObject() instanceof ITableViewColumn )
        {
          final ITableViewColumn col = (ITableViewColumn) evt.getObject();
          m_model.addTableViewColumn( col );

          checkForecast( col.getTheme().getObservation() );
        }

        // SHOW/HIDE A COLUMN
        if( evt.isType( TemplateEvent.TYPE_SHOW_STATE )
            && evt.getObject() instanceof ITableViewColumn )
        {
          final ITableViewColumn col = (ITableViewColumn) evt.getObject();
          
          if( col.isShown() )
            m_model.addTableViewColumn( col );
          else
            m_model.removeTableViewColumn( col );
        }
        
        // REMOVE THEME
        if( evt.getType() == TemplateEvent.TYPE_REMOVE
            && evt.getObject() instanceof ITableViewTheme )
        {
          final ITableViewTheme theme = (ITableViewTheme) evt.getObject();
          m_model.removeTableViewColumns( theme );

          // TODO clear the marker that may be associated with the obs
          // of the theme we are removing
        }

        // REMOVE ALL
        if( evt.getType() == TemplateEvent.TYPE_REMOVE_ALL )
        {
          m_model.clearColumns();
          m_dateRenderer.clearMarkers();
        }
      }
    };

    try
    {
      SwingUtilities.invokeAndWait( runnable );

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

  protected void checkForecast( final IObservation obs )
  {
    // check if observation is a vorhersage
    final DateRangeArgument dr = TimeserieUtils.isForecast( obs );
    if( dr != null )
      m_dateRenderer.addMarker( new ForecastLabelMarker( dr ) );
  }
}