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
import java.awt.event.MouseEvent;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.NumberFormat;
import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.JTable;
import javax.swing.event.TableColumnModelEvent;
import javax.swing.table.TableCellRenderer;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.internal.Workbench;
import org.kalypso.auth.KalypsoAuthPlugin;
import org.kalypso.auth.scenario.IScenario;
import org.kalypso.auth.scenario.ScenarioUtilities;
import org.kalypso.commons.java.util.StringUtilities;
import org.kalypso.contribs.java.lang.CatchRunnable;
import org.kalypso.contribs.java.swing.SwingInvokeHelper;
import org.kalypso.contribs.java.swing.table.ExcelClipboardAdapter;
import org.kalypso.contribs.java.swing.table.SelectAllCellEditor;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationConstants;
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

  private final boolean m_waitForSwing;

  private final PopupMenu m_popup;

  private ExcelClipboardAdapter m_excelCp;

  protected ObservationTablePanel m_panel = null;

  protected String m_currentScenarioName = "";

  /**
   * if the amount of columns is equal or bigger than this threshold, then the autoResizeMode property is set to
   * AUTO_RESIZE_OFF
   * <p>
   * if value is < 0 then function is not used
   */
  private int m_thresholdColumnsAutoResizeModeOff = -1;

  public ObservationTable( final TableView template )
  {
    this( template, false, true );
  }

  /**
   * @param waitForSwing
   *          when true, the events are handled synchonuously in onObsviewChanged(), this is usefull when you are
   *          creating the table for non-gui purposes such as in the export-document-wizard: there you need to wait for
   *          swing to be finished with updating/painting the table before doing the export, else you get unexpected
   *          results
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
  }

  /**
   * @see javax.swing.JTable#columnAdded(javax.swing.event.TableColumnModelEvent)
   */
  public void columnAdded( TableColumnModelEvent e )
  {
    super.columnAdded( e );

    if( m_thresholdColumnsAutoResizeModeOff >= 0 && getColumnCount() >= m_thresholdColumnsAutoResizeModeOff )
      setAutoResizeMode( JTable.AUTO_RESIZE_OFF );
  }

  /**
   * @see javax.swing.JTable#columnRemoved(javax.swing.event.TableColumnModelEvent)
   */
  public void columnRemoved( TableColumnModelEvent e )
  {
    super.columnRemoved( e );

    if( m_thresholdColumnsAutoResizeModeOff >= 0 && getColumnCount() < m_thresholdColumnsAutoResizeModeOff )
      setAutoResizeMode( JTable.AUTO_RESIZE_ALL_COLUMNS );
  }

  public void dispose()
  {
    if( m_excelCp != null )
      m_excelCp.dispose();

    m_dateRenderer.clearMarkers();
    m_view.removeObsViewListener( this );

    m_panel = null;

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
        final int evenType = evt.getType();

        // REFRESH ONE COLUMN
        if( evenType == ObsViewEvent.TYPE_ITEM_DATA_CHANGED && evt.getObject() instanceof TableViewColumn )
        {
          final TableViewColumn column = (TableViewColumn)evt.getObject();
          model.refreshColumn( column, evt.getSource() );

          analyseObservation( column.getObservation(), true );
        }

        // REFRESH COLUMN ACCORDING TO ITS STATE
        if( evenType == ObsViewEvent.TYPE_ITEM_STATE_CHANGED && evt.getObject() instanceof TableViewColumn )
        {
          final TableViewColumn column = (TableViewColumn)evt.getObject();

          if( column.isShown() )
            model.addColumn( column );
          else
            model.removeColumn( column );

          analyseObservation( column.getObservation(), column.isShown() );
        }

        // ADD COLUMN
        if( evenType == ObsViewEvent.TYPE_ITEM_ADD && evt.getObject() instanceof TableViewColumn )
        {
          final TableViewColumn column = (TableViewColumn)evt.getObject();
          if( column.isShown() )
            model.addColumn( column );

          analyseObservation( column.getObservation(), true );
        }

        // REMOVE COLUMN
        if( evenType == ObsViewEvent.TYPE_ITEM_REMOVE && evt.getObject() instanceof TableViewColumn )
        {
          final TableViewColumn column = (TableViewColumn)evt.getObject();
          model.removeColumn( column );

          analyseObservation( column.getObservation(), false );

          if( model.getColumnCount() == 0 )
          {
            m_panel.clearLabel();
            m_currentScenarioName = "";
          }
        }

        // REMOVE ALL
        if( evenType == ObsViewEvent.TYPE_ITEM_REMOVE_ALL )
        {
          model.clearColumns();
          dateRenderer.clearMarkers();

          if( m_panel != null )
          {
            m_panel.clearLabel();
            m_currentScenarioName = "";
          }
        }
      }
    };

    try
    {
      SwingInvokeHelper.invoke( runnable, m_waitForSwing );
    }
    catch( final Throwable e )
    {
      e.printStackTrace();

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
   * Helper method that analyses the observation.
   * <ul>
   * <li>adds a marker to the date renderer for observations that are forecasts or remove the corresponding marker when
   * the associated column is removed from the model
   * <li>adds a label if the observation has a scenario property
   * </ul>
   * 
   * @param adding
   *          is true when the observation (actually its associated table-view-column) is added to the model
   */
  protected void analyseObservation( final IObservation obs, final boolean adding )
  {
    if( obs != null )
    {
      // check if observation is a vorhersage
      final DateRange dr = TimeserieUtils.isForecast( obs );
      if( dr != null )
      {
        if( adding )
          m_dateRenderer.addMarker( new ForecastLabelMarker( dr ) );
        else
          m_dateRenderer.removeMarker( new ForecastLabelMarker( dr ) );
      }

      final MetadataList mdl = obs.getMetadataList();

      // add a scenario-label if obs has scenario specific metadata property
      if( mdl.getProperty( ObservationConstants.MD_SCENARIO ) != null )
      {
        final IScenario scenario = KalypsoAuthPlugin.getDefault().getScenario(
            mdl.getProperty( ObservationConstants.MD_SCENARIO ) );

        if( scenario != null && !ScenarioUtilities.isDefaultScenario( scenario ) && m_panel != null
            && !m_panel.isLabelSet() )
        {
          Icon icon = null;
          final String imageURL = scenario.getProperty( IScenario.PROP_TABLE_HEADER_IMAGE_URL, null );
          if( imageURL != null )
          {
            try
            {
              icon = new ImageIcon( new URL( imageURL ) );
            }
            catch( final MalformedURLException e )
            {
              Logger.getLogger( getClass().getName() ).log( Level.WARNING, "Bild konnte nicht geladen werden", e );
            }
          }

          Color color = null;
          final String strc = scenario.getProperty( IScenario.PROP_TABLE_HEADER_RGB, null );
          if( strc != null )
            color = StringUtilities.stringToColor( strc );

          Integer height = null;
          final String strHeight = scenario.getProperty( IScenario.PROP_TABLE_HEADER_HEIGHT, null );
          if( strHeight != null )
            height = Integer.valueOf( strHeight );

          Boolean showTxt = null;
          final String strShow = scenario.getProperty( IScenario.PROP_TABLE_HEADER_SHOWTEXT, null );
          if( strShow != null )
            showTxt = Boolean.valueOf( strShow );

          m_currentScenarioName = scenario.getName();
          m_panel.setLabel( m_currentScenarioName, icon, color, height, showTxt );
        }
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

  public ObservationTableModel getObservationTableModel()
  {
    return m_model;
  }

  public void setPanel( final ObservationTablePanel panel )
  {
    m_panel = panel;
  }

  public String getCurrentScenarioName()
  {
    return m_currentScenarioName;
  }

  public void setThresholdColumnsAutoResizeModeOff( final int thresholdColumnsAutoResizeModeOff )
  {
    m_thresholdColumnsAutoResizeModeOff = thresholdColumnsAutoResizeModeOff;
  }
}
