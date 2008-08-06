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
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.MouseEvent;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.NumberFormat;
import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JViewport;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;

import org.kalypso.auth.KalypsoAuthPlugin;
import org.kalypso.auth.scenario.IScenario;
import org.kalypso.auth.scenario.ScenarioUtilities;
import org.kalypso.commons.java.swing.jtable.PopupMenu;
import org.kalypso.commons.java.util.StringUtilities;
import org.kalypso.contribs.java.lang.CatchRunnable;
import org.kalypso.contribs.java.swing.table.ExcelClipboardAdapter;
import org.kalypso.contribs.java.swing.table.SelectAllCellEditor;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationConstants;
import org.kalypso.ogc.sensor.tableview.TableView;
import org.kalypso.ogc.sensor.tableview.TableViewColumn;
import org.kalypso.ogc.sensor.tableview.swing.editor.DoubleCellEditor;
import org.kalypso.ogc.sensor.tableview.swing.marker.ForecastLabelMarker;
import org.kalypso.ogc.sensor.tableview.swing.renderer.ColumnHeaderListener;
import org.kalypso.ogc.sensor.tableview.swing.renderer.ColumnHeaderRenderer;
import org.kalypso.ogc.sensor.tableview.swing.renderer.DateTableCellRenderer;
import org.kalypso.ogc.sensor.tableview.swing.renderer.MaskedNumberTableCellRenderer;
import org.kalypso.ogc.sensor.tableview.swing.tablemodel.ObservationTableModel;
import org.kalypso.ogc.sensor.template.IObsViewEventListener;
import org.kalypso.ogc.sensor.template.ObsViewEvent;
import org.kalypso.ogc.sensor.template.SwingEclipseUtilities;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;

/**
 * A JTable that can display observations.
 * 
 * @author schlienger
 */
public class ObservationTable extends JPanel implements IObsViewEventListener
{
  private final ObservationTableModel m_model;
  private final MainTable m_table;

  private final TableView m_view;

  protected final DateTableCellRenderer m_dateRenderer;

  private final boolean m_waitForSwing;

  protected String m_currentScenarioName = ""; //$NON-NLS-1$ 

  /** is created if an observation has a scenario property */
  private final JLabel m_label = new JLabel( "", SwingConstants.CENTER ); //$NON-NLS-1$

  /** default background color for the date renderer when not displaying forecast */
  private static final Color BG_COLOR = new Color( 222, 222, 222 );

  public ObservationTable( final TableView template )
  {
    this( template, false, true );
  }

  /**
   * @param waitForSwing
   *          when true, the events are handled synchronously in onObsviewChanged(), this is useful when you are
   *          creating the table for non-gui purposes such as in the export-document-wizard: there you need to wait for
   *          swing to be finished with updating/painting the table before doing the export, else you get unexpected
   *          results
   */
  public ObservationTable( final TableView template, final boolean waitForSwing, final boolean useContextMenu )
  {
    m_view = template;
    m_waitForSwing = waitForSwing;

    m_model = new ObservationTableModel();
    m_model.setRules( template.getRules() );

    // date renderer with timezone
    m_dateRenderer = new DateTableCellRenderer();
    m_dateRenderer.setTimeZone( template.getTimezone() );    
    
    final NumberFormat nf = NumberFormat.getNumberInstance();
    nf.setGroupingUsed( false );

    final TableColumnModel cm = new MainColumnModel( m_model );

    m_table = new MainTable( useContextMenu, nf, m_model, cm );

    final JTableHeader header = m_table.getTableHeader();
    final ColumnHeaderListener columnHeaderListener = new ColumnHeaderListener();
    header.addMouseListener( columnHeaderListener );
    header.setReorderingAllowed( false );
    header.setEnabled( true );

    final TableCellRenderer nbRenderer = new MaskedNumberTableCellRenderer( m_model );
    m_table.setDefaultRenderer( Date.class, m_dateRenderer );
    m_table.setDefaultRenderer( Number.class, nbRenderer );
    m_table.setDefaultRenderer( Double.class, nbRenderer );
    m_table.setDefaultRenderer( Float.class, nbRenderer );
    m_table.setAutoCreateColumnsFromModel( true );
    m_table
        .setDefaultEditor( Double.class, new SelectAllCellEditor( new DoubleCellEditor( nf, true, new Double( 0 ) ) ) );
    m_table.setCellSelectionEnabled( true );
    m_table.setAutoResizeMode( JTable.AUTO_RESIZE_OFF );

    final JTable rowHeader = new JTable( m_model, new RowHeaderColumnModel() );
    rowHeader.setDefaultRenderer( Date.class, m_dateRenderer );
    rowHeader.setDefaultRenderer( Number.class, nbRenderer );
    rowHeader.setDefaultRenderer( Double.class, nbRenderer );
    rowHeader.setDefaultRenderer( Float.class, nbRenderer );
    rowHeader.setColumnSelectionAllowed( false );
    rowHeader.setCellSelectionEnabled( false );
    rowHeader.getTableHeader().setReorderingAllowed( false );
    rowHeader.getTableHeader().setDefaultRenderer( new ColumnHeaderRenderer( ) );
    rowHeader.setAutoCreateColumnsFromModel( true );

    // make sure that selections between the main table and the header stay in sync
    // by sharing the same model
    m_table.setSelectionModel( rowHeader.getSelectionModel() );

    final JViewport vp = new JViewport();
    vp.setView( rowHeader );
    vp.setPreferredSize( rowHeader.getPreferredSize() );

    final JScrollPane scrollPane = new JScrollPane( m_table );
    scrollPane.setRowHeader( vp );
    scrollPane.setCorner( ScrollPaneConstants.UPPER_LEFT_CORNER, rowHeader.getTableHeader() );

    setLayout( new BoxLayout( this, BoxLayout.Y_AXIS ) );

    m_label.setVisible( false );
    add( m_label );

    add( scrollPane );

    // removed in this.dispose()
    m_view.addObsViewEventListener( this );
  }

  public void dispose()
  {
    m_table.dispose();

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
    final MainTable table = m_table;

    final CatchRunnable runnable = new CatchRunnable()
    {
      @Override
      protected void runIntern() throws Throwable
      {
        final int evenType = evt.getType();

        // REFRESH ONE COLUMN
        if( evenType == ObsViewEvent.TYPE_ITEM_DATA_CHANGED && evt.getObject() instanceof TableViewColumn )
        {
          final TableViewColumn column = (TableViewColumn)evt.getObject();
          model.refreshColumn( column, evt.getSource() );

          analyseObservation( column.getObservation(), true );

          // also repaint header, status may have changed
          table.getTableHeader().repaint();
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
            clearLabel();
            m_currentScenarioName = ""; //$NON-NLS-1$
          }
        }

        // REMOVE ALL
        if( evenType == ObsViewEvent.TYPE_ITEM_REMOVE_ALL )
        {
          model.clearColumns();
          dateRenderer.clearMarkers();

          clearLabel();
          m_currentScenarioName = ""; //$NON-NLS-1$
        }

        // VIEW CHANGED
        if( evenType == ObsViewEvent.TYPE_VIEW_CHANGED )
        {
          final TableView view = (TableView)evt.getObject();
          model.setAlphaSort( view.isAlphaSort() );
          
          m_dateRenderer.setTimeZone( view.getTimezone() );

          repaint();
        }
      }
    };

    SwingEclipseUtilities.invokeAndHandleError( runnable, m_waitForSwing );
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
          m_dateRenderer.addMarker( new ForecastLabelMarker( dr, BG_COLOR ) );
        else
          m_dateRenderer.removeMarker( new ForecastLabelMarker( dr, BG_COLOR ) );
      }

      final MetadataList mdl = obs.getMetadataList();

      // add a scenario-label if obs has scenario specific metadata property
      if( mdl.getProperty( ObservationConstants.MD_SCENARIO ) != null )
      {
        final IScenario scenario = KalypsoAuthPlugin.getDefault().getScenario(
            mdl.getProperty( ObservationConstants.MD_SCENARIO ) );

        if( scenario != null && !ScenarioUtilities.isDefaultScenario( scenario ) && isLabelSet() )
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
              Logger.getLogger( getClass().getName() ).log( Level.WARNING, Messages.getString("org.kalypso.ogc.sensor.tableview.swing.ObservationTable.3"), e ); //$NON-NLS-1$
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
          setLabel( m_currentScenarioName, icon, color, height, showTxt );
        }
      }
    }
  }

  public ObservationTableModel getObservationTableModel()
  {
    return m_model;
  }

  public TableView getTemplate()
  {
    return m_view;
  }

  public String getCurrentScenarioName()
  {
    return m_currentScenarioName;
  }

  public void setAlphaSortActivated( final boolean bAlphaSort )
  {
    m_model.setAlphaSort( bAlphaSort );
  }

  /**
   * @see org.kalypso.ogc.sensor.template.IObsViewEventListener#onPrintObsView(org.kalypso.ogc.sensor.template.ObsViewEvent)
   */
  public void onPrintObsView( final ObsViewEvent evt )
  {}

  protected void clearLabel()
  {
    m_label.setText( "" ); //$NON-NLS-1$
    m_label.setVisible( false );
  }

  protected boolean isLabelSet()
  {
    return m_label.isVisible();
  }

  protected void setLabel( final String txt, final Icon icon, final Color color, final Integer height,
      final Boolean showTxt )
  {
    final boolean bShowText;
    if( showTxt != null )
      bShowText = showTxt.booleanValue();
    else
      bShowText = true;

    if( bShowText )
      m_label.setText( txt );
    else
      m_label.setText( "" ); //$NON-NLS-1$
    m_label.setIcon( icon );
    setBackground( color );

    if( height != null )
      m_label.setPreferredSize( new Dimension( m_label.getWidth(), height.intValue() ) );

    m_label.setVisible( true );
    doLayout();
  }

  private static class MainColumnModel extends DefaultTableColumnModel
  {
    private final ObservationTableModel m_obsModel;

    public MainColumnModel( final ObservationTableModel model )
    {
      m_obsModel = model;
    }

    // ignore key column
    @Override
    public void addColumn( final TableColumn aColumn )
    {
      final IAxis sharedAxis = m_obsModel.getSharedAxis();
      if( sharedAxis != null && sharedAxis.getName().equals( aColumn.getHeaderValue() ) )
        return;

      final TableCellRenderer headerRenderer = new ColumnHeaderRenderer();
      aColumn.setHeaderRenderer( headerRenderer );
      // Overwrite header value: normally its just 'getColumnName'; but we need more information
      final Object headerValue = m_obsModel.getColumnValue( aColumn.getModelIndex() );
      aColumn.setHeaderValue( headerValue );

      // Auto-resize column
      final Component c = headerRenderer.getTableCellRendererComponent( null, aColumn.getHeaderValue(), false, false,
          0, 0 );
      final int colWidth = c.getPreferredSize().width + 5;
      aColumn.setPreferredWidth( colWidth );
      aColumn.setWidth( colWidth );
      aColumn.setMinWidth( 50 );

      super.addColumn( aColumn );
    }
  }

  private static class RowHeaderColumnModel extends DefaultTableColumnModel
  {
    // just fist column, other are ignored
    @Override
    public void addColumn( final TableColumn aColumn )
    {
      if( getColumnCount() >= 1 )
        return;

      aColumn.setMaxWidth( 100 );
      aColumn.setResizable( false );
      aColumn.setMinWidth( 100 );
      aColumn.setWidth( 100 );

      super.addColumn( aColumn );
    }
  }

  private static class MainTable extends JTable
  {
    private PopupMenu m_popup = null;

    private ExcelClipboardAdapter m_excelCp = null;

    public MainTable( final boolean useContextMenu, final NumberFormat nf, final TableModel dm,
        final TableColumnModel cm )
    {
      super( dm, cm );

      if( useContextMenu )
      {
        m_popup = new PopupMenu( this );
        m_excelCp = new ExcelClipboardAdapter( this, nf );

        m_popup.add( new JPopupMenu.Separator() );
        m_popup.add( m_excelCp.getCopyAction() );
        m_popup.add( m_excelCp.getPasteAction() );
      }
    }

    public void dispose()
    {
      if( m_excelCp != null )
        m_excelCp.dispose();
    }

    @Override
    protected void processMouseEvent( MouseEvent e )
    {
      if( e.isPopupTrigger() && m_popup != null )
        m_popup.show( this, e.getX(), e.getY() );
      else
        super.processMouseEvent( e );
    }
  }
}
