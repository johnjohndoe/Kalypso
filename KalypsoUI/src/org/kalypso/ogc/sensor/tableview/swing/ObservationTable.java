package org.kalypso.ogc.sensor.tableview.swing;

import java.awt.Color;
import java.text.NumberFormat;
import java.util.Date;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JTable;
import javax.swing.SwingUtilities;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.internal.Workbench;
import org.kalypso.java.lang.CatchRunnable;
import org.kalypso.java.swing.table.SelectAllCellEditor;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.ogc.sensor.tableview.swing.editor.DoubleCellEditor;
import org.kalypso.ogc.sensor.tableview.swing.renderer.DateTableCellRenderer;
import org.kalypso.ogc.sensor.tableview.swing.renderer.MaskedNumberTableCellRenderer;
import org.kalypso.ogc.sensor.template.ITemplateEventListener;
import org.kalypso.ogc.sensor.template.TemplateEvent;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.util.runtime.args.DateRangeArgument;

/**
 * @author schlienger
 */
public class ObservationTable extends JTable implements ITemplateEventListener
{
  private final static Color FORECAST_BG = new Color( 0, 0, 255, 50);
  private final static String FORECAST_TT = "Begin der Vorhersage";
  private final static Icon FORECAST_ICON = new ImageIcon( ObservationTable.class.getResource("resource/warning_small.gif") );
  
  protected final ObservationTableModel m_model;
  protected final DateTableCellRenderer m_dateRenderer;

  public ObservationTable( final ObservationTableModel model )
  {
    super( model );

    // for convenience
    m_model = model;

    m_dateRenderer = new DateTableCellRenderer();
    setDefaultRenderer( Date.class, m_dateRenderer );
    setDefaultRenderer( Number.class, new MaskedNumberTableCellRenderer() );

    final NumberFormat nf = NumberFormat.getNumberInstance();
    nf.setGroupingUsed( false );
    setDefaultEditor( Double.class, new SelectAllCellEditor(new DoubleCellEditor( nf, true ) ) );
    
    setSelectionForeground( Color.BLACK );
    setSelectionBackground( Color.YELLOW );
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
        if( evt.getType() == TemplateEvent.TYPE_ADD
            && evt.getObject() instanceof ITableViewColumn )
        {
          final ITableViewColumn col = (ITableViewColumn) evt.getObject();
          m_model.addTableViewColumn( col );
          
          checkForecast( col.getTheme().getObservation() );
        }

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
      MessageDialog.openError( Workbench.getInstance().getActiveWorkbenchWindow().getShell(), "Aktualisierungsfehler", e.toString() );
    }
  }
  
  protected void checkForecast( final IObservation obs )
  {
    // check if observation is a vorhersage
    final DateRangeArgument dr = TimeserieUtils.isForecast( obs );
    if( dr != null )
    {
      m_dateRenderer.addMarker( dr.getFrom(), FORECAST_BG, null, FORECAST_ICON, FORECAST_TT );
    }
  }
}