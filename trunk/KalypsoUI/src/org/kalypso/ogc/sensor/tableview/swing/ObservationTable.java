package org.kalypso.ogc.sensor.tableview.swing;

import java.awt.Color;
import java.util.Date;

import javax.swing.JTable;

import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.ogc.sensor.tableview.swing.renderer.DateTableCellRenderer;
import org.kalypso.ogc.sensor.tableview.swing.renderer.MaskedNumberTableCellRenderer;
import org.kalypso.ogc.sensor.template.ITemplateEventListener;
import org.kalypso.ogc.sensor.template.TemplateEvent;

/**
 * @author schlienger
 */
public class ObservationTable extends JTable implements ITemplateEventListener
{
  private final ObservationTableModel m_model;
  
  public ObservationTable( final ObservationTableModel model )
  {
    super( model );
    
    // for convenience
    m_model = model;

    setDefaultRenderer( Date.class, new DateTableCellRenderer() );
    setDefaultRenderer( Number.class, new MaskedNumberTableCellRenderer() );

    setSelectionForeground( getSelectionBackground() );
    setSelectionBackground( Color.YELLOW );
  }

  /**
   * @see org.kalypso.ogc.sensor.template.ITemplateEventListener#onTemplateChanged(org.kalypso.ogc.sensor.template.TemplateEvent)
   */
  public void onTemplateChanged( TemplateEvent evt )
  {
    try
    {
      if( evt.getType() == TemplateEvent.TYPE_ADD && evt.getObject() instanceof ITableViewColumn )
      {
        ITableViewColumn col = (ITableViewColumn)evt.getObject();
        m_model.addColumn( col );
      }

      if( evt.getType() == TemplateEvent.TYPE_REMOVE_ALL )
      {
        m_model.clearColumns();
      }
    }
    catch( SensorException e )
    {
      throw new RuntimeException( e );
    }
  }
}