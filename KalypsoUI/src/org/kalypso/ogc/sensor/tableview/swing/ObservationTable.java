package org.kalypso.ogc.sensor.tableview.swing;

import java.awt.Color;
import java.lang.reflect.InvocationTargetException;
import java.util.Date;

import javax.swing.JTable;
import javax.swing.SwingUtilities;

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
  protected final ObservationTableModel m_model;

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
  public void onTemplateChanged( final TemplateEvent evt )
  {
    final Runnable runnable = new Runnable()
    {
      public void run( )
      {

        try
        {
          if( evt.getType() == TemplateEvent.TYPE_ADD
              && evt.getObject() instanceof ITableViewColumn )
          {
            final ITableViewColumn col = (ITableViewColumn) evt.getObject();
            m_model.addTableViewColumn( col );
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
    };

    try
    {
      SwingUtilities.invokeAndWait( runnable );
    }
    catch( InterruptedException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( InvocationTargetException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }
}