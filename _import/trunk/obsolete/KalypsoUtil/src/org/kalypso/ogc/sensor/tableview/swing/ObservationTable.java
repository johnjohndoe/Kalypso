package org.kalypso.ogc.sensor.tableview.swing;

import java.awt.Color;
import java.util.Date;

import javax.swing.JTable;

import org.kalypso.ogc.sensor.tableview.swing.renderer.DateTableCellRenderer;
import org.kalypso.ogc.sensor.tableview.swing.renderer.MaskedNumberTableCellRenderer;

/**
 * @author schlienger
 */
public class ObservationTable extends JTable
{
  public ObservationTable( final ObservationTableModel model )
  {
    super( model );
    
    setDefaultRenderer( Date.class, new DateTableCellRenderer() );
    setDefaultRenderer( Number.class, new MaskedNumberTableCellRenderer() );
    
    final Color bkg = getSelectionBackground();
    setSelectionBackground( getSelectionForeground() );
    setSelectionForeground( bkg );
  }
}
