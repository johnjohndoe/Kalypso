package org.kalypso.ogc.sensor.tableview.swing.renderer;

import java.awt.Component;
import java.text.DateFormat;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;

import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;

import org.kalypso.ogc.sensor.tableview.swing.marker.ILabelMarker;

/**
 * Helper: formatiert das Datum auf eine richtige Art und Weise
 * 
 * @author schlienger
 */
public class DateTableCellRenderer extends DefaultTableCellRenderer
{
  /** maps dates to markers */
  private final Set m_markers = new TreeSet();

  // TODO: Wenn die Daten keine Zeit-Information haben, dann wird die aktuelle
  // Systemzeit
  // im TableView angezeit!!!
  private final static DateFormat df = DateFormat.getDateTimeInstance();//new

  // SimpleDateFormat(
  // "dd.MM.yyyy
  // HH:mm:ss"
  // );

  /**
   * @see javax.swing.table.DefaultTableCellRenderer#getTableCellRendererComponent(javax.swing.JTable,
   *      java.lang.Object, boolean, boolean, int, int)
   */
  public Component getTableCellRendererComponent( JTable table, Object value,
      boolean isSelected, boolean hasFocus, int row, int column )
  {
    final JLabel label = (JLabel) super.getTableCellRendererComponent( table,
        value, isSelected, hasFocus, row, column );

    label.setText( df.format( value ) );

    if( !isSelected )
    {
      // maybe mark this item
      for( final Iterator it = m_markers.iterator(); it.hasNext(); )
      {
        final ILabelMarker marker = (ILabelMarker) it.next();
        if( marker.validates( value ) )
          marker.apply( label );
        else
          marker.reset( label );
      }
    }
    else
    {
      label.setIcon( null );
    }

    return label;
  }

  /**
   * Adds a marker that modifies this renderer.
   * 
   * @param marker
   */
  public void addMarker( final ILabelMarker marker )
  {
    m_markers.add( marker );
  }

  /**
   * Clears all markers
   */
  public void clearMarkers( )
  {
    m_markers.clear();
  }
}