package org.kalypso.ogc.sensor.tableview.swing.renderer;

import java.awt.Color;
import java.awt.Component;
import java.text.DateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;

/**
 * Helper: formatiert das Datum auf eine richtige Art und Weise
 * 
 * @author schlienger
 */
public class DateTableCellRenderer extends DefaultTableCellRenderer
{
  /** maps dates to markers */
  private final Map m_markers = new HashMap();

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

    // maybe mark this item
    if( m_markers.containsKey( value ) )
    {
      final Marker m = (Marker) m_markers.get( value );
      label.setBackground( m.m_bg );
      label.setForeground( m.m_fg );
      label.setToolTipText( m.m_ttext );
      label.setIcon( m.m_icon );
    }
    else
    {
      label.setBackground( null );
      label.setForeground( null );
      label.setToolTipText( "" );
      label.setIcon( null );
    }

    return label;
  }

  /**
   * Adds a marker that this renderer will show using the given background color
   * and outline paint.
   * 
   * @param date
   *          for which to add marker
   * @param bg
   *          background color, nullable
   * @param fg
   *          foreground color, nullable
   * @param icon
   *          icon, nullable
   * @param ttext
   *          tooltip text, nullable
   */
  public void addMarker( final Date date, final Color bg, final Color fg,
      final Icon icon, final String ttext )
  {
    m_markers.put( date, new Marker( bg, fg, icon, ttext ) );
  }

  /**
   * Clears all markers
   */
  public void clearMarkers( )
  {
    m_markers.clear();
  }

  /**
   * Internal class used as structure containing marker information.
   * 
   * @author schlienger
   */
  private final static class Marker
  {
    protected final Color m_bg;

    protected final Color m_fg;

    protected final Icon m_icon;

    protected final String m_ttext;

    public Marker( Color bg, Color fg, Icon icon, String ttext )
    {
      m_bg = bg;
      m_fg = fg;
      m_icon = icon;
      m_ttext = ttext;
    }
  }
}