package org.kalypso.ogc.sensor.tableview.swing.marker;

import javax.swing.JLabel;

/**
 * Common interface for JLabel markers used in renderers.
 * 
 * @author schlienger
 */
public interface ILabelMarker extends Comparable
{
  /**
   * @param value
   * @return true when the given value is validated by the marker, means the marker can apply
   * its settings on the JLabel that renders the value.
   */
  public boolean validates( Object value );
  
  /**
   * Applies some visual changes to the label to reflect this marker's settings
   * @param label
   */
  public void apply( JLabel label );
  
  /**
   * Resets the label so that it behaves as if this marker would'nt have applied any changes
   * @param label
   */
  public void reset( JLabel label );
}
