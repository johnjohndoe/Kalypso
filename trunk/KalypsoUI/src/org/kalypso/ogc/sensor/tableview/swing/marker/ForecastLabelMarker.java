package org.kalypso.ogc.sensor.tableview.swing.marker;

import java.awt.Color;
import java.util.Date;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JLabel;

import org.kalypso.ogc.sensor.tableview.swing.ObservationTable;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.util.runtime.args.DateRangeArgument;

/**
 * ForecastLabelMarker
 * 
 * @author schlienger
 */
public class ForecastLabelMarker implements ILabelMarker
{
  private final static Color FORECAST_BG = TimeserieUtils.getColorForMD( TimeserieConstants.MD_VORHERSAGE );

  private final static String FORECAST_TT = TimeserieConstants.MD_VORHERSAGE;

//  private final static Icon FORECAST_ICON = new ImageIcon(
//      ObservationTable.class.getResource( "resource/warning_small.gif" ) );

  private final DateRangeArgument m_dra;

  /**
   * Constructor
   * 
   * @param dra
   */
  public ForecastLabelMarker( DateRangeArgument dra )
  {
    m_dra = dra;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.swing.marker.ILabelMarker#validates(java.lang.Object)
   */
  public boolean validates( final Object value )
  {
    if( !(value instanceof Date) )
      return false;

    return m_dra.contains( (Date) value );
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.swing.marker.ILabelMarker#apply(javax.swing.JLabel)
   */
  public void apply( final JLabel label )
  {
    label.setBackground( FORECAST_BG );
    label.setToolTipText( FORECAST_TT );
    //label.setIcon( FORECAST_ICON );
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.swing.marker.ILabelMarker#reset(javax.swing.JLabel)
   */
  public void reset( JLabel label )
  {
    label.setBackground( null );
    label.setToolTipText( "" );
    label.setIcon( null );
  }

  /**
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  public int compareTo( Object o )
  {
    if( !(o instanceof ForecastLabelMarker) )
      return -1;

    return m_dra.compareTo( ((ForecastLabelMarker) o).m_dra );
  }
}