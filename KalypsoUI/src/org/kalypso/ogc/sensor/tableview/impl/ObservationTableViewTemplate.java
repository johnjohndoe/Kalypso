package org.kalypso.ogc.sensor.tableview.impl;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.util.runtime.IVariableArguments;

/**
 * An <code>ITableViewTemplate</code> designed to be used with an
 * <code>IObservation</code>.
 * 
 * @author schlienger
 */
public class ObservationTableViewTemplate extends DefaultTableViewTemplate
{
  /** If set, axises with this type will be ignored */
  private String m_ignoreType;

  public ObservationTableViewTemplate()
  {
    super();
  }
  
  public void setIgnoreType( final String ignoreType )
  {
    m_ignoreType = ignoreType;
  }

  /**
   * Sets the observation for this template.
   * 
   * @param obs
   * @param editableColumns
   * @param args
   */
  public void setObservation( final IObservation obs, final boolean editableColumns,
      final IVariableArguments args )
  {
    removeAllThemes();

    addObservation( obs, editableColumns, args );
  }

  /**
   * Adds an observation and its values as columns to this template.
   * 
   * @param obs
   * @param editableColumns
   * @param args
   */
  public void addObservation( final IObservation obs, final boolean editableColumns,
      final IVariableArguments args )
  {
    final IAxis[] axes = obs.getAxisList();

    // do not even continue if there are no axes
    if( axes.length == 0 )
      return;

    // actually just the first key axis is relevant in our case
    final IAxis[] keyAxes = ObservationUtilities.findAxisByKey( axes );

    // do not continue if no key axis
    if( keyAxes.length != 1 )
      return;

    final DefaultTableViewTheme theme = new DefaultTableViewTheme();
    theme.setObservation( obs );
    theme.setArguments( args );

    for( int i = 0; i < axes.length; i++ )
    {
      // ignore axis if it is a kalypso status axis
      if( !KalypsoStatusUtils.isStatusAxis( axes[i] ) && !axes[i].equals( keyAxes[0] ) )
      {
        final IAxis valueAxis = ObservationUtilities.findAxisByName( axes, axes[i].getName() );

        if( !valueAxis.getType().equals( m_ignoreType ) )
        {
          final DefaultTableViewColumn col = new DefaultTableViewColumn( theme.getName() + " (" + axes[i].getName() + " - "
              + axes[i].getUnit() + ")", editableColumns, 50, keyAxes[0], valueAxis, theme );

          theme.addColumn( col );
        }
      }
    }

    addTheme( theme );
  }
}