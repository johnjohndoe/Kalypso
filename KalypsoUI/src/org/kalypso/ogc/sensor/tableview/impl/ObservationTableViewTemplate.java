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
  public ObservationTableViewTemplate( )
  {
    super();
  }

  /**
   * Sets the observation for this template.
   * 
   * @param obs
   * @param editableColumns
   * @param args
   */
  public void setObservation( final IObservation obs,
      final boolean editableColumns, final IVariableArguments args )
  {
    removeAllColumns();

    final IAxis[] axes = obs.getAxisList();

    // do not even continue if there are no axes
    if( axes.length == 0 )
      return;

    // actually just the first key axis is relevant in our case
    final IAxis[] keyAxes = ObservationUtilities.findAxisByKey( axes );

    // do not continue if no key axis
    if( keyAxes.length != 1 )
      return;

    for( int i = 0; i < axes.length; i++ )
    {
      // ignore axis if it is a kalypso status axis
      if( !KalypsoStatusUtils.isStatusAxis( axes[i] )
          && !axes[i].equals( keyAxes[0] ) )
      {
        final DefaultTableViewColumn col = new DefaultTableViewColumn( axes[i]
            .getName()
            + " - " + axes[i].getUnit(), editableColumns, 50, axes[i].getName(), obs );
        
        col.setArguments( args );

        addColumn( col );
      }
    }
  }
}