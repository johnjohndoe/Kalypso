package org.kalypso.ogc.sensor.tableview;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservationProvider;

/**
 * Provides a shared and a value axis to be used within the context of table views.
 * 
 * @author schlienger
 */
public interface ITableObservationProvider extends IObservationProvider
{
  /**
   * Shared axes are used when displaying many IObservation together. The Value axes are
   * merged on the shared axes basis.
   * 
   * @return the shared axis
   */
  public IAxis getSharedAxis();
  
  /**
   * Value axis are used when displaying many IObservations. Each IObservationProvider delivers
   * one and only one value axis that will be displayed "over" the shared axis.
   * 
   * @return the value axis
   */
  public IAxis getValueAxis();
}
