package org.kalypso.ogc.sensor.diagview;

import org.kalypso.ogc.sensor.IAxis;

/**
 * Maps an observation axis to a diagram axis
 * 
 * @author schlienger
 *
 */
public interface IAxisMapping
{
  public IAxis getObservationAxis();
  public IDiagramAxis getDiagramAxis();
}
