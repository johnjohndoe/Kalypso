package org.kalypso.ogc.sensor.timeseries;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.util.runtime.IVariableArguments;
import org.kalypso.util.xml.xlink.IXlink;

/**
 * TODO
 * 
 * @author schlienger
 */
public class WQObservation implements IObservation
{
  private IObservation m_obs;

  public WQObservation( final IObservation obs )
  {
    m_obs = obs;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getIdentifier()
   */
  public String getIdentifier()
  {
    return null;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getName()
   */
  public String getName()
  {
    return null;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#isEditable()
   */
  public boolean isEditable()
  {
    return false;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getTarget()
   */
  public IXlink getTarget()
  {
    return null;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getMetadataList()
   */
  public MetadataList getMetadataList()
  {
    return null;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getAxisList()
   */
  public IAxis[] getAxisList()
  {
    return null;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getValues(org.kalypso.util.runtime.IVariableArguments)
   */
  public ITuppleModel getValues( IVariableArguments args ) throws SensorException
  {
    return null;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#setValues(org.kalypso.ogc.sensor.ITuppleModel)
   */
  public void setValues( ITuppleModel values ) throws SensorException
  {}
}
