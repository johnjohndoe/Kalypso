package org.kalypso.dcadapter;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.IObservationListener;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.util.runtime.IVariableArguments;
import org.kalypso.util.xml.xlink.IXlink;

/**
 * DataCenterObservation
 * 
 * @author marc
 */
public class DataCenterObservation implements IObservation
{
  /**
   * 
   */
  public DataCenterObservation( )
  {
    super();
    // TODO Auto-generated constructor stub
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getIdentifier()
   */
  public String getIdentifier( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getName()
   */
  public String getName( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#isEditable()
   */
  public boolean isEditable( )
  {
    // TODO Auto-generated method stub
    return false;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getTarget()
   */
  public IXlink getTarget( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getMetadataList()
   */
  public MetadataList getMetadataList( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getAxisList()
   */
  public IAxis[] getAxisList( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getValues(org.kalypso.util.runtime.IVariableArguments)
   */
  public ITuppleModel getValues( IVariableArguments args )
      throws SensorException
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#setValues(org.kalypso.ogc.sensor.ITuppleModel)
   */
  public void setValues( ITuppleModel values ) throws SensorException
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getHref()
   */
  public String getHref( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservationEventProvider#addListener(org.kalypso.ogc.sensor.IObservationListener)
   */
  public void addListener( IObservationListener listener )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.ogc.sensor.IObservationEventProvider#removeListener(org.kalypso.ogc.sensor.IObservationListener)
   */
  public void removeListener( IObservationListener listener )
  {
    // TODO Auto-generated method stub

  }

}
