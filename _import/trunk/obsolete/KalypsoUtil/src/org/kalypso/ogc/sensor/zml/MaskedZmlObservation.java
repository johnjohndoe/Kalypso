package org.kalypso.ogc.sensor.zml;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;

/**
 * Handles the creation of the masked axis when retrieving the ZmlObservation.
 * 
 * @author schlienger
 */
public class MaskedZmlObservation extends ZmlObservation
{
  public MaskedZmlObservation( File file ) throws MalformedURLException
  {
    super( file );
  }

  public MaskedZmlObservation( String sourceName, URL url )
  {
    super( sourceName, url );
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.ZmlObservation#getAxisList()
   */
  public synchronized IAxis[] getAxisList()
  {
    IAxis[] axes = super.getAxisList();

    for( int i = 0; i < axes.length; i++ )
    {
      if( Number.class.isAssignableFrom( axes[i].getDataClass() )
          && !KalypsoStatusUtils.isStatusAxis( axes[i] ) )
      {
        try
        {
          addAxis( KalypsoStatusUtils.getStatusAxisLabelFor( axes[i] ),
              KalypsoStatusUtils.STATUS_AXIS_UNIT, KalypsoStatusUtils.STATUS_AXIS_DATATYPE,
              KalypsoStatusUtils.STATUS_AXIS_SEPARATOR, KalypsoStatusUtils.STATUS_AXIS_VALUES );
        }
        catch( SensorException e )
        {
          throw new RuntimeException( e );
        }
      }
    }

    return axes;
  }
}