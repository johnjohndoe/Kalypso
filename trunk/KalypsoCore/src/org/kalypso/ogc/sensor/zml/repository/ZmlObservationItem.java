package org.kalypso.ogc.sensor.zml.repository;

import java.io.File;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.repository.file.FileItem;
import org.kalypso.repository.file.FileRepository;

/**
 * An IObservation aware FileItem.
 * 
 * @author schlienger
 */
public class ZmlObservationItem extends FileItem
{
  private IObservation m_zmlFile = null;

  public ZmlObservationItem( final FileRepository rep, final File file )
  {
    super( rep, file );
  }

  /**
   * @see org.kalypso.repository.file.FileItem#getAdapter(java.lang.Class)
   */
  public Object getAdapter( Class anotherClass )
  {
    try
    {
      if( anotherClass == IObservation.class )
        return getZmlFileObservation();
    }
    catch( SensorException e )
    {
      // TODO handling
      throw new RuntimeException( e );
    }

    return null;
  }

  /**
   * Helper, lazy loading.
   * 
   * @throws SensorException
   */
  private IObservation getZmlFileObservation() throws SensorException
  {
    try
    {
      // check against the filter
      if( m_zmlFile == null && getRep().getFilter().accept( getFile() ) )
      {
        final File f = getFile();
        m_zmlFile = ZmlFactory.parseXML( f.toURL(), f.getAbsolutePath() );
      }

      return m_zmlFile;
    }
    catch( Exception e )
    {
      throw new SensorException( e );
    }
  }
}