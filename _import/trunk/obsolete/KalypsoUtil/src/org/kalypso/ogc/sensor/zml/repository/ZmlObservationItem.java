package org.kalypso.ogc.sensor.zml.repository;

import java.io.File;
import java.net.MalformedURLException;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.zml.ZmlObservation;
import org.kalypso.util.repository.file.FileItem;
import org.kalypso.util.repository.file.FileRepository;

/**
 * An IObservation aware FileItem.
 * 
 * @author schlienger
 */
public class ZmlObservationItem extends FileItem
{
  private ZmlObservation m_zmlFile = null;

  public ZmlObservationItem( final FileRepository rep, final File file )
  {
    super( rep, file );
  }

  /**
   * @see org.kalypso.util.repository.file.FileItem#getAdapter(java.lang.Class)
   */
  public Object getAdapter( Class anotherClass )
  {
    if( anotherClass == IObservation.class )
      try
      {
        return getZmlFileObservation();
      }
      catch( MalformedURLException e )
      {
        // TODO: ok so?
        throw new RuntimeException( e );
      }

    return null;
  }

  /**
   * Helper, lazy loading.
   * 
   * @throws MalformedURLException
   */
  private ZmlObservation getZmlFileObservation() throws MalformedURLException
  {
    // check against the filter
    if( m_zmlFile == null && getRep().getFilter().accept( getFile() ) )
      m_zmlFile = new ZmlObservation( getFile() );

    return m_zmlFile;
  }
}