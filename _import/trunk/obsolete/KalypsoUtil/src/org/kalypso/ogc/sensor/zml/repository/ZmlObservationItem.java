package org.kalypso.ogc.sensor.zml.repository;

import java.io.File;
import java.io.FileNotFoundException;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.zml.ZmlFileObservation;
import org.kalypso.util.repository.file.FileItem;
import org.kalypso.util.repository.file.FileRepository;

/**
 * An IObservation aware FileItem.
 * 
 * @author schlienger
 */
public class ZmlObservationItem extends FileItem
{
  private ZmlFileObservation m_zmlFile = null;

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
      catch( FileNotFoundException e )
      {
        // TODO: ok so?
        throw new RuntimeException( e );
      }
    
    return null;
  }

  /**
   * Helper, lazy loading.
   * @throws FileNotFoundException
   */
  private ZmlFileObservation getZmlFileObservation() throws FileNotFoundException
  {
    // check against the filter
    if( m_zmlFile == null && getRep().getFilter().accept( getFile() ) )
      m_zmlFile = new ZmlFileObservation( getFile() );
    
    return m_zmlFile;
  }
}
