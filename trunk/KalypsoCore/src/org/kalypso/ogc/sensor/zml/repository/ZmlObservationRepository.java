package org.kalypso.ogc.sensor.zml.repository;

import java.io.File;
import java.io.FileFilter;

import org.kalypso.repository.file.FileItem;
import org.kalypso.repository.file.FileRepository;

/**
 * A IObservation aware FileRepository.
 * 
 * @author schlienger
 */
public class ZmlObservationRepository extends FileRepository
{
  public final static String[] ZML_FILES = {"*.zml"};
  
  public ZmlObservationRepository( String location, FileFilter filter )
  {
    super( location, filter );
  }

  public ZmlObservationRepository( String location )
  {
    super( location );
  }

  /**
   * @see org.kalypso.repository.file.FileRepository#createItem(java.io.File)
   */
  public FileItem createItem( File file )
  {
    if( !file.isDirectory() )
      return new ZmlObservationItem( this, file );
    
    return super.createItem( file );
  }
}
