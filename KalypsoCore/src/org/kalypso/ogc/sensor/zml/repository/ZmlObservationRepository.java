package org.kalypso.ogc.sensor.zml.repository;

import java.io.File;
import java.io.FileFilter;

import org.kalypso.repository.IRepositoryFactory;
import org.kalypso.repository.file.FileItem;
import org.kalypso.repository.file.FileRepository;

/**
 * A IObservation aware FileRepository.
 * 
 * @author schlienger
 */
public class ZmlObservationRepository extends FileRepository
{
  public ZmlObservationRepository( final IRepositoryFactory factory, final String root_location, final boolean readOnly, final FileFilter filter )
  {
    super( factory, root_location, readOnly, filter );
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
