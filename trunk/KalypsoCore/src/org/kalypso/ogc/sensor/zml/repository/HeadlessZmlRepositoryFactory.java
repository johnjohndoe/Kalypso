package org.kalypso.ogc.sensor.zml.repository;

import java.io.FileFilter;

import org.kalypso.java.io.filter.AcceptAllFileFilter;
import org.kalypso.java.io.filter.MultipleWildCardFileFilter;
import org.kalypso.repository.AbstractRepositoryFactory;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.RepositoryException;

/**
 * A simple (and headless) RepositoryFactory for ZmlObservations.
 * 
 * @author schlienger
 */
public class HeadlessZmlRepositoryFactory extends AbstractRepositoryFactory
{
  /**
   * Does nothing.
   * 
   * @see org.kalypso.repository.IRepositoryFactory#configureRepository()
   */
  public boolean configureRepository(  )
  {
    return true;
  }

  /**
   * The configuration string should be build in the following way:
   * 
   * <pre>
   * root_location[#filter_spec]
   * </pre>
   * 
   * <p>
   * root_location: the location of the root directory
   * <p>
   * filter_spec: [optional] the filter specification as used in
   * <code>MultipleWildCardFileFilter</code>
   * 
   * <p>
   * Beispiel:
   * <p>
   * c:/temp#*.txt,*.ini
   * <p>
   * 
   * @see org.kalypso.repository.IRepositoryFactory#createRepository()
   */
  public IRepository createRepository() throws RepositoryException
  {
    final String[] conf = getConfiguration().split( "#" );

    if( conf.length == 0 )
      throw new RepositoryException( "Invalid configuration in RepositoryFactory" );

    final FileFilter filter;

    if( conf.length == 2 )
    {
      final String[] ZML_FILES = conf[1].split( "," );
      filter = new MultipleWildCardFileFilter( ZML_FILES, false, true, false );
    }
    else
      filter = new AcceptAllFileFilter();

    return new ZmlObservationRepository( this, conf[0], isReadOnly(), filter );
  }
}