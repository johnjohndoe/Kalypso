package org.kalypso.repository.file;

import java.io.FileFilter;

import org.kalypso.java.io.filter.AcceptAllFileFilter;
import org.kalypso.java.io.filter.MultipleWildCardFileFilter;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.RepositoryException;
import org.kalypso.repository.factory.AbstractRepositoryFactory;

/**
 * FileRepositoryFactory
 * 
 * @author schlienger
 */
public class FileRepositoryFactory extends AbstractRepositoryFactory
{
  protected final static String SEPARATOR = "#";

  /**
   * @see org.kalypso.repository.factory.IRepositoryFactory#configureRepository()
   */
  public boolean configureRepository( )
  {
    return true;
  }

  /**
   * The configuration string should be build in the following way:
   * <p>
   * root_location[#filter_spec]
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
   * @see org.kalypso.repository.factory.IRepositoryFactory#createRepository()
   */
  public IRepository createRepository( ) throws RepositoryException
  {
    final String[] conf = getConfiguration().split( SEPARATOR );

    if( conf.length < 1 )
      throw new RepositoryException(
          "Invalid configuration in RepositoryFactory: " + getConfiguration() );

    final String location = conf[0];

    final FileFilter filter;

    if( conf.length == 2 )
    {
      final String[] WILDS = conf[1].split( "," );
      filter = new MultipleWildCardFileFilter( WILDS, false, true, false );
    }
    else
      filter = new AcceptAllFileFilter();

    return createRepository( getConfiguration(), location, getRepositoryName(), isReadOnly(),
        filter );
  }

  /**
   * Actually instanciates a FileRepository. This default implementation
   * instanciates a FileRepository. You can override this method to instanciate
   * your custom version of FileRepository.
   * 
   * @param conf
   * @param location
   * @param id
   * @param ro
   * @param filter
   * @return instance of FileRepository
   */
  public FileRepository createRepository( String conf, String location, String id,
      boolean ro, FileFilter filter )
  {
    return new FileRepository( getClass().getName(), conf, location, id, ro, filter );
  }
}