package org.kalypso.ogc.sensor.zml.repository;

import java.io.FileFilter;

import org.kalypso.java.io.filter.MultipleWildCardFileFilter;
import org.kalypso.repository.AbstractRepositoryFactory;
import org.kalypso.repository.IRepository;

/**
 * A simple (and headless) RepositoryFactory for ZmlObservations.
 * 
 * @author schlienger
 */
public class SimpleZmlObservationRepositoryFactory extends AbstractRepositoryFactory
{
  public SimpleZmlObservationRepositoryFactory( String conf )
  {
    super( conf );
  }

  /**
   * Does nothing in this implementation.
   * 
   * @see org.kalypso.repository.IRepositoryFactory#configureRepository(org.kalypso.repository.IRepository)
   */
  public boolean configureRepository( IRepository rep )
  {
    return false;
  }

  /**
   * Creates the <code>ZmlObservationRepository</code> using the configuration
   * inherited from <code>AbstractRepositoryFactory</code>.
   * 
   * @see org.kalypso.repository.IRepositoryFactory#createRepository()
   */
  public IRepository createRepository()
  {
    final FileFilter filter = new MultipleWildCardFileFilter( ZmlObservationRepository.ZML_FILES, false, true, false );
    
    return new ZmlObservationRepository( m_configuration, filter );
  }
}
