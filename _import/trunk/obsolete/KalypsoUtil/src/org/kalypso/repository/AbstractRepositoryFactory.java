package org.kalypso.repository;

/**
 * Abstract implementation of the <code>IRepositoryFactory</code> to permit
 * subclasses to inherit from this common functionality.
 * <p>
 * This class mainly provides a constructor with the configuration string
 * that each repository factory will handle differently.
 * 
 * @author schlienger
 */
public abstract class AbstractRepositoryFactory implements IRepositoryFactory
{
  /** configuration string, may be used by subclasses */
  protected String m_configuration;

  /**
   * Constructor with configuration string.
   * 
   * @param conf the configuration string. Client implementation of this class will use
   * this to configure themselves.
   */
  public AbstractRepositoryFactory( final String conf )
  {
    m_configuration = conf;
  }
}
