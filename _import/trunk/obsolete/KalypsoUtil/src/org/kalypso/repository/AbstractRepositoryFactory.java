package org.kalypso.repository;

/**
 * Abstract implementation of the <code>IRepositoryFactory</code> to permit
 * subclasses to inherit from this common functionality.
 * <p>
 * This class provides support for setting the configuration and readonly parameters.
 * 
 * @author schlienger
 */
public abstract class AbstractRepositoryFactory implements IRepositoryFactory
{
  /** configuration string, may be used by subclasses */
  private String m_configuration;
  
  /** readonly flag */
  private boolean m_readOnly;

  public String getConfiguration()
  {
    return m_configuration;
  }
  
  /**
   * @see org.kalypso.repository.IRepositoryFactory#setConfiguration(java.lang.String)
   */
  public void setConfiguration( final String configuration )
  {
    m_configuration = configuration;
  }
  
  /**
   * @see org.kalypso.repository.IRepositoryFactory#setReadOnly(boolean)
   */
  public void setReadOnly( final boolean ro )
  {
    m_readOnly = ro;
  }
  
  public boolean isReadOnly()
  {
    return m_readOnly;
  }
}
