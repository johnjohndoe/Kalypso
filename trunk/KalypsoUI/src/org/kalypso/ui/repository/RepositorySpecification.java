package org.kalypso.ui.repository;

import org.kalypso.java.lang.reflect.ClassUtilities;
import org.kalypso.java.lang.reflect.ClassUtilities.ClassUtilityException;
import org.kalypso.repository.IRepositoryFactory;

/**
 * Einfache Repository Spezifikation
 * 
 * @author schlienger
 */
public class RepositorySpecification
{
  private final String m_id;

  private final String m_desc;

  private final String m_factoryClass;

  public RepositorySpecification( final String id, final String desc, final String factoryClass )
  {
    m_id = id;
    m_desc = desc;
    m_factoryClass = factoryClass;
  }

  public String getDesc()
  {
    return m_desc;
  }

  public String getId()
  {
    return m_id;
  }

  /**
   * Erzeugt eine Repository Factory für diese eine Spezifikation.
   * 
   * @throws ClassUtilityException
   */
  public IRepositoryFactory createFactory( ) throws ClassUtilityException
  {
    return (IRepositoryFactory)ClassUtilities.newInstance( m_factoryClass,
        IRepositoryFactory.class, getClass().getClassLoader() );
  }

  /**
   * @see java.lang.Object#toString()
   */
  public String toString()
  {
    return m_desc;
  }
}