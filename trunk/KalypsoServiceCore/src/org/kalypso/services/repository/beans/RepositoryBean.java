package org.kalypso.services.repository.beans;

/**
 * A Repository Bean: a repository contains elements that are structured in some way.
 * 
 * @author schlienger
 */
public class RepositoryBean extends ItemBean 
{
  public RepositoryBean()
  {
    super();
  }

  public RepositoryBean( final int id, final String name )
  {
    super( id, name );
  }
}