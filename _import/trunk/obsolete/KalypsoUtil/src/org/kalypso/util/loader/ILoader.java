package org.kalypso.util.loader;

/**
 * @author schlienger
 *
 */
public interface ILoader
{
  public Object load( String source ) throws LoaderException;
  
  public void save( String source, Object data ) throws LoaderException;

  public String getDescription();
  
  public void setSource( final String source ) throws LoaderException;

  public String getSource();
  
  public Object createControl( final Object argument ) throws LoaderException;
}
