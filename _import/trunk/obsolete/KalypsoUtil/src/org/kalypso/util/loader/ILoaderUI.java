package org.kalypso.util.loader;

/**
 * @author belger
 */
public interface ILoaderUI
{
  public void setSource( final String source ) throws LoaderException;
  public String getSource();
  public Object createControl( final Object argument ) throws LoaderException;
}
