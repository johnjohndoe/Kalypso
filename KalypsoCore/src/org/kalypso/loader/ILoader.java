package org.kalypso.loader;

import java.net.URL;

import org.eclipse.core.runtime.IProgressMonitor;

/**
 * 
 * 
 * @author schlienger
 */
public interface ILoader
{
  public String getDescription();
  
  /**
   * Loads an object from somewhere.
   * 
   * @param location information about the location of the resource to load
   * @param context some context for making the relative location of the resource to load absolute
   * @param monitor monitors the progress of loading
   * @return object
   * @throws LoaderException
   */
  public Object load( final String location, final URL context, final IProgressMonitor monitor ) throws LoaderException;
  
  public void save( final String location, final URL context, final IProgressMonitor monitor, final Object data ) throws LoaderException;

  /**
   * TODO: document this
   * @param object
   */
  public void release( final Object object );
  
  public void addLoaderListener( final ILoaderListener l );
  
  public void removeLoaderListener( final ILoaderListener l );
}
