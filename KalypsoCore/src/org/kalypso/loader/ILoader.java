package org.kalypso.loader;

import java.net.URL;
import java.util.Properties;

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
   * @param source information about the location of the resource to load
   * @param context some context for making the relative location of the resource to load absolute
   * @param monitor monitors the progress of loading
   */
  public Object load( final Properties source, final URL context, final IProgressMonitor monitor ) throws LoaderException;
  
  public void save( final Properties source, final URL context, final IProgressMonitor monitor, final Object data ) throws LoaderException;

  /**
   * TODO: document this
   */
  public void release( final Object object );
  
  public void addLoaderListener( final ILoaderListener l );
  
  public void removeLoaderListener( final ILoaderListener l );

  public int compareKeys( final Properties source1, final URL context1, final Properties source2, final URL context2 );
}
