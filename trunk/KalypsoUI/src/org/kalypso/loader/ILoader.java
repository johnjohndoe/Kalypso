package org.kalypso.loader;

import java.util.Properties;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;

/**
 * @author schlienger
 *
 */
public interface ILoader
{
  public Object load( final Properties source, final IProject project, final IProgressMonitor monitor ) throws LoaderException;
  
  public void save( final Properties source, final Object data ) throws LoaderException;

  public String getDescription();
  
  public void addLoaderListener( final ILoaderListener l );
  
  public void removeLoaderListener( final ILoaderListener l );

  public void release( final Object object );
}
