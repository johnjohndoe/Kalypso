package org.kalypso.ogc.sensor.zml;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Properties;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.loader.AbstractLoader;
import org.kalypso.loader.LoaderException;

/**
 * A specific loader for ZML-Files. Loads <code>ZmlObservation</code> objects.
 * 
 * @author schlienger
 */
public class ZmlLoader extends AbstractLoader
{
  /**
   * @see org.kalypso.loader.AbstractLoader#loadIntern(java.util.Properties,
   *      org.eclipse.core.resources.IProject,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  protected Object loadIntern( Properties source, IProject project, IProgressMonitor monitor )
      throws LoaderException
  {
    String type = source.getProperty( "TYPE" );
    String location = source.getProperty( "LOCATION" );

    monitor.beginTask( "Laden von ZML-Datei von " + location, 1 );

    URL url = null;

    try
    {
      if( type.equals( "relative" ) )
        url = project.getFile( location ).getLocation().toFile().toURL();
      else if( type.equals( "absolute" ) )
        url = new URL( location );
      else
        throw new LoaderException( "Href Type is not supported: " + type );
    }
    catch( MalformedURLException e )
    {
      try
      {
        // try with file
        url = new File( location ).toURL();
      }
      catch( MalformedURLException e1 )
      {
        throw new LoaderException( e );
      }
    }

    ZmlObservation obs = new ZmlObservation( location, url );

    monitor.worked( 1 );

    return obs;
  }

  /**
   * @see org.kalypso.loader.ILoader#getDescription()
   */
  public String getDescription()
  {
    return "ZML";
  }
}