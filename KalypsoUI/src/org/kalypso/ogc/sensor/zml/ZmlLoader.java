package org.kalypso.ogc.sensor.zml;

import java.io.InputStream;
import java.net.URL;
import java.util.Properties;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.loader.AbstractLoader;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.sensor.DefaultObservationProvider;
import org.kalypso.ogc.sensor.ObservationUtilities;

/**
 * A specific loader for ZML-Files. Load liefert ein IObservationProvider.
 * 
 * @author schlienger
 */
public class ZmlLoader extends AbstractLoader
{
  /**
   * @see org.kalypso.loader.AbstractLoader#loadIntern(java.util.Properties, org.eclipse.core.resources.IProject, org.eclipse.core.runtime.IProgressMonitor)
   */
  protected Object loadIntern( Properties source, IProject project, IProgressMonitor monitor )
      throws LoaderException
  {
    String type = source.getProperty( "TYPE" );
    String location = source.getProperty( "LOCATION" );
    String axis = source.getProperty( "AXIS" );
    
    monitor.beginTask( "Laden von ZML-Datei von " + location, 2 );
    
    InputStream ins = null;
    
    try
    {
    if( type.equals( "relative" ) )
    {
      ins = project.getFile( location ).getContents();
    }
    else if( type.equals( "absolute" ) )
    {
      ins = new URL( location ).openStream();
    }
    }
    catch( Exception e )
    {
      throw new LoaderException( e );
    }
    
    monitor.worked( 1 );
    
    ZmlObservation obs = new ZmlObservation( location, ins );
    
    monitor.worked( 1 );
    
    return new DefaultObservationProvider( obs, ObservationUtilities.findAxis( obs, axis ) );
  }

  /**
   * @see org.kalypso.loader.ILoader#getDescription()
   */
  public String getDescription()
  {
    return "ZML";
  }
}
