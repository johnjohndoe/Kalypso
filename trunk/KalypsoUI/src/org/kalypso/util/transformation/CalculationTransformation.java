package org.kalypso.util.transformation;

import java.io.IOException;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.kalypso.java.lang.CatchThread;
import org.kalypso.java.util.PropertiesHelper;

/**
 * @author belger
 */
public class CalculationTransformation extends AbstractTransformation
{
  /** Dateiname für das Ergebnis */
  public static final String PROP_OUTPUT = "output";

  /** Key/Value Paare */
  public static final String PROP_ENTRY = "entry";

  public void transformIntern( final Properties properties,
      final IProgressMonitor monitor ) throws TransformationException
  {
    try
    {
      final Properties targetProperties = parseProperties( properties );
      final PipedOutputStream pos = new PipedOutputStream();
      final PipedInputStream pis = new PipedInputStream( pos );

      final CatchThread ct = new CatchThread()
      {
        protected void runIntern() throws Throwable
        {
          targetProperties.store( pos, "Steuerparameter der Berechnung" );
          pos.close();
        }
      };
      ct.start();

      final IFile outputFile = ResourcesPlugin.getWorkspace().getRoot().getFile( new Path( properties.getProperty( PROP_OUTPUT ) ) );
      outputFile.create( pis, false, monitor );
    }
    catch( final CoreException e )
    {
      throw new TransformationException( e );
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
  }

  /**
   * Aus den Entries wieder einzelne Properties machen
   */ 
  private Properties parseProperties( final Properties properties )
  {
    final Properties newProps = new Properties();

    for( final Iterator pIt = properties.entrySet().iterator(); pIt.hasNext(); )
    {
      final Map.Entry entry = (Entry)pIt.next();

      final String key = entry.getKey().toString();

      if( key.startsWith( PROP_ENTRY ) )
      {
        final String value = entry.getValue().toString();
        final Properties entryProp = PropertiesHelper.parseFromString( value, '#' );

        for( final Iterator eIt = entryProp.entrySet().iterator(); eIt.hasNext(); )
        {
          final Map.Entry entryEntry = (Entry)eIt.next();

          final String entryKey = entryEntry.getKey().toString();
          final String entryValue = entryEntry.getValue().toString();
          
          newProps.setProperty( entryKey, entryValue );
        }
      }
    }

    return newProps;
  }
}