package org.kalypso.model.transformation;

import java.io.IOException;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.java.lang.CatchThread;
import org.kalypso.ui.nature.ModelNature;

/**
 * @author belger
 */
public class CalculationTransformation extends AbstractTransformation
{
  /**
   * @throws TransformationException
   * @see org.kalypso.model.transformation.ICalculationCaseTransformation#transform(org.eclipse.core.resources.IFolder, org.eclipse.core.runtime.IProgressMonitor)
   */
  public void transformIntern( final IFolder targetFolder, final Properties properties, final IProgressMonitor monitor ) throws TransformationException
  {
    try
    {
      final Properties targetProperties = parseProperties(  properties ); 
      
      final IFile outputFile = targetFolder.getFile( ModelNature.CALCULATION_FILE );

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

  /** Ersetzt bestimmte Schlüsselwörter in den Properties  
   * @throws TransformationException*/
  private Properties parseProperties( final Properties properties ) throws TransformationException
  {
    final Properties newProps = new Properties();
    
    for( final Iterator pIt = properties.entrySet().iterator(); pIt.hasNext(); )
    {
      final Map.Entry entry = (Entry)pIt.next();
      
      final String value = (String)entry.getValue();

      // nach Schlüselwort suchen
      // TODO: allgemeinerer Mechanismus?
      final StringBuffer restValue = new StringBuffer( value );
      final StringBuffer newValue = new StringBuffer();
      while( restValue.length() > 0 )
      {
        final int startPos = restValue.indexOf( "${" );

        if( startPos == 0 )
        {
          final int endPos = restValue.indexOf( "}", 2 );
          if( endPos == -1 )
            throw new TransformationException( "Fehlende schliessende Klammer } beim Parsen eines Schlüsselwortes: " + entry.getKey() );
          
          final String keyWord = restValue.substring( 2, endPos );
          newValue.append( parseKeyWord( keyWord ) );
          restValue.delete( 0, endPos + 1 );
        }
        else
        {
          final int endPos = startPos == -1 ? restValue.length() : startPos;
          newValue.append( restValue.subSequence( 0, endPos ) );
          restValue.delete( 0 , endPos );
        }
      }
      
      newProps.setProperty( (String)entry.getKey(), newValue.toString() );
    }
    
    return newProps;
  }

  private String parseKeyWord( final String keyWord ) throws TransformationException
  {
    if( keyWord.equalsIgnoreCase( "TIME" ) == true )
      return new SimpleDateFormat( "dd.MM.yyyy HH:mm" ).format( new Date( System.currentTimeMillis() ) );
    
    throw new TransformationException( "Unbekanntes Schlüsselwort: " + keyWord );
  }
}
