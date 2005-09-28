package org.kalypso.ogc.sensor.diagview.grafik;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.util.Date;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.logging.Logger;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.commons.io.CSV;
import org.kalypso.commons.resources.SetContentHelper;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.java.util.Arrays;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.zml.ObservationType;
import org.xml.sax.InputSource;

/**
 * RememberForSync: remembers which zml file belongs to which grafik dat file for a given axis
 * 
 * @author schlienger
 */
final class RememberForSync
{
  private final IFile m_zmlFile;

  private final IFile m_datFile;

  private final IAxis m_numberAxis;

  private final long m_modificationStamp;

  public RememberForSync( IFile zmlFile, IFile datFile, IAxis numberAxis )
  {
    m_zmlFile = zmlFile;
    m_datFile = datFile;
    m_numberAxis = numberAxis;

    //    m_modificationStamp = m_datFile.getFullPath().toFile().lastModified();
    m_modificationStamp = m_datFile.getModificationStamp();
  }

  public IAxis getNumberAxis()
  {
    return m_numberAxis;
  }

  public IFile getZmlFile()
  {
    return m_zmlFile;
  }

  public IFile getDatFile()
  {
    return m_datFile;
  }

  private boolean isInSync() throws CoreException
  {
    m_datFile.getParent().refreshLocal( 1, new NullProgressMonitor() );

    //    return m_modificationStamp ==
    // m_datFile.getFullPath().toFile().lastModified();
    return m_modificationStamp == m_datFile.getModificationStamp();
  }

  public String toString()
  {
    return "Grafik-Kalypso MemSync: " + m_datFile.getName() + "-" + m_zmlFile.getName() + " Axis:" + m_numberAxis;
  }

  public void synchronizeZml() throws Exception
  {
    if( isInSync() )
    {
      Logger.getLogger( getClass().getName() ).info( "In Sync, " + toString() );
      return;
    }

    Logger.getLogger( getClass().getName() ).info( "Update, " + toString() );

    Reader datReader = null;
    Reader zmlReader = null;
    try
    {
      datReader = new BufferedReader( new InputStreamReader( m_datFile.getContents(), m_datFile.getCharset() ) );

      final CSV csv = new CSV( "\t", 1, true );
      csv.setCommentedLineBeginString( "//" );

      csv.fetch( datReader );
      IOUtils.closeQuietly( datReader );

      zmlReader = new BufferedReader( new InputStreamReader( m_zmlFile.getContents(), m_zmlFile.getCharset() ) );

      IObservation obs = ZmlFactory.parseXML( new InputSource( zmlReader ), m_zmlFile.getName(), ResourceUtilities
          .createURL( m_zmlFile ) );
      IOUtils.closeQuietly( zmlReader );

      ITuppleModel values = obs.getValues( null );

      IAxis[] axes = obs.getAxisList();

      final IAxis dateAxis = ObservationUtilities.findAxisByClass( axes, Date.class );

      final List axisList = Arrays.asList( axes );
      axisList.remove( dateAxis );
      axisList.remove( m_numberAxis );

      IAxis statusAxis = null;
      try
      {
        statusAxis = KalypsoStatusUtils.findStatusAxisFor( axes, m_numberAxis );
        axisList.remove( statusAxis );
      }
      catch( NoSuchElementException e )
      {
        // ignored
      }

      for( int l = 0; l < csv.getLines(); l++ )
      {
        Date date = GrafikLauncher.GRAFIK_DF.parse( csv.getItem( l, 0 ) );
        double d = Double.parseDouble( csv.getItem( l, 1 ).replaceAll( ",", "." ) ); // replace all ',' with '.' so that
        // Double-parsing always works

        /*
         * Großes TODO: z.Z. werden durch das Grafiktool hinzugefügte oder gelöschte Werte nicht berücksichtigt. Ist
         * auch die Frage ob man es überhaupt unterstützen sollte...
         */
        int ix = values.indexOf( date, dateAxis );

        if( ix != -1 )
        {
          values.setElement( ix, new Double( d ), m_numberAxis );

          if( statusAxis != null )
            values.setElement( ix, KalypsoStati.STATUS_USERMOD, statusAxis );
        }
        else
        {
          // TODO: entweder löschen oder hinzufügen... Wenn überhaupt...
        }
      }

      obs.setValues( values );
      final ObservationType xml = ZmlFactory.createXML( obs, null );

      SetContentHelper helper = new SetContentHelper()
      {
        protected void write( final OutputStreamWriter writer ) throws Throwable
        {
          ZmlFactory.getMarshaller().marshal( xml, writer );
        }
      };

      helper.setFileContents( m_zmlFile, true, false, new NullProgressMonitor(), m_zmlFile.getCharset() );
    }
    finally
    {
      IOUtils.closeQuietly( datReader );
      IOUtils.closeQuietly( zmlReader );
    }
  }
}