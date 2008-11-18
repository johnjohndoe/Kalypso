package org.kalypso.ogc.sensor.diagview.grafik;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.util.ArrayList;
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
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.zml.Observation;
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

  public RememberForSync( final IFile zmlFile, final IFile datFile, final IAxis numberAxis )
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

  @Override
  public String toString()
  {
    return Messages.getString("org.kalypso.ogc.sensor.diagview.grafik.RememberForSync.0") + m_datFile.getName() + "-" + m_zmlFile.getName() + Messages.getString("org.kalypso.ogc.sensor.diagview.grafik.RememberForSync.2") + m_numberAxis; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  }

  public void synchronizeZml() throws Exception
  {
    if( isInSync() )
    {
      Logger.getLogger( getClass().getName() ).info( Messages.getString("org.kalypso.ogc.sensor.diagview.grafik.RememberForSync.3") + toString() ); //$NON-NLS-1$
      return;
    }

    Logger.getLogger( getClass().getName() ).info( Messages.getString("org.kalypso.ogc.sensor.diagview.grafik.RememberForSync.4") + toString() ); //$NON-NLS-1$

    Reader datReader = null;
    Reader zmlReader = null;
    try
    {
      datReader = new BufferedReader( new InputStreamReader( m_datFile.getContents(), m_datFile.getCharset() ) );

      final CSV csv = new CSV( "\t", 1, true ); //$NON-NLS-1$
      csv.setCommentedLineBeginString( "//" ); //$NON-NLS-1$

      csv.fetch( datReader );
      IOUtils.closeQuietly( datReader );

      zmlReader = new BufferedReader( new InputStreamReader( m_zmlFile.getContents(), m_zmlFile.getCharset() ) );

      final IObservation obs = ZmlFactory.parseXML( new InputSource( zmlReader ), m_zmlFile.getName(), ResourceUtilities
          .createURL( m_zmlFile ) );
      IOUtils.closeQuietly( zmlReader );

      final ITuppleModel values = obs.getValues( null );

      final IAxis[] axes = obs.getAxisList();

      final IAxis dateAxis = ObservationUtilities.findAxisByClass( axes, Date.class );

      // IMPORTANT: copy complete list into ArrayList; the list returned by Array.asList cannot be
      // modified (add/remove not implemented)
      final List<IAxis> axisList = new ArrayList<IAxis>( java.util.Arrays.asList( axes ) );
      axisList.remove( dateAxis );
      axisList.remove( m_numberAxis );

      IAxis statusAxis = null;
      try
      {
        statusAxis = KalypsoStatusUtils.findStatusAxisFor( axes, m_numberAxis );
        axisList.remove( statusAxis );
      }
      catch( final NoSuchElementException e )
      {
        // ignored
      }

      for( int l = 0; l < csv.getLines(); l++ )
      {
        final Date date = GrafikLauncher.GRAFIK_DF.parse( csv.getItem( l, 0 ) );
        final double d = Double.parseDouble( csv.getItem( l, 1 ).replaceAll( ",", "." ) ); // replace all ',' with '.' so that //$NON-NLS-1$ //$NON-NLS-2$
        // Double-parsing always works

        /*
         * Großes TODO: z.Z. werden durch das Grafiktool hinzugefügte oder gelöschte Werte nicht berücksichtigt. Ist
         * auch die Frage ob man es überhaupt unterstützen sollte...
         */
        final int ix = values.indexOf( date, dateAxis );

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
      final Observation xml = ZmlFactory.createXML( obs, null );

      final SetContentHelper helper = new SetContentHelper()
      {
        @Override
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