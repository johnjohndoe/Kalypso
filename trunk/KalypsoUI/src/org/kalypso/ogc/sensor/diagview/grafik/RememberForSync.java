package org.kalypso.ogc.sensor.diagview.grafik;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.Writer;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.eclipse.util.SetContentHelper;
import org.kalypso.java.util.Arrays;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.util.io.CSV;
import org.kalypso.zml.ObservationType;
import org.xml.sax.InputSource;

/**
 * RememberForSync: remembers which zml file belongs to which grafik dat file
 * for a given axis
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

  public IAxis getNumberAxis( )
  {
    return m_numberAxis;
  }

  public IFile getZmlFile( )
  {
    return m_zmlFile;
  }

  public IFile getDatFile( )
  {
    return m_datFile;
  }

  private boolean isInSync( ) throws CoreException
  {
    m_datFile.getParent().refreshLocal( 1, new NullProgressMonitor( ) );
    
//    return m_modificationStamp == m_datFile.getFullPath().toFile().lastModified();
    return m_modificationStamp == m_datFile.getModificationStamp();
  }
  
  public String toString( )
  {
    return "Grafik-Kalypso MemSync: " + m_datFile.getName() + "-" + m_zmlFile.getName() + " Axis:" + m_numberAxis;
  }
  
  public void synchronizeZml( ) throws Exception
  {
    if( isInSync() )
    {
      Logger.getLogger( getClass().getName() ).info( "In Sync, " + toString() );
      return;
    }
    else
      Logger.getLogger( getClass().getName() ).info( "Update, " + toString() );
    
    Reader datReader = null;
    Reader zmlReader = null;
    try
    {
      datReader = new BufferedReader( new InputStreamReader( m_datFile
          .getContents(), m_datFile.getCharset() ) );

      final CSV csv = new CSV( "\t", 1, true );
      csv.setCommentedLineBeginString( "//" );

      csv.fetch( datReader );
      IOUtils.closeQuietly( datReader );

      zmlReader = new BufferedReader( new InputStreamReader( m_zmlFile
          .getContents(), m_zmlFile.getCharset() ) );

      IObservation obs = ZmlFactory.parseXML( new InputSource( zmlReader ),
          m_zmlFile.getName(), ResourceUtilities.createURL( m_zmlFile ) );
      IOUtils.closeQuietly( zmlReader );
      
      ITuppleModel values = obs.getValues( null );

      IAxis[] axes = obs.getAxisList();

      SimpleTuppleModel stm = new SimpleTuppleModel( axes );

      final IAxis dateAxis = ObservationUtilities.findAxisByClass( axes,
          Date.class )[0];

      final List axisList = Arrays.asList( axes );
      axisList.remove( dateAxis );
      axisList.remove( m_numberAxis );

      for( int l = 0; l < csv.getLines(); l++ )
      {
        Date date = GrafikLauncher.GRAFIK_DF.parse( csv.getItem( l, 0 ) );
        double d = Double.parseDouble( csv.getItem( l, 1 ) );

        Object[] tupple = new Object[axes.length];
        java.util.Arrays.fill( tupple, new Double( 0 ) );

        tupple[values.getPositionFor( dateAxis )] = date;
        tupple[values.getPositionFor( m_numberAxis )] = new Double( d );

        int ix = values.indexOf( date, dateAxis );

        if( ix != -1 )
        {
          for( Iterator it = axisList.iterator(); it.hasNext(); )
          {
            IAxis axis = (IAxis) it.next();

            tupple[values.getPositionFor( axis )] = values
                .getElement( ix, axis );
          }
        }

        stm.addTupple( tupple );
      }

      obs.setValues( stm );
      final ObservationType xml = ZmlFactory.createXML( obs, null );
      
      SetContentHelper helper = new SetContentHelper()
      {
        protected void write( Writer writer ) throws Throwable
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