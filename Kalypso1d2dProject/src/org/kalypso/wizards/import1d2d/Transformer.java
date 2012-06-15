package org.kalypso.wizards.import1d2d;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypsomodel1d2d.conv.DiscretisationModel1d2dHandler;
import org.kalypso.kalypsomodel1d2d.conv.IPositionProvider;
import org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler;
import org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv;
import org.kalypso.kalypsomodel1d2d.conv.XYZOffsetPositionProvider;

/**
 * Provides the mechanism for transforming a 2D-Ascii model into a 1d 2d gml model
 * 
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class Transformer implements ICoreRunnableWithProgress
{
  private final DataContainer m_data;

  public Transformer( final DataContainer data )
  {
    m_data = data;
  }

  @Override
  public IStatus execute( IProgressMonitor monitor )
  {
    if( monitor == null )
      monitor = new NullProgressMonitor();
    try
    {
      RMA10S2GmlConv.VERBOSE_MODE = false;
      final IPositionProvider positionProvider = new XYZOffsetPositionProvider( 0.0, 0.0, m_data.getCoordinateSystem( true ) );
      final RMA10S2GmlConv converter = new RMA10S2GmlConv( monitor, getNumberOfLines( m_data.getInputFile() ) );
      final IRMA10SModelElementHandler handler = new DiscretisationModel1d2dHandler( m_data.getFE1D2DDiscretisationModel(), positionProvider );
      converter.setRMA10SModelElementHandler( handler );
      converter.parse( m_data.getInputFileURL().openStream() );
      if( monitor.isCanceled() )
        return Status.CANCEL_STATUS;
      monitor.done();
      /* post empty command(s) in order to make pool dirty. */
      // m_data.postCommand( IFEDiscretisationModel1d2d.class, new EmptyCommand( Messages.getString("Transformer.0"),
      // false ) ); //$NON-NLS-1$
    }
    catch( Exception e )
    {
      return new Status( Status.ERROR, KalypsoCorePlugin.getID(), Status.CANCEL, e.getMessage(), e );
    }
    return Status.OK_STATUS;
  }

  public static int getNumberOfLines( String fileName )
  {
    final File file = new File( fileName );
    if( file == null || !file.exists() )
    {
      return -1;
    }
    int linesCnt = 0;
    try
    {
      final FileReader file_reader = new FileReader( file );
      final BufferedReader buf_reader = new BufferedReader( file_reader );
      do
      {
        final String line = buf_reader.readLine();
        if( line == null )
          break;
        linesCnt++;
      }
      while( true );
      buf_reader.close();
    }
    catch( IOException e )
    {
      return -1;
    }
    return linesCnt;
  }

}
