package org.kalypso.kalypso1d2d.internal.bce2d.imports;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashSet;
import java.util.Set;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.afgui.model.IModel;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypsomodel1d2d.conv.DiscretisationModel1d2dHandler;
import org.kalypso.kalypsomodel1d2d.conv.IPositionProvider;
import org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler;
import org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv;
import org.kalypso.kalypsomodel1d2d.conv.XYZOffsetPositionProvider;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;

/**
 * Provides the mechanism for transforming a 2D-Ascii model into a 1d 2d gml model
 * 
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class Import2dOperation implements ICoreRunnableWithProgress
{
  private final Import2dData m_data;

  public Import2dOperation( final Import2dData data )
  {
    m_data = data;
  }

  @Override
  public IStatus execute( IProgressMonitor monitor )
  {
    if( monitor == null )
      monitor = new NullProgressMonitor();

    InputStream is = null;
    try
    {
      RMA10S2GmlConv.VERBOSE_MODE = false;
      final IPositionProvider positionProvider = new XYZOffsetPositionProvider( 0.0, 0.0, m_data.getCoordinateSystem() );

      final File importFile = m_data.getInputFileData().getFile();

      // FIXME: counting and using line numbers here is heavy, use monitor-stream instead
      final RMA10S2GmlConv converter = new RMA10S2GmlConv( monitor, getNumberOfLines( importFile ) );
      final Set< Class< ? extends IModel> > lSetModelClassesSetDirty = new HashSet<Class< ? extends IModel> >();
      final CommandableWorkspace workspace = m_data.getCommandableWorkspace( IFEDiscretisationModel1d2d.class.getName() );
      final IRMA10SModelElementHandler handler = new DiscretisationModel1d2dHandler( m_data.getFE1D2DDiscretisationModel(), m_data.getFlowrelationshipModel(), positionProvider, lSetModelClassesSetDirty, workspace );
      converter.setRMA10SModelElementHandler( handler );

      is = new BufferedInputStream( new FileInputStream( importFile ) );
      converter.parse( is );
      is.close();

      if( monitor.isCanceled() )
        return Status.CANCEL_STATUS;

      for( final Class< ? extends IModel> element : lSetModelClassesSetDirty )
      {
        final Class< ? extends IModel> clazz = element;
        m_data.postCommand( clazz.getName(), new EmptyCommand( "Get dirty!", false ) ); //$NON-NLS-1$
      }
    }
    catch( final Exception e )
    {
      return new Status( Status.ERROR, KalypsoCorePlugin.getID(), Status.CANCEL, e.getMessage(), e );
    }
    finally
    {
      IOUtils.closeQuietly( is );
      monitor.done();
    }

    return Status.OK_STATUS;
  }

  // FIXME: heavy operation just to get the lines, is this really necessary?
  public static int getNumberOfLines( final File file )
  {
    if( file == null || !file.exists() )
      return -1;

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
    catch( final IOException e )
    {
      return -1;
    }
    return linesCnt;
  }
}