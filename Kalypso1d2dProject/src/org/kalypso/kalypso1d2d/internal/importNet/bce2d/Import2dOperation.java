package org.kalypso.kalypso1d2d.internal.importNet.bce2d;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.core.runtime.ProgressInputStream;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.conv.DiscretisationModel1d2dHandler;
import org.kalypso.kalypsomodel1d2d.conv.IPositionProvider;
import org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv;
import org.kalypso.kalypsomodel1d2d.conv.XYZOffsetPositionProvider;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

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
  public IStatus execute( IProgressMonitor monitor ) throws CoreException, InvocationTargetException, InterruptedException
  {
    if( monitor == null )
      monitor = new NullProgressMonitor();

    InputStream is = null;
    try
    {
      final File importFile = m_data.getInputFileData().getFile();
      final long contentLength = importFile.length();
      monitor.beginTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv.0" ), (int) contentLength );//$NON-NLS-1$

      // TODO: use this position provider to import 2d-files with missing 6th coordinate -> ask user for offset values
      final IPositionProvider positionProvider = new XYZOffsetPositionProvider( 0.0, 0.0, m_data.getCoordinateSystem() );

      final IScenarioDataProvider szenarioDataProvider = m_data.getSzenarioDataProvider();
      final DiscretisationModel1d2dHandler handler = new DiscretisationModel1d2dHandler( szenarioDataProvider, positionProvider );
      handler.setImportRoughness( m_data.getImportRoughness() );

      final BufferedInputStream fis = new BufferedInputStream( new FileInputStream( importFile ) );
      is = new ProgressInputStream( fis, contentLength, monitor );

      final RMA10S2GmlConv converter = new RMA10S2GmlConv( monitor );
      converter.setRMA10SModelElementHandler( handler );

      converter.parse( is );
      is.close();

      ProgressUtilities.worked( monitor, 0 );

      final String[] dirtyModels = handler.getDirtyModels();
      for( final String modelID : dirtyModels )
      {
        szenarioDataProvider.postCommand( modelID, new EmptyCommand( "Get dirty!", false ) ); //$NON-NLS-1$
      }

      return handler.getStatus();
    }
    catch( final CoreException e )
    {
      throw e;
    }
    catch( final IOException e )
    {
      // better error message needed?
      throw new InvocationTargetException( e );
    }
    catch( final OperationCanceledException e )
    {
      throw new InterruptedException();
    }
    catch( final Exception e )
    {
      // Should never happen; problems accessing the models
      throw new InvocationTargetException( e );
    }
    finally
    {
      IOUtils.closeQuietly( is );
      monitor.done();
    }
  }
}