package org.kalypso.wizards.import1d2d;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypsomodel1d2d.conv.IPositionProvider;
import org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv;
import org.kalypso.kalypsomodel1d2d.conv.TypeIdAppendIdProvider;
import org.kalypso.kalypsomodel1d2d.conv.XYZOffsetPositionProvider;
import org.kalypso.ui.wizards.imports.Messages;

/**
 * Provides the mechanism for transforming a 2D-Ascii model into a 1d 2d gml model
 * 
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class Transformer implements ICoreRunnableWithProgress
{
  private DataContainer m_data;

  public Transformer( final DataContainer data )
  {
    m_data = data;
  }

  public IStatus execute( final IProgressMonitor monitor )
  {
    boolean hasMonitor = monitor != null;
    try
    {
      if( hasMonitor )
      {
        monitor.beginTask( Messages.getString( "org.kalypso.wizards.import1d2d.Transformer.0" ), 100 ); //$NON-NLS-1$
        monitor.worked( 10 );
        monitor.subTask( Messages.getString( "org.kalypso.wizards.import1d2d.Transformer.1" ) ); //$NON-NLS-1$
      }
      try
      {
        serialize( );
        if( hasMonitor && monitor.isCanceled() )
          return Status.CANCEL_STATUS;
      }
      catch( ClassCastException e )
      {
        return new Status( Status.ERROR, KalypsoCorePlugin.getID(), Status.CANCEL, e.getMessage(), e );
      }
      if( hasMonitor )
        monitor.done();
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return new Status( Status.ERROR, KalypsoCorePlugin.getID(), Status.CANCEL, e.getMessage(), e );
    }
    return Status.OK_STATUS;
  }

  private void serialize( ) throws IllegalStateException
  {
    try
    {
      RMA10S2GmlConv.verboseMode = false;
      IPositionProvider positionProvider = new XYZOffsetPositionProvider( 0.0, 0.0, m_data.getCoordinateSystem( true ) );
      RMA10S2GmlConv.toDiscretisationModel( m_data.getInputFileURL().openStream(), m_data.getFE1D2DDiscretisationModel(), positionProvider, new TypeIdAppendIdProvider() );
    }
    catch( Throwable th )
    {
      th.printStackTrace();
    }
  }

}
