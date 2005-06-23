/*
 * Created on Jan 19, 2005
 *  
 */
package org.kalypso.ui.editor.gmleditor.ui;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.part.FileEditorInput;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.AbstractEditorPart;
import org.kalypso.ui.editor.gmleditor.util.GMLReader;

/**
 * @author F.Lindemann
 */
public class GMLEditor extends AbstractEditorPart implements ICommandTarget
{
  protected GMLEditorTreeView m_viewer = null;

  protected GMLReader m_gmlReader = null;

  public GMLEditor()
  {
  /**/
  }

  public void dispose()
  {
    if( m_gmlReader != null )
      m_gmlReader.dispose();

    if( m_viewer != null )
      m_viewer.dispose();

    super.dispose();
  }

  protected void doSaveInternal( IProgressMonitor monitor, IFileEditorInput input )
  {
  // not implemented
  }

  public GMLEditorTreeView getTreeView()
  {
    return m_viewer;
  }

  protected void loadInternal( final IProgressMonitor monitor, final IStorageEditorInput input ) throws Exception,
      CoreException
  {
    monitor.beginTask( "Vorlage wird geladen", 1000 );
    if( m_gmlReader != null )
    {
      m_gmlReader.dispose();
      m_gmlReader = null;
    }

    try
    {
      m_gmlReader = createReaderFromInput( input );

      getEditorSite().getShell().getDisplay().asyncExec( new Runnable()
      {
        public void run()
        {
          if( m_viewer != null )
            m_viewer.setGmlReader( m_gmlReader );
        }
      } );
    }
    finally
    {
      monitor.done();
    }

  }

  private GMLReader createReaderFromInput( final IStorageEditorInput input ) throws CoreException
  {
    BufferedReader br = null;

    try
    {
      final IFile inputFile = ( (IFileEditorInput)input ).getFile();
      final URL context = ResourceUtilities.createURL( inputFile );

      final String gmlType;
      final String gmlSource;
      if( input instanceof GmlEditorInput )
      {
        final GmlEditorInput gmlEditorInput = (GmlEditorInput)input;
        gmlType = gmlEditorInput.getLinktype();
        gmlSource = context.toString();
      }
      else
      {
        br = new BufferedReader( new InputStreamReader( inputFile.getContents() ) );
        gmlType = br.readLine();
        gmlSource = br.readLine();
      }

      return new GMLReader( gmlType, gmlSource, context );
    }
    catch( final IOException e )
    {
      e.printStackTrace();

      throw new CoreException( KalypsoGisPlugin.createErrorStatus( "Fehler beim Laden der Vorlagendatei.", e ) );
    }
    finally
    {
      IOUtils.closeQuietly( br );
    }
  }

  public synchronized void createPartControl( final Composite parent )
  {
    super.createPartControl( parent );
    m_viewer = new GMLEditorTreeView( parent );//, this );
    m_viewer.setGmlReader( m_gmlReader );
  }

  public final static class GmlEditorInput extends FileEditorInput
  {
    private final String m_linktype;

    public GmlEditorInput( final String linktype, final IFile file )
    {
      super( file );
      m_linktype = linktype;
    }

    public String getLinktype()
    {
      return m_linktype;
    }
  }
}