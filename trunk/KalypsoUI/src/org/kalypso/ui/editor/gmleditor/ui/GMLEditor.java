/*
 * Created on Jan 19, 2005
 *  
 */
package org.kalypso.ui.editor.gmleditor.ui;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IStorageEditorInput;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.AbstractEditorPart;
import org.kalypso.ui.editor.gmleditor.util.GMLReader;
import org.kalypso.util.command.ICommandTarget;

/**
 * @author F.Lindemann
 *  
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

  protected void loadInternal( final IProgressMonitor monitor, final IStorageEditorInput input )
      throws Exception, CoreException
  {
    if( m_gmlReader != null )
    {
      m_gmlReader.dispose();
      m_gmlReader = null;
    }
    
    BufferedReader br = null;
    try
    {
      final IFile inputFile = ( (IFileEditorInput)getEditorInput() ).getFile();
      final URL context = ResourceUtilities.createURL( inputFile );

      br = new BufferedReader( new InputStreamReader( inputFile.getContents() ) );
      String gmlType = br.readLine();
      String gmlSource = br.readLine();

      m_gmlReader = new GMLReader( gmlType, gmlSource, context );

      getEditorSite().getShell().getDisplay().asyncExec( new Runnable()
      {
        public void run()
        {
          if( m_viewer != null )
            m_viewer.setGmlReader( m_gmlReader );
        }
      } );
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();

      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Fehler beim Parsen der Context-URL", e ) );
    }
    finally
    {
      IOUtils.closeQuietly( br );
      monitor.done();
    }

  }

  public synchronized void createPartControl( final Composite parent )
  {
    super.createPartControl( parent );
    m_viewer = new GMLEditorTreeView( parent, this );
    m_viewer.setGmlReader( m_gmlReader );
  }
}