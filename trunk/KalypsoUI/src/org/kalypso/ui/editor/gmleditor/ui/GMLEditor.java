/*
 * Created on Jan 19, 2005
 *  
 */
package org.kalypso.ui.editor.gmleditor.ui;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
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

  protected GMLEditorTreeView viewer = null;
  
  protected GMLReader gmlReader = null;

  public GMLEditor()
  {
    /**/
  }

  public void dispose()
  {
    viewer.dispose();
    super.dispose();
  }
  
  protected void doSaveInternal( IProgressMonitor monitor, IFileEditorInput input )
  {
  // TODO Auto-generated method stub
  }

  protected void loadInternal( final IProgressMonitor monitor,
      final IStorageEditorInput input ) throws Exception, CoreException
  {
    if( viewer == null )
      return;

    try
    {
      final IFile inputFile = ( (IFileEditorInput)getEditorInput() ).getFile();
      final URL context = ResourceUtilities.createURL( inputFile );

      BufferedReader br = new BufferedReader( new InputStreamReader( inputFile.getContents() ) );
      String gmlType = br.readLine();
      String gmlSource = br.readLine();
      br.close();
      
      gmlReader = new GMLReader( gmlType, gmlSource, context );           

      getEditorSite().getShell().getDisplay().asyncExec( new Runnable()
      {
        public void run()
        {
          viewer.setGmlReader(gmlReader);
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
      monitor.done();
    }

  }

  public synchronized void createPartControl( final Composite parent )
  {    
    super.createPartControl( parent );
    viewer = new GMLEditorTreeView( parent );
    load();
  }
}