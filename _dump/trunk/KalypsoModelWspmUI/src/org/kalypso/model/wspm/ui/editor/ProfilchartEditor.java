/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.ui.editor;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.commands.operations.IOperationHistoryListener;
import org.eclipse.core.commands.operations.OperationHistoryEvent;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.util.SafeRunnable;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.ide.IGotoMarker;
import org.eclipse.ui.part.EditorPart;
import org.kalypso.contribs.eclipse.ui.JavaFileEditorInput;
import org.kalypso.contribs.eclipse.ui.actions.RetargetActionManager;
import org.kalypso.contribs.eclipse.ui.partlistener.PartAdapter2;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.ProfilFactory;
import org.kalypso.model.wspm.core.profil.changes.ActiveObjectEdit;
import org.kalypso.model.wspm.core.profil.impl.ProfilEventManager;
import org.kalypso.model.wspm.core.profil.serializer.IProfilSink;
import org.kalypso.model.wspm.core.profil.serializer.IProfilSource;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.core.result.IStationResult;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.preferences.PreferenceConstants;
import org.kalypso.model.wspm.ui.profil.IProfilProvider2;
import org.kalypso.model.wspm.ui.profil.IProfilProviderListener;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilUndoContext;
import org.kalypso.model.wspm.ui.view.AbstractProfilPart;
import org.kalypso.model.wspm.ui.view.IProfilViewProvider;
import org.kalypso.model.wspm.ui.view.ProfilViewData;
import org.kalypso.model.wspm.ui.view.chart.action.ProfilChartActionsEnum;

import de.belger.swtchart.legend.ChartLegend;

/**
 * An editor, which shows one profil as a chart.
 * 
 * @author belger
 */
public class ProfilchartEditor extends EditorPart implements IProfilViewProvider, IGotoMarker, IProfilProvider2
{
  private final AbstractProfilPart m_profilPart = new AbstractProfilPart();

  private IStationResult[] m_results;

  private Collection<IProfilchartEditorListener> m_listener = new LinkedList<IProfilchartEditorListener>();

  private PartAdapter2 m_partAdapter = new PartAdapter2()
  {
    @Override
    public void partOpened( final IWorkbenchPartReference partRef )
    {
      if( partRef.getPart( false ) == ProfilchartEditor.this )
      {
        final boolean opentable = KalypsoModelWspmUIPlugin.getDefault().getPreferenceStore().getBoolean( PreferenceConstants.P_ALLWAYSOPENTABLE );
        if( opentable )
        {
          // the newly opening table needs that this editor is already set as active editor, which
          // is not guaranteed by the framework.
          // so give the framework a bit time to to it (as this is most probalbly the swt-thread
          // it is guaranteed, that the run method
          // is executed after return from this event handling
          getEditorSite().getShell().getDisplay().asyncExec( new Runnable()
          {
            public void run( )
            {
              openTableview();
            }
          } );
        }
      }
    }
  };

  protected final Runnable m_dirtyRunnable = new Runnable()
  {
    @SuppressWarnings("synthetic-access")
    public void run( )
    {
      firePropertyChange( IEditorPart.PROP_DIRTY );
    }
  };

  private IOperationHistoryListener m_undoOperationListener = new IOperationHistoryListener()
  {
    public void historyNotification( final OperationHistoryEvent event )
    {
      fireDirtyChanged();
    }
  };

  private final IResourceChangeListener m_fileListener = new IResourceChangeListener()
  {
    public void resourceChanged( final IResourceChangeEvent event )
    {
      final Shell shell = getEditorSite().getShell();
      if( shell == null || shell.isDisposed() )
        return;

      shell.getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          if( shell.isDisposed() )
            return;
          handleResourceChanged( event );
        }
      } );
    }
  };

  private List<IProfilProviderListener> m_profilProviderListener = new ArrayList<IProfilProviderListener>( 5 );

  private final MenuManager m_menuManager = new MenuManager( "#PopupMenu" ); //$NON-NLS-1$

  public ProfilchartEditor( )
  {
    m_profilPart.getUndoHistory().addOperationHistoryListener( m_undoOperationListener );
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#dispose()
   */
  @Override
  public void dispose( )
  {
    super.dispose();

    getEditorSite().getPage().removePartListener( m_partAdapter );

    m_profilPart.getUndoHistory().removeOperationHistoryListener( m_undoOperationListener );

    ResourcesPlugin.getWorkspace().removeResourceChangeListener( m_fileListener );

    m_menuManager.dispose();

    m_profilPart.dispose();
  }

  /**
   * @see org.eclipse.ui.part.EditorPart#doSave(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public void doSave( final IProgressMonitor monitor )
  {
    final IWorkspace workspace = ResourcesPlugin.getWorkspace();
    try
    {
      // unhook resource listener, because we do change the local file
      workspace.removeResourceChangeListener( m_fileListener );

      monitor.beginTask( "Profil speichern", 2000 );

      final IEditorInput editorInput = getEditorInput();
      final IFile file = (IFile) editorInput.getAdapter( IFile.class );

      final StringWriter prfWriter = new StringWriter();

      final IProfilSink prfSink = KalypsoModelWspmCoreExtensions.createProfilSink( "prf" );
      prfSink.write( getProfil(), prfWriter );
      prfWriter.close();

      monitor.worked( 500 );

      final ByteArrayInputStream bis = new ByteArrayInputStream( prfWriter.toString().getBytes( file.getCharset() ) );
      file.setContents( bis, false, true, new SubProgressMonitor( monitor, 1500 ) );

      // clear undo history, now may undo via resource history
      m_profilPart.getUndoHistory().dispose( m_profilPart.getUndoContext(), true, true, true );
      fireDirtyChanged();
    }
    catch( final IOException e )
    {
      e.printStackTrace();

      MessageDialog.openError( getEditorSite().getShell(), "Profil speichern", "Profil konnte nicht gespeichert werden.\n" + e.getLocalizedMessage() );
    }
    catch( final CoreException e )
    {
      ErrorDialog.openError( getEditorSite().getShell(), "Profil speichern", "Profil konnte nicht gespeichert werden.", e.getStatus() );

      KalypsoModelWspmUIPlugin.getDefault().getLog().log( e.getStatus() );
    }
    finally
    {
      monitor.done();

      workspace.addResourceChangeListener( m_fileListener );
    }
  }

  protected void fireDirtyChanged( )
  {
    final IEditorSite editorSite = getEditorSite();
    if( editorSite == null )
      return;

    final Shell shell = editorSite.getShell();
    if( shell == null )
      return;

    shell.getDisplay().asyncExec( m_dirtyRunnable );
  }

  /**
   * @see org.eclipse.ui.part.EditorPart#doSaveAs()
   */
  @Override
  public void doSaveAs( )
  {
    // hängt davon ab, ob wir stand-alone oder in der workbench laufen
    // je nachdem save in den workspace oder ins file-system

    // erstmal nur ins file-system
    final FileDialog fd = new FileDialog( getEditorSite().getShell(), SWT.SAVE );

    fd.setFileName( "neues Profil" );
    fd.setFilterExtensions( new String[] { "*.prf", "*" } );
    fd.setFilterNames( new String[] { "WspWin Profildatei (*.prf)", "Alle Dateien (*.*)" } );
    fd.setText( "Profil speichern" );

    final IFile fileFromInput = (IFile) getEditorInput().getAdapter( IFile.class );
    final String filterPath = fileFromInput == null ? null : fileFromInput.getParent().getLocation().toOSString();
    fd.setFilterPath( filterPath );

    final String path = fd.open();
    if( path == null )
      return;

    final Job job = new Job( "Profil speichern" )
    {
      @Override
      protected IStatus run( final IProgressMonitor monitor )
      {
        monitor.beginTask( "Profil speichern", 1 );
        try
        {
          final File file = new File( path );
          final PrintWriter pw = new PrintWriter( file );
          final IProfilSink prfSink = KalypsoModelWspmCoreExtensions.createProfilSink( "prf" );
          prfSink.write( getProfil(), pw );
          pw.close();
          setInput( new JavaFileEditorInput( file ) );
        }
        catch( final Exception e )
        {
          return new Status( IStatus.ERROR, KalypsoModelWspmUIPlugin.ID, 0, "Fehler beim Speichern des Profils", e );
        }
        finally
        {
          monitor.done();
        }

        return Status.OK_STATUS;
      }
    };
    job.setUser( true );
    job.schedule();
  }

  /**
   * @see org.eclipse.ui.part.EditorPart#init(org.eclipse.ui.IEditorSite, org.eclipse.ui.IEditorInput)
   */
  @Override
  public void init( final IEditorSite site, final IEditorInput input )
  {
    setSite( site );
    setInput( input );

    final IWorkbenchPage page = site.getPage();
    page.addPartListener( m_partAdapter );

    // add resource listener to listen to changes to my input file
    final IWorkspace workspace = ResourcesPlugin.getWorkspace();
    workspace.addResourceChangeListener( m_fileListener );
  }

  /**
   * public, damit bei Navigation im Strang der Input geändert werden kann.
   * 
   * @see org.eclipse.ui.part.EditorPart#setInput(org.eclipse.ui.IEditorInput)
   */
  @Override
  public void setInput( final IEditorInput input )
  {
    super.setInput( input );

    final IFile file = (IFile) input.getAdapter( IFile.class );
    final String name = (String) input.getAdapter( String.class );

    // TODO: get profiletype from input (possible if input represents a 'Zustand').
    // Use pasche as default.
    final String profiletype = "org.kalypso.model.wspm.tuhh.profiletype";

    final IStationResult[] results = (IStationResult[]) input.getAdapter( IStationResult[].class );
    setResults( results );

    if( file != null )
    {
      loadFile( file, profiletype );

      if( name != null )
        setPartName( "Profil km " + name );
    }
  }

  private void loadFile( final IFile file, final String profiletype )
  {
    if( file == null )
    {
      setProfil( null, null );
      return;
    }

    final Job job = new Job( "Profil laden" )
    {
      @Override
      protected IStatus run( final IProgressMonitor monitor )
      {
        monitor.beginTask( "Lade Profil: " + file.getName(), 2 );
        InputStream contents = null;
        try
        {
          contents = file.getContents();
          final String charset = file.getCharset( true );
          final InputStreamReader reader = new InputStreamReader( contents, charset );

          final IProfilSource source = KalypsoModelWspmCoreExtensions.createProfilSource( "prf" );
          if( source == null )
            throw new Exception( "No Profil-Source found for type 'prf'." );

          final IProfil profil = ProfilFactory.createProfil( profiletype );
          if( !source.read( profil, reader ) )
          {
            contents.close();
            throw new Exception( file.getName() );
          }
          contents.close();
          monitor.worked( 1 );
          if( monitor.isCanceled() )
            return Status.CANCEL_STATUS;

          setProfil( new ProfilEventManager( profil, getResults() ), file );

          return Status.OK_STATUS;
        }
        catch( final Exception e )
        {
          setProfil( null, null );
          return new Status( IStatus.ERROR, KalypsoModelWspmUIPlugin.ID, 0, "Fehler beim Laden eines Profils:\n" + e.getLocalizedMessage(), e );
        }
        finally
        {

          IOUtils.closeQuietly( contents );
          monitor.done();

        }
      }
    };
    job.setUser( true );
    job.schedule();
  }

  public synchronized void setProfil( final IProfilEventManager pem, final IFile file )
  {
    final IProfilEventManager oldPem = m_profilPart.getProfilEventManager();
    final ProfilViewData oldViewData = m_profilPart.getViewData();

    m_profilPart.setProfil( pem, file, getEditorSite().getId() );

    fireOnProfilProviderChanged( oldPem, m_profilPart.getProfilEventManager(), oldViewData, m_profilPart.getViewData() );

    // TODO: remove this, it is deprecated
    fireProfilChanged( pem == null ? null : pem.getProfil() );

    fireDirtyChanged();
  }

  /**
   * @see org.eclipse.ui.part.EditorPart#isDirty()
   */
  @Override
  public boolean isDirty( )
  {
    final ProfilUndoContext undoContext = m_profilPart.getUndoContext();
    return undoContext == null ? false : m_profilPart.getUndoHistory().canUndo( undoContext );
  }

  /**
   * @see org.eclipse.ui.part.EditorPart#isSaveAsAllowed()
   */
  @Override
  public boolean isSaveAsAllowed( )
  {
    return true;
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    m_profilPart.createPartControl( parent );
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
   */
  @Override
  public void setFocus( )
  {
    m_profilPart.setFocus();
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#getAdapter(java.lang.Class)
   */
  @Override
  public Object getAdapter( final Class adapter )
  {
    final Object adapted = m_profilPart.getAdapter( adapter );
    if( adapted != null )
      return adapted;

    if( adapter == ProfilchartEditor.class )
      return this;

    if( adapter == IFile.class )
      return getFile();

    if( adapter == IGotoMarker.class )
      return this;

    if( adapter == IProfilProvider2.class )
      return this;

    return super.getAdapter( adapter );
  }

  public void openTableview( )
  {
    // try
    // {
    // final IWorkbenchPage page = getEditorSite().getPage();
    //
    // // weil eventuell vom letzten Start noch ein Table-Editor da ist, holen wir uns den
    // // als die erste TableView holen, die noch keinen Editor zugeordnet hat
    //
    // final IViewReference[] viewReferences = page.getViewReferences();
    //
    // // if any tableview already is attached to this editor, simply
    // // activate it and return
    // for( final IViewReference reference : viewReferences )
    // {
    // if( reference.getId().equals( TableView.class.getName() ) )
    // {
    // final TableView view = (TableView) reference.getView( false );
    // if( view != null && view.getEditor() == this )
    // {
    // page.activate( view );
    // return;
    // }
    // }
    // }
    //
    // for( final IViewReference reference : viewReferences )
    // {
    // if( reference.getId().equals( TableView.class.getName() ) )
    // {
    // final TableView view = (TableView) reference.getView( false );
    // if( view != null && !view.isAttached() )
    // {
    // view.setEditor( this );
    // return;
    // }
    // }
    // }
    //
    // // keine offene, leere TableView gefunden, neue aufmachen
    // page.showView( TableView.class.getName(), this.toString(), IWorkbenchPage.VIEW_ACTIVATE );
    // }
    // catch( final PartInitException e )
    // {
    // e.printStackTrace();
    // }
  }

  public void addProfilchartEditorListener( final IProfilchartEditorListener l )
  {
    m_listener.add( l );
  }

  public void removeProfilchartEditorListener( final IProfilchartEditorListener l )
  {
    m_listener.remove( l );
  }

  /**
   * @see com.bce.profil.ui.view.IProfilViewProvider#getViewData()
   */
  public ProfilViewData getViewData( )
  {
    return m_profilPart.getViewData();
  }

  /**
   * @see com.bce.profil.ui.view.IProfilProvider#getProfil()
   */
  public IProfil getProfil( )
  {
    return m_profilPart.getProfil();
  }

  public ChartLegend createChartLegend( final Composite control, final int style )
  {
    return m_profilPart.createChartLegend( control, style );
  }

  /**
   * @param chartlegend
   */
  public void saveLegend( final ChartLegend chartlegend )
  {
    m_profilPart.saveLegend( chartlegend );
  }

  public void setResults( final IStationResult[] results )
  {
    m_results = results;
  }

  public IStationResult[] getResults( )
  {
    return m_results;
  }

  public void gotoMarker( final IMarker marker )
  {
    try
    {
      if( !marker.getType().equals( KalypsoModelWspmUIPlugin.MARKER_ID ) )
        return;

      final int pointPos = marker.getAttribute( IValidatorMarkerCollector.MARKER_ATTRIBUTE_POINTPOS, -1 );

      if( pointPos == -1 )
        return;

      // set active point, but dont put this operation in the undo queue
      final IProfil profil = getProfil();
      if( profil == null )
        return;

      final IProfilPoint point = profil.getPoints().get( pointPos );
      final ProfilOperation operation = new ProfilOperation( "", getProfilEventManager(), new ActiveObjectEdit( profil, point, null ), true );
      final IStatus status = operation.execute( new NullProgressMonitor(), null );
      operation.dispose();
      if( !status.isOK() )
        KalypsoModelWspmUIPlugin.getDefault().getLog().log( status );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();

      KalypsoModelWspmUIPlugin.getDefault().getLog().log( e.getStatus() );
    }
  }

  public IProfilEventManager getProfilEventManager( )
  {
    return m_profilPart.getProfilEventManager();
  }

  public void runChartAction( final ProfilChartActionsEnum chartAction )
  {
    m_profilPart.runChartAction( chartAction );
  }

  public void registerCommonGlobalActions( final IActionBars bars )
  {
    final ProfilchartEditorContributor contributor = (ProfilchartEditorContributor) getEditorSite().getActionBarContributor();
    final RetargetActionManager retargetManager = contributor.getRetargetManager();
    retargetManager.registerGlobalActionHandlers( bars );
  }

  protected void handleResourceChanged( final IResourceChangeEvent event )
  {
    final IEditorInput editorInput = getEditorInput();
    if( editorInput == null )
      return;

    final IFile file = (IFile) editorInput.getAdapter( IFile.class );
    if( file == null )
      return;

    final IResourceDelta delta = event.getDelta().findMember( file.getFullPath() );
    if( delta == null )
      return;

    switch( delta.getKind() )
    {
      case IResourceDelta.ADDED:
        break;

      case IResourceDelta.REMOVED:
        MessageDialog.openWarning( getEditorSite().getShell(), "Datei " + file.getLocation().toFile().getAbsoluteFile(), "Die Profildatei wurde gelöscht.\nSpeichern Sie das Profil, um die Datei wiederherzustellen." );
        // todo: was tun? am besten vorschlagen:
        // - profileditor schliessen
        // - profil neu speichern
        // denn ohne resource gibts ständig woanders fehlermeldungen
        break;

      case IResourceDelta.CHANGED:
        // todo: we get here also, when the user chooses replace from local history
        // but in this case we do want to do it anyway

        // only if content has changed
        if( (delta.getFlags() & IResourceDelta.CONTENT) != 0 )
        {
          final StringBuffer message = new StringBuffer();
          message.append( "Die Profildatei wurde geändert.\n" );
          message.append( "Soll die Datei neu geladen werden?\n" );
          if( isDirty() )
            message.append( "ACHTUNG: Die Änderungen im Profileditor gehen verloren!" );

          if( MessageDialog.openQuestion( getEditorSite().getShell(), "Datei " + file.getLocation().toFile().getAbsoluteFile(), message.toString() ) )
            setInput( getEditorInput() );
        }
        break;
    }
  }

  protected void fireProfilChanged( final IProfil newprofil )
  {
    final IProfilchartEditorListener[] listeners = m_listener.toArray( new IProfilchartEditorListener[m_listener.size()] );
    for( final IProfilchartEditorListener l : listeners )
    {
      SafeRunnable.run( new SafeRunnable()
      {
        public void run( ) throws Exception
        {
          l.onProfilChanged( ProfilchartEditor.this, newprofil );
        }
      } );
    }
  }

  /**
   * @see com.bce.profil.ui.view.IProfilProvider2#getEventManager()
   */
  public IProfilEventManager getEventManager( )
  {
    return m_profilPart.getProfilEventManager();
  }

  /**
   * @see com.bce.profil.ui.view.IProfilProvider2#getFile()
   */
  public IFile getFile( )
  {
    return (IFile) getEditorInput().getAdapter( IFile.class );
  }

  /**
   * @see com.bce.profil.ui.view.IProfilProvider2#addProfilProviderListener(com.bce.profil.ui.view.IProfilProviderListener)
   */
  public void addProfilProviderListener( final IProfilProviderListener l )
  {
    m_profilProviderListener.add( l );
  }

  /**
   * @see com.bce.profil.ui.view.IProfilProvider2#removeProfilProviderListener(com.bce.profil.ui.view.IProfilProviderListener)
   */
  public void removeProfilProviderListener( final IProfilProviderListener l )
  {
    m_profilProviderListener.remove( l );
  }

  private void fireOnProfilProviderChanged( final IProfilEventManager oldPem, final IProfilEventManager newPem, final ProfilViewData oldViewData, final ProfilViewData newViewData )
  {
    for( final IProfilProviderListener l : m_profilProviderListener )
      l.onProfilProviderChanged( this, oldPem, newPem, oldViewData, newViewData );
  }

}
