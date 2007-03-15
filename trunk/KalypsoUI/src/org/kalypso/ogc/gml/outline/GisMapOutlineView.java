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
package org.kalypso.ogc.gml.outline;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.commons.command.ICommand;
import org.kalypso.contribs.eclipse.ui.partlistener.PartAdapter;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.command.MoveThemeDownCommand;
import org.kalypso.ogc.gml.command.MoveThemeUpCommand;
import org.kalypso.ogc.gml.command.RemoveThemeCommand;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModellView;
import org.kalypso.ui.editor.mapeditor.AbstractMapPart;
import org.kalypso.ui.views.map.MapView;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypsodeegree.model.feature.event.ModellEvent;

/**
 * A legend view that uses the {@link GisMapOutlineViewer}. Provides the same view on a {@link IMapModell} as a
 * {@link org.kalypso.ui.editor.mapeditor.GisMapOutlinePage}. Usable with all view or editors that extend
 * {@link AbstractMapPart}.
 * 
 * @author Stefan Kurzbach
 */
public class GisMapOutlineView extends ViewPart implements IMapModellView
{
  public static final String ID = "org.kalypso.ui.views.outline";

  GisMapOutlineViewer m_viewer;

  private JobExclusiveCommandTarget m_commandTarget;

  AbstractMapPart m_mapPart;

  private IPartListener m_partListener;

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    final Composite container = new Composite( parent, SWT.FILL );
    container.setLayout( new FillLayout() );
    m_viewer = new GisMapOutlineViewer( null, null );
    m_viewer.createControl( container );
    final IWorkbenchPartSite site = getSite();
    site.setSelectionProvider( m_viewer );

    // initialize view with active map editor or view if any
    final IWorkbenchPage page = site.getPage();
    page.addPartListener( m_partListener );
    final IEditorPart activeEditor = page.getActiveEditor();
    final IViewPart mapView = page.findView( MapView.ID );
    // try to find map editor first
    if( activeEditor != null && activeEditor instanceof AbstractMapPart )
    {
      setMapPart( (AbstractMapPart) activeEditor );
    }
    else if( mapView != null )
    {
      setMapPart( (AbstractMapPart) mapView );
    }
  }

  /**
   * @see org.eclipse.ui.part.ViewPart#init(org.eclipse.ui.IViewSite, org.eclipse.ui.IMemento)
   */
  @Override
  public void init( final IViewSite site, final IMemento memento ) throws PartInitException
  {
    super.init( site, memento );
    m_partListener = new PartAdapter()
    {
      /**
       * @see org.kalypso.contribs.eclipse.ui.partlistener.PartAdapter#partActivated(org.eclipse.ui.IWorkbenchPart)
       */
      @Override
      public void partActivated( final IWorkbenchPart part )
      {
        if( part instanceof AbstractMapPart )
        {
          setMapPart( (AbstractMapPart) part );
        }
      }

      /**
       * @see org.kalypso.contribs.eclipse.ui.partlistener.PartAdapter#partClosed(org.eclipse.ui.IWorkbenchPart)
       */
      @Override
      public void partClosed( final IWorkbenchPart part )
      {
        if( part instanceof AbstractMapPart )
        {
          if( m_mapPart == part )
          {
            setMapPart( null );
          }
        }
      }
    };

    // TODO: this is no good!
    // do not set this to the selection provider of another part!
    // Instead: set this to the slection provider of my site, the StyleEditor should listen to any selection
    // and react accordingly
    // // bei jedem Focus, überprüfe ob outline beim StyleEditor registriert ist.
    // final IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
    // final StyleEditorViewPart part = (StyleEditorViewPart) window.getActivePage().findView(
    // "org.kalypso.ui.editor.mapeditor.views.styleeditor" );
    //
    // if( part != null )
    // part.setSelectionChangedProvider( m_viewer );

  }

  void setMapPart( final AbstractMapPart mapPart )
  {
    m_mapPart = mapPart;
    String newName = "Outline";
    if( m_mapPart != null )
    {
      newName += "(" + mapPart.getPartName() + ")";
      final JobExclusiveCommandTarget commandTarget = m_mapPart.getCommandTarget();
      if( commandTarget != m_commandTarget )
      {
        m_commandTarget = commandTarget;
        m_viewer.setCommandTarget( commandTarget );
      }
      m_mapPart.setMapModellView( this );
    }
    else
    {
      m_viewer.setMapModell( null );
    }
    setPartName( newName );
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#dispose()
   */
  @Override
  public void dispose( )
  {
    if( m_viewer != null )
    {
      getSite().setSelectionProvider( null );
      m_viewer.dispose();
    }
    if( m_mapPart != null )
    {
      m_mapPart.setMapModellView( null );
    }
    super.dispose();
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
   */
  @Override
  public void setFocus( )
  {
    if( m_viewer != null && !m_viewer.getControl().isDisposed() )
      m_viewer.getControl().setFocus();
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModellView#getMapModell()
   */
  public IMapModell getMapModell( )
  {
    return m_viewer.getMapModell();
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModellView#setMapModell(org.kalypso.ogc.gml.mapmodel.IMapModell)
   */
  public void setMapModell( final IMapModell modell )
  {
    m_viewer.setMapModell( modell );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.commons.list.IListManipulator#moveElementDown(java.lang.Object)
   */
  public void moveElementDown( final Object element )
  {
    final MoveThemeUpCommand moveThemeUpCommand = new MoveThemeUpCommand( getMapModell(), (IKalypsoTheme) element );
    m_commandTarget.postCommand( moveThemeUpCommand, new SelectThemeRunner( (IKalypsoTheme) element ) );
  }

  /**
   * @see org.kalypso.commons.list.IListManipulator#moveElementUp(java.lang.Object)
   */
  public void moveElementUp( final Object element )
  {
    m_commandTarget.postCommand( new MoveThemeDownCommand( getMapModell(), (IKalypsoTheme) element ), new SelectThemeRunner( (IKalypsoTheme) element ) );
  }

  /**
   * @see org.kalypso.commons.list.IListManipulator#removeElement(java.lang.Object)
   */
  public void removeElement( final Object element )
  {
    m_commandTarget.postCommand( new RemoveThemeCommand( getMapModell(), (IKalypsoTheme) element ), new SelectThemeRunner( (IKalypsoTheme) element ) );
  }

  /**
   * @see org.kalypso.commons.list.IListManipulator#addElement(java.lang.Object)
   */
  public void addElement( final Object element )
  {
    // m_commandTarget.postCommand( new AddThemeCommand( getMapModell(),
    // (IKalypsoTheme)element ),
    // new SelectThemeRunner( (IKalypsoTheme)element ) );
  }

  private final class SelectThemeRunner implements Runnable
  {
    public final IKalypsoTheme m_theme;

    public SelectThemeRunner( final IKalypsoTheme theme )
    {
      m_theme = theme;
    }

    /**
     * @see java.lang.Runnable#run()
     */
    public void run( )
    {
      getSite().getWorkbenchWindow().getWorkbench().getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          setSelection( new StructuredSelection( m_theme ) );
        }
      } );
    }
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#addSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void addSelectionChangedListener( ISelectionChangedListener listener )
  {
    m_viewer.addSelectionChangedListener( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#getSelection()
   */
  public ISelection getSelection( )
  {
    return m_viewer.getSelection();
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#removeSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void removeSelectionChangedListener( ISelectionChangedListener listener )
  {
    m_viewer.removeSelectionChangedListener( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  public void setSelection( ISelection selection )
  {
    m_viewer.setSelection( selection );
  }

  /**
   * @param command
   * @param runnable
   * @see org.kalypso.util.command.JobExclusiveCommandTarget#postCommand(org.kalypso.commons.command.ICommand,
   *      java.lang.Runnable)
   */
  public void postCommand( ICommand command, Runnable runnable )
  {
    m_commandTarget.postCommand( command, runnable );
  }

}
