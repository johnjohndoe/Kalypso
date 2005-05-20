/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
  
---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.repository.view;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.NoSuchElementException;

import org.bce.eclipse.ui.MementoUtils;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.views.properties.IPropertySheetPage;
import org.eclipse.ui.views.properties.PropertySheetEntry;
import org.eclipse.ui.views.properties.PropertySheetPage;
import org.kalypso.ogc.sensor.view.propertySource.ObservationPropertySourceProvider;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.conf.RepositoryFactoryConfig;

/**
 * Wird als ZeitreihenBrowser benutzt.
 * 
 * @author schlienger
 */
public class RepositoryExplorerPart extends ViewPart implements ISelectionProvider, ISelectionChangedListener
{
  private PropertySheetPage m_propsPage = null;

  private IMemento m_memento = null;

  // persistence flags for memento
  private static final String TAG_REPOSITORY = "repository"; //$NON-NLS-1$
  
  private static final String TAG_REPOSITORY_PROPS = "repositoryProperties"; //$NON-NLS-1$

  private static final String TAG_EXPANDED = "expanded"; //$NON-NLS-1$

  private static final String TAG_ELEMENT = "element"; //$NON-NLS-1$

  private static final String TAG_IDENFITIER = "identifier"; //$NON-NLS-1$

  private static final String TAG_REPOSITORIES = "repositories"; //$NON-NLS-1$

  private ObservationChooser m_chooser;

  /**
   * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
   */
  public Object getAdapter( final Class adapter )
  {
    if( adapter == IPropertySheetPage.class )
    {
      // lazy loading
      if( m_propsPage == null || m_propsPage.getControl().isDisposed() )
      {
        // dispose it when not null (not sure if this is ok)
        if( m_propsPage != null )
          m_propsPage.dispose();
        
        // PropertySheetPage erzeugen. Sie wird in das standard PropertySheet
        // von Eclipse dargestellt
        m_propsPage = new PropertySheetPage();

        // eigenes entry mit source provider
        final PropertySheetEntry entry = new PropertySheetEntry();
        entry.setPropertySourceProvider( new ObservationPropertySourceProvider() );

        m_propsPage.setRootEntry( entry );

        m_propsPage.selectionChanged( this, getSelection() );
      }

      return m_propsPage;
    }

    return null;
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#dispose()
   */
  public void dispose()
  {
    removeSelectionChangedListener( this );

    super.dispose();
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( final Composite parent )
  {
    m_chooser = new ObservationChooser( parent, null, getViewSite() );
    addSelectionChangedListener( this );

    if( m_memento != null )
      restoreState( m_memento );
    m_memento = null;
  }

  /**
   * @see org.eclipse.ui.part.ViewPart#init(org.eclipse.ui.IViewSite, org.eclipse.ui.IMemento)
   */
  public void init( final IViewSite site, final IMemento memento ) throws PartInitException
  {
    super.init( site, memento );
    
    m_memento = memento;
  }
  
  /**
   * @see org.eclipse.ui.IWorkbenchPart#setFocus()
   */
  public void setFocus()
  {
  // noch nix
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( final SelectionChangedEvent event )
  {
    if( m_propsPage != null && !m_propsPage.getControl().isDisposed() )
      m_propsPage.selectionChanged( this, event.getSelection() );
  }

  /**
   * @see org.eclipse.ui.part.ViewPart#saveState(org.eclipse.ui.IMemento)
   */
  public void saveState( final IMemento memento )
  {
    if( m_chooser == null )
      return;
    
    final TreeViewer viewer = m_chooser.getViewer();
    if( viewer == null )
    {
      if( m_memento != null ) //Keep the old state;
        memento.putMemento( m_memento );

      return;
    }

    // save list of repositories
    final IMemento repsMem = memento.createChild( TAG_REPOSITORIES );
    for( final Iterator it = m_chooser.getRepositoryContainer().getRepositories().iterator(); it.hasNext(); )
    {
      final IRepository rep = (IRepository)it.next();

      final IMemento child = repsMem.createChild( TAG_REPOSITORY );
      child.putTextData( new RepositoryFactoryConfig( rep ).saveState() );
      
      // save properties for that repository
      final IMemento propsMem = child.createChild( TAG_REPOSITORY_PROPS );
      try
      {
        MementoUtils.saveProperties( propsMem, rep.getProperties() );
      }
      catch( IOException e )
      {
        e.printStackTrace();
      }
    }

    // save visible expanded elements
    final Object expandedElements[] = viewer.getVisibleExpandedElements();
    if( expandedElements.length > 0 )
    {
      final IMemento expandedMem = memento.createChild( TAG_EXPANDED );
      for( int i = 0; i < expandedElements.length; i++ )
      {
        if( expandedElements[i] instanceof IRepositoryItem )
        {
          final IMemento elementMem = expandedMem.createChild( TAG_ELEMENT );
          final String id = ( (IRepositoryItem)expandedElements[i] )
          .getIdentifier();
          elementMem.putString( TAG_IDENFITIER, id );
        }
      }
    }
  }

  /**
   * Restores the state of the receiver to the state described in the specified
   * memento.
   * 
   * @param memento
   *          the memento
   */
  private void restoreState( final IMemento memento )
  {
    if( m_chooser == null )
      return;
    
    final TreeViewer viewer = m_chooser.getViewer();

    final IMemento repsMem = memento.getChild( TAG_REPOSITORIES );
    if( repsMem != null )
    {
      final IMemento[] repMem = repsMem.getChildren( TAG_REPOSITORY );
      for( int i = 0; i < repMem.length; i++ )
      {
        if( repMem[i] == null )
          continue;
          
        try
        {
          final RepositoryFactoryConfig item = RepositoryFactoryConfig.restore( repMem[i].getTextData() );
          if( item != null )
          {
	          final IRepository rep = item.createFactory( getClass().getClassLoader() ).createRepository();
	          
	          final IMemento propsMem = repMem[i].getChild( TAG_REPOSITORY_PROPS );
	          if( propsMem != null )
	            MementoUtils.loadProperties( propsMem, rep.getProperties() );
	          
	          m_chooser.getRepositoryContainer().addRepository( rep );
          }
        }
        catch( Exception e ) // generic exception caught for simplicity
        {
          // ignored
          e.printStackTrace();
        }
      }
    }

    final IMemento childMem = memento.getChild( TAG_EXPANDED );
    if( childMem != null )
    {
      final ArrayList elements = new ArrayList();
      final IMemento[] elementMem = childMem.getChildren( TAG_ELEMENT );
      for( int i = 0; i < elementMem.length; i++ )
      {
        try
        {
          // Marc's Note: commented this out because it is too slow...
          // Also tried to make it faster using m_srv.findItem() in
          // ObservationServiceRepository but the problem is once
          // we got an ItemBean, we still need to make an IRepositoryItem
          // (in fact a ServiceRepositoryItem) using the constructor
          // but we are not able to set all arguments in findItem
          // TODO: try to find a better solution
//          final String id = elementMem[i].getString( TAG_IDENFITIER );
//          final Object element = m_repContainer.findItem( id );
//          
//          if( element != null )
//            elements.add( element );
//          else
//            Logger.getLogger( getClass().getName() ).warning( "Restoring GUI State for observation explorer part: could not find item " + id );
        }
        catch( NoSuchElementException e )
        {
          // ignored
        }
      }
      
      viewer.setExpandedElements( elements.toArray() );
    }
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#getSelection()
   */
  public ISelection getSelection()
  {
    return m_chooser.getSelection();
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  public void setSelection( ISelection selection )
  {
    m_chooser.setSelection( selection );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#addSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void addSelectionChangedListener( ISelectionChangedListener listener )
  {
    if( m_chooser != null )
      m_chooser.addSelectionChangedListener( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#removeSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void removeSelectionChangedListener( ISelectionChangedListener listener )
  {
    if( m_chooser != null )
      m_chooser.removeSelectionChangedListener( listener );
  }
}