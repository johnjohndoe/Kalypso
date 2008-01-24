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
package org.kalypso.model.wspm.ui.view.table;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.contexts.IContextService;
import org.eclipse.ui.operations.UndoRedoActionGroup;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.contribs.eclipse.jface.viewers.DefaultTableViewer;
import org.kalypso.contribs.eclipse.ui.partlistener.AdapterPartListener;
import org.kalypso.contribs.eclipse.ui.partlistener.EditorFirstAdapterFinder;
import org.kalypso.contribs.eclipse.ui.partlistener.IAdapterEater;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilListener;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.editor.ProfilchartEditor;
import org.kalypso.model.wspm.ui.profil.IProfilProvider2;
import org.kalypso.model.wspm.ui.profil.IProfilProviderListener;
import org.kalypso.model.wspm.ui.view.ProfilViewData;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.ITupleResultChangedListener;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.om.table.TupleResultCellModifier;
import org.kalypso.ogc.gml.om.table.TupleResultContentProvider;
import org.kalypso.ogc.gml.om.table.TupleResultLabelProvider;
import org.kalypso.ogc.gml.om.table.handlers.ComponentUiHandlerFactory;
import org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypsodeegree.model.feature.Feature;

/**
 * TableView für ein Profil. Ist eine feste View auf genau einem(!) Editor.
 * 
 * @author belger
 */
public class TableView extends ViewPart implements IPropertyChangeListener, IAdapterEater, IProfilProviderListener
{
  private final AdapterPartListener m_profilProviderListener = new AdapterPartListener( IProfilProvider2.class, this, EditorFirstAdapterFinder.instance(), EditorFirstAdapterFinder.instance() );

  private Composite m_control;

  private UndoRedoActionGroup m_group;

  private IProfilProvider2 m_provider;

  protected IProfilEventManager m_pem;

  private DefaultTableViewer m_view;

  private TupleResultContentProvider m_tupleResultContentProvider;

  private TupleResultLabelProvider m_tupleResultLabelProvider;

  private final IProfilListener m_profileListener = new IProfilListener()
  {
    public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
    {
      new UIJob( "updating cross section table..." )
      {
        @Override
        public IStatus runInUIThread( IProgressMonitor monitor )
        {
          updateControl();

          return Status.OK_STATUS;
        }
      }.schedule();
    }
  };

  private final ITupleResultChangedListener m_changedListener = new ITupleResultChangedListener()
  {
    public void componentsChanged( final IComponent[] components, final TYPE type )
    {
      /** not fired, because of eventloop with pem */
// final ProfilChangeHint hint = new ProfilChangeHint();
// hint.setProfilPropertyChanged( true );
//
// m_pem.fireProfilChanged( hint, new IProfilChange[] {} );
    }

    public void recordsChanged( final IRecord[] records, final TYPE type )
    {
// /** not fired, because of eventloop with pem */
// final ProfilChangeHint hint = new ProfilChangeHint();
// hint.setPointsChanged();
//
// m_pem.fireProfilChanged( hint, new IProfilChange[] { new TupleResultChange() } );
    }

    public void valuesChanged( final ValueChange[] changes )
    {
// final ProfilChangeHint hint = new ProfilChangeHint();
// hint.setPointValuesChanged();
//
// m_pem.fireProfilChanged( hint, new IProfilChange[] { new TupleResultChange() } );
    }
  };

  /**
   * @see org.eclipse.ui.part.ViewPart#init(org.eclipse.ui.IViewSite)
   */
  @Override
  public void init( final IViewSite site ) throws PartInitException
  {
    super.init( site );

    m_profilProviderListener.init( site.getPage() );

    KalypsoModelWspmUIPlugin.getDefault().getPreferenceStore().addPropertyChangeListener( this );
  }

  @Override
  public void dispose( )
  {
    super.dispose();

    if( m_pem != null )
      m_pem.removeProfilListener( m_profileListener );

    unhookProvider();

    m_tupleResultContentProvider.dispose();
    m_tupleResultLabelProvider.dispose();

    m_profilProviderListener.dispose();

    if( m_group != null )
      m_group.dispose();

    if( m_control != null )
      m_control.dispose();

    KalypsoModelWspmUIPlugin.getDefault().getPreferenceStore().removePropertyChangeListener( this );
  }

  private void unhookProvider( )
  {
    if( m_provider != null )
    {
      m_provider.removeProfilProviderListener( this );
      m_provider = null;
    }

  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {

    final IContextService contextService = (IContextService) getSite().getService( IContextService.class );
    if( contextService != null )
      contextService.activateContext( "org.kalypso.model.wspm.ui.view.table.swt.context" );

    m_control = new Composite( parent, SWT.NONE );
    final GridLayout gridLayout = new GridLayout();
    gridLayout.marginHeight = 0;
    gridLayout.marginWidth = 0;
    m_control.setLayout( gridLayout );

    updateControl();
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#setFocus()
   */
  @Override
  public void setFocus( )
  {
    if( m_view != null )
    {
      final Control control = m_view.getControl();
      if( (control != null) && !control.isDisposed() )
        control.setFocus();
    }
  }

  protected void updateControl( )
  {
    if( (m_control == null) || m_control.isDisposed() )
      return;

    final Control[] childcontrols = m_control.getChildren();
    for( final Control c : childcontrols )
      c.dispose();

    unregisterGlobalActions();

    final IProfilEventManager pem = m_provider == null ? null : m_provider.getEventManager();
    final ProfilViewData pvd = m_provider == null ? null : m_provider.getViewData();

    if( (pem == null) || (pvd == null) )
    {
      final Label label = new Label( m_control, SWT.BORDER );
      label.setLayoutData( new GridData( GridData.FILL_BOTH ) );
      label.setText( "Kein Profil geladen." );
    }
    else
    {
      m_view = new DefaultTableViewer( m_control, SWT.BORDER | SWT.MULTI | SWT.FULL_SELECTION );
      m_view.getTable().setHeaderVisible( true );
      m_view.getTable().setLinesVisible( true );

      m_view.getTable().setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );

      final List<IComponentUiHandler> myHandlers = new ArrayList<IComponentUiHandler>();

      final IProfil profil = getProfilEventManager().getProfil();
      final IComponent[] pointMarkerTypes = profil.getPointMarkerTypes();

      final IComponent[] components = profil.getPointProperties();
      for( final IComponent component : components )
      {
        /* marker?!? yes -> continue */
        if( ArrayUtils.contains( pointMarkerTypes, component ) )
          continue;

        final int spacing = 100 / components.length;

        final QName valueTypeName = component.getValueTypeName();

        // TODO: let a table-column-provider create the handlers (profile-type dependent)
        if( ComponentUiHandlerFactory.Q_DATE_TIME.equals( valueTypeName ) )
        {
          myHandlers.add( ComponentUiHandlerFactory.getHandler( component, true, true, true, component.getName(), SWT.NONE, 100, spacing, "%s", "%s", "" ) );
        }
        else if( ComponentUiHandlerFactory.Q_STRING.equals( valueTypeName ) )
        {
          myHandlers.add( ComponentUiHandlerFactory.getHandler( component, true, true, true, component.getName(), SWT.NONE, 100, spacing, "%s", "%s", "" ) );
        }
        else if( ComponentUiHandlerFactory.Q_INTEGER.equals( valueTypeName ) )
        {
          myHandlers.add( ComponentUiHandlerFactory.getHandler( component, true, true, true, component.getName(), SWT.NONE, 100, spacing, "%s", "%s", "" ) );
        }
        else if( ComponentUiHandlerFactory.Q_DECIMAL.equals( valueTypeName ) )
        {
          myHandlers.add( ComponentUiHandlerFactory.getHandler( component, true, true, true, component.getName(), SWT.RIGHT, 100, spacing, "%.03f", "null", "" ) );
        }
        else if( ComponentUiHandlerFactory.Q_DOUBLE.equals( valueTypeName ) )
        {
          myHandlers.add( ComponentUiHandlerFactory.getHandler( component, true, true, true, component.getName(), SWT.RIGHT, 100, spacing, "%.03f", "null", "" ) );
        }
        else
          myHandlers.add( ComponentUiHandlerFactory.getHandler( component, true, true, true, component.getName(), SWT.NONE, 100, spacing, "%s", "%s", "" ) );
      }

      m_tupleResultContentProvider = new TupleResultContentProvider( myHandlers.toArray( new IComponentUiHandler[] {} ) );
      m_tupleResultLabelProvider = new TupleResultLabelProvider( myHandlers.toArray( new IComponentUiHandler[] {} ) );

      m_view.setContentProvider( m_tupleResultContentProvider );
      m_view.setLabelProvider( m_tupleResultLabelProvider );
      m_view.setCellModifier( new TupleResultCellModifier( m_tupleResultContentProvider ) );

      final Feature[] obsFeatures = FeatureSelectionHelper.getAllFeaturesOfType( KalypsoCorePlugin.getDefault().getSelectionManager(), ObservationFeatureFactory.OM_OBSERVATION );
      if( obsFeatures.length > 0 )
      {
        final IProfil profile = ProfileFeatureFactory.toProfile( obsFeatures[0] );
        m_view.setInput( profile.getResult() );

        final TupleResult result = profile.getResult();
        result.remove( m_changedListener );
        result.addChangeListener( m_changedListener );

      }
      else
        m_view.setInput( null );

      registerGlobalActions( m_view );

      // FIXME
// getSite().registerContextMenu( m_view.getContextMenuManager(), m_view.getSelectionProvider() );
// getSite().setSelectionProvider( m_view.getSelectionProvider() );
    }

    m_control.layout();

  }

  private void registerGlobalActions( final DefaultTableViewer tableView )
  {
    // final IActionBars actionBars = getViewSite().getActionBars();

    // TODO: we can't do that, because then caopy/paste within a cell does not work any more
    // actionBars.setGlobalActionHandler( ActionFactory.COPY.getId(), tableView.getAction(
    // ProfilSWTTableView.ACTION_COPY ) );
    // actionBars.setGlobalActionHandler( ActionFactory.PASTE.getId(), tableView.getAction(
    // ProfilSWTTableView.ACTION_PASTE ) );
    // actionBars.setGlobalActionHandler( ActionFactory.DELETE.getId(), tableView.getAction(
    // ProfilSWTTableView.ACTION_DELETEPOINTS ) );
    // actionBars.setGlobalActionHandler( ActionFactory.SELECT_ALL.getId(), tableView.getAction(
    // ProfilSWTTableView.ACTION_SELECTALL ) );
    // actionBars.setGlobalActionHandler( ProfilchartEditorContributor.RETARGET_INSERT, tableView.getAction(
    // ProfilSWTTableView.ACTION_INSERTPOINT ) );

    // actionBars.updateActionBars();
  }

  private void unregisterGlobalActions( )
  {
// final IActionBars actionBars = getViewSite().getActionBars();

    // actionBars.setGlobalActionHandler( ActionFactory.COPY.getId(), null );
    // actionBars.setGlobalActionHandler( ActionFactory.PASTE.getId(), null );
    // actionBars.setGlobalActionHandler( ActionFactory.DELETE.getId(), null );
// actionBars.setGlobalActionHandler( ActionFactory.SELECT_ALL.getId(), null );
// actionBars.setGlobalActionHandler( ProfilchartEditorContributor.RETARGET_INSERT, null );
//
// actionBars.updateActionBars();
  }

  /**
   * @see org.eclipse.jface.util.IPropertyChangeListener#propertyChange(org.eclipse.jface.util.PropertyChangeEvent)
   */
  public void propertyChange( final PropertyChangeEvent event )
  {
    // FIXME wie spalten, die hinzukommen
  }

  /** Must be called in the swt thread */
  protected void updatePartNameAndControl( final ProfilchartEditor editor )
  {
    setPartName( editor.getPartName() );
    if( !m_control.isDisposed() ) // control may have been disposed in the meantime
      updateControl();
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.partlistener.IAdapterEater#setAdapter(java.lang.Object)
   */
  public void setAdapter( final IWorkbenchPart part, final Object adapter )
  {
    final IProfilProvider2 provider = (IProfilProvider2) adapter;

    if( (m_provider == provider) && (provider != null) )
      return;

    unhookProvider();

    m_provider = provider;

    if( m_provider != null )
      m_provider.addProfilProviderListener( this );

    final IProfilEventManager pem = m_provider == null ? null : m_provider.getEventManager();
    final ProfilViewData viewData = m_provider == null ? null : m_provider.getViewData();
    onProfilProviderChanged( m_provider, null, pem, null, viewData );
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.IProfilProviderListener#onProfilProviderChanged(org.kalypso.model.wspm.ui.profil.view.IProfilProvider2,
   *      org.kalypso.model.wspm.core.profil.IProfilEventManager,
   *      org.kalypso.model.wspm.core.profil.IProfilEventManager, org.kalypso.model.wspm.ui.profil.view.ProfilViewData,
   *      org.kalypso.model.wspm.ui.profil.view.ProfilViewData)
   */
  public void onProfilProviderChanged( final IProfilProvider2 provider, final IProfilEventManager oldPem, final IProfilEventManager newPem, final ProfilViewData oldViewData, final ProfilViewData newViewData )
  {
    // if( m_group != null )
    // {
    // m_group.dispose();
    // m_group = null;
    // }
    //
    // m_group = new UndoRedoActionGroup( getSite(), new ProfilUndoContext( getProfil() ), true );
    // final IActionBars actionBars = getViewSite().getActionBars();
    // m_group.fillActionBars( actionBars );
    //
    // if( m_control != null && !m_control.isDisposed() )
    // {
    // m_control.getDisplay().asyncExec( new Runnable()
    // {
    // public void run( )
    // {
    // actionBars.updateActionBars();
    // updatePartNameAndControl( editor );
    // }
    // } );
    // }

    if( m_pem != null )
      m_pem.removeProfilListener( m_profileListener );

    m_pem = newPem;

    if( m_pem != null )
      m_pem.addProfilListener( m_profileListener );

    if( (m_control != null) && !m_control.isDisposed() )
      m_control.getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          updateControl();
        }
      } );
  }

  public IProfilEventManager getProfilEventManager( )
  {
    return m_pem;
  }
}
