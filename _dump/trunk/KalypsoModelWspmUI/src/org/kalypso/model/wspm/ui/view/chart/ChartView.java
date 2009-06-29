/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.model.wspm.ui.view.chart;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.chart.ui.IChartPart;
import org.kalypso.chart.ui.editor.commandhandler.ChartHandlerUtilities;
import org.kalypso.chart.ui.editor.mousehandler.AxisDragHandlerDelegate;
import org.kalypso.chart.ui.editor.mousehandler.PlotDragHandlerDelegate;
import org.kalypso.contribs.eclipse.ui.partlistener.AdapterPartListener;
import org.kalypso.contribs.eclipse.ui.partlistener.EditorFirstAdapterFinder;
import org.kalypso.contribs.eclipse.ui.partlistener.IAdapterEater;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilListener;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.profil.IProfilProvider;
import org.kalypso.model.wspm.ui.profil.IProfilProviderListener;

import de.openali.odysseus.chart.framework.util.ChartUtilities;
import de.openali.odysseus.chart.framework.view.impl.ChartComposite;

/**
 * @author kimwerner
 */
public class ChartView extends ViewPart implements IChartPart, IProfilListener, IProfilProviderListener, IAdapterEater<IProfilProvider>
{
  public static final String ID = "org.kalypso.model.wspm.ui.view.chart.ChartView"; //$NON-NLS-1$

  private final AdapterPartListener<IProfilProvider> m_adapterPartListener = new AdapterPartListener<IProfilProvider>( IProfilProvider.class, this, new EditorFirstAdapterFinder<IProfilProvider>(), new EditorFirstAdapterFinder<IProfilProvider>() );

  private final UIJob m_updateProfilJob = new UIJob( Messages.getString( "org.kalypso.model.wspm.ui.view.AbstractProfilViewPart_0" ) ) //$NON-NLS-1$
  {
    @Override
    public IStatus runInUIThread( final IProgressMonitor monitor )
    {
      handleProfilChanged();
      return Status.OK_STATUS;
    }
  };

  private IProfil m_profile = null;

  private Composite m_control;

  private IProfilProvider m_provider;

  /** The part where the profile provider came from. */
  private IWorkbenchPart m_profilProviderPart = null;

  final private ProfilChartView m_chart = new ProfilChartView();

  private FormToolkit m_toolkit = null;

  private Form m_form = null;

  public ChartView( )
  {
    setLayerProvider( m_chart );
  }
  
  @Override
  public void init( final IViewSite site ) throws PartInitException
  {
    super.init( site );

    final IWorkbenchPage page = site.getPage();
    m_adapterPartListener.init( page );
  }

  public IProfil getProfil( )
  {
    return m_profile;
  }

  public void setAdapter( final IWorkbenchPart part, final IProfilProvider adapter )
  {
    if( adapter == m_provider )
    {
      // for first initialization, provider empty profile
      if( adapter == null )
        onProfilProviderChanged( null, null, null );
      return;
    }

    if( m_provider != null )
    {
      m_provider.removeProfilProviderListener( this );
      m_provider = null;
    }

    final IProfil oldProfile = m_profile;

    m_provider = adapter;
    m_profilProviderPart = part;

    if( m_provider != null )
      m_provider.addProfilProviderListener( this );

    final IProfil newProfile = m_provider == null ? null : m_provider.getProfil();
    onProfilProviderChanged( m_provider, oldProfile, newProfile );
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public final void createPartControl( final Composite parent )
  {
    m_control = new Composite( parent, SWT.NONE );
    final GridLayout gridLayout = new GridLayout();
    gridLayout.marginHeight = 0;
    gridLayout.marginWidth = 0;
    m_control.setLayout( gridLayout );

    onProfilProviderChanged( m_provider, null, m_profile );
  }

  /**
   * @see com.bce.profil.ui.view.IProfilProviderListener#onProfilProviderChanged(com.bce.eind.core.profil.IProfilEventManager,
   *      com.bce.eind.core.profil.IProfilEventManager, com.bce.profil.ui.view.ProfilViewData,
   *      com.bce.profil.ui.view.ProfilViewData)
   */
  public void onProfilProviderChanged( final IProfilProvider provider, final IProfil oldProfile, final IProfil newProfile )
  {
    setPartNames( Messages.getString( "org.kalypso.model.wspm.ui.view.AbstractProfilViewPart_1" ), Messages.getString( "org.kalypso.model.wspm.ui.view.AbstractProfilViewPart_2" ) ); //$NON-NLS-1$ //$NON-NLS-2$

    m_profile = newProfile;

    onProfilChanged();
  }

  /** Returns the part which provides the current profile. */
  public IWorkbenchPart getProfileProviderPart( )
  {
    return m_profilProviderPart;
  }

  private void setPartNames( final String partName, final String tooltip )
  {
    final Composite control = getControl();
    if( control != null && !control.isDisposed() )
    {
      final Runnable object = new Runnable()
      {
        public void run( )
        {
          if( !control.isDisposed() )
            setPartNamesInternal( partName, tooltip );
        }
      };
      control.getDisplay().asyncExec( object );
    }
  }

  protected void setPartNamesInternal( final String partName, final String tooltip )
  {
    setTitleToolTip( tooltip );
    setPartName( partName );
  }

  @Override
  public void setFocus( )
  {
    m_control.setFocus();
  }

  /** Recreates the control */
  private final void onProfilChanged( )
  {
    // REMARK: this makes this code a bit more robust, if too many profil changes go in...
    // we quickly cancel still pending jobs and reschedule again (with a small schedule)
    m_updateProfilJob.cancel();
    m_updateProfilJob.schedule( 100 );
  }

  protected Composite getControl( )
  {
    return m_control;
  }

  /** Used internally. Must be called in the SWT-Thread. */
  protected void handleProfilChanged( )
  {
    if( m_control == null || m_control.isDisposed() )
      return;

    final String partName = m_profile == null ? Messages.getString( "org.kalypso.model.wspm.ui.view.AbstractProfilViewPart_4" ) : Messages.getFormatString( "org.kalypso.model.wspm.ui.view.AbstractProfilViewPart_3", m_profile.getStation() ); //$NON-NLS-1$ //$NON-NLS-2$
    final String tooltip = null;

    setPartNames( partName, tooltip );

    final Composite parent = getControl();
    if( parent == null || parent.isDisposed() )
      return;

    createContent( parent );
    parent.layout();
  }

  /**
   * @see com.bce.profil.eclipse.view.AbstractProfilViewPart2#createContent(org.eclipse.swt.widgets.Composite)
   */
  protected Control createContent( final Composite parent )
  {
    if( m_toolkit == null )
      m_toolkit = new FormToolkit( parent.getDisplay() );

    if( m_form == null )
    {
      m_form = m_toolkit.createForm( parent );
      m_form.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
      final GridLayout gridLayout = new GridLayout();
      gridLayout.marginWidth = 0;
      gridLayout.marginHeight = 0;
      m_form.setLayout( gridLayout );
      m_form.getBody().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
      m_form.getBody().setLayout( new GridLayout() );
      m_toolkit.decorateFormHeading( m_form );
    }
    if( m_chart.getChartComposite() == null )
    {
      m_chart.createControl( m_form.getBody() );
    }
    if( getProfil() == null )
    {
      m_form.setMessage( org.kalypso.model.wspm.ui.i18n.Messages.getString( "org.kalypso.model.wspm.ui.view.chart.ChartView.0" ), IMessageProvider.INFORMATION ); //$NON-NLS-1$
      m_chart.setProfil( null );
    }
    else
    {
      m_form.setMessage( null );
      m_chart.setProfil( getProfil() );
      
      final ChartComposite chart = m_chart.getChart();
      if( chart != null )
        ChartUtilities.maximize( chart.getChartModel() );
      
      m_form.getBody().layout();
    }

    return getChartComposite();
  }


  /**
   * Can be overridden if one wants to set the layer provider.
   * 
   * @param chart
   *          The profile chart view.
   */
  @SuppressWarnings("unused")
  protected void setLayerProvider( final ProfilChartView chart )
  {
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.AbstractProfilViewPart#dispose()
   */
  @Override
  public void dispose( )
  {
    if( m_form != null && !m_form.isDisposed() )
    {
      m_form.dispose();
      m_form = null;
    }
    if( m_toolkit != null )
    {
      m_toolkit.dispose();
      m_toolkit = null;
    }

    if( m_chart != null )
    {
      ChartHandlerUtilities.updateElements( m_chart );
      m_chart.dispose();
    }
    
    m_adapterPartListener.dispose();
    m_control.dispose();

    m_profilProviderPart = null;

    super.dispose();
  }

  /**
   * @see com.bce.eind.core.profil.IProfilListener#onProfilChanged(com.bce.eind.core.profil.changes.ProfilChangeHint,
   *      com.bce.eind.core.profil.IProfilChange[])
   */
  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
    if( hint.isPointPropertiesChanged() )
      m_chart.updateLayer();
  }

  public ChartComposite getChart( )
  {
    return m_chart.getChartComposite();
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilListener#onProblemMarkerChanged(org.kalypso.model.wspm.core.profil.IProfil)
   */
  public void onProblemMarkerChanged( final IProfil source )
  {
    //
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#getAdapter(java.lang.Class)
   */
  @SuppressWarnings("unchecked")
  @Override
  public Object getAdapter( final Class adapter )
  {
    if( ChartView.class.equals( adapter ) )
      return this;

    if( IChartPart.class.equals( adapter ) )
      return m_chart;

    return super.getAdapter( adapter );
  }

  /**
   * @see org.kalypso.chart.ui.IChartPart#getAxisDragHandler()
   */
  public AxisDragHandlerDelegate getAxisDragHandler( )
  {
    if( m_chart == null )
      return null;
    return m_chart.getAxisDragHandler();
  }

  /**
   * @see org.kalypso.chart.ui.IChartPart#getChartComposite()
   */
  public ChartComposite getChartComposite( )
  {
    if( m_chart == null )
      return null;
    return m_chart.getChartComposite();
  }

  /**
   * @see org.kalypso.chart.ui.IChartPart#getPlotDragHandler()
   */
  public PlotDragHandlerDelegate getPlotDragHandler( )
  {
    if( m_chart == null )
      return null;
    return m_chart.getPlotDragHandler();
  }

  public ProfilChartView getProfilChartView( )
  {
    return m_chart;
  }

}