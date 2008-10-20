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
package org.kalypso.ui.views.map;

import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.menus.WorkbenchWindowControlContribution;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.partlistener.AdapterPartListener;
import org.kalypso.contribs.eclipse.ui.partlistener.EditorFirstAdapterFinder;
import org.kalypso.contribs.eclipse.ui.partlistener.IAdapterEater;
import org.kalypso.contribs.eclipse.ui.partlistener.IAdapterFinder;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.listeners.IMapPanelListener;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * This class dipslays a small bar which enables the user to change the scale.
 * 
 * @author Holger Albert
 */
public class MapScaleStatusLineItem extends WorkbenchWindowControlContribution implements IAdapterEater<IMapPanel>, IMapPanelListener
{
  private class UpdateScaleJob extends UIJob
  {
    private double m_mapScale;

    public UpdateScaleJob( final String name )
    {
      super( name );
    }

    public void setMapScale(final double mapScale  )
    {
      m_mapScale = mapScale;
    }

    @Override
    public IStatus runInUIThread( final IProgressMonitor monitor )
    {
      if( m_text != null && !m_text.isDisposed() )
      {
        if( Double.isNaN( m_mapScale ) || Double.isInfinite( m_mapScale ) )
          m_mapScale = 0;

        final BigDecimal bigScale = new BigDecimal( m_mapScale, new MathContext( 3, RoundingMode.HALF_UP ) );
        final String scaleString = bigScale.toPlainString();

        m_text.setText( scaleString );
      }

      return Status.OK_STATUS;
    }
  }

  private IAdapterFinder<IMapPanel> m_closeFinder;

  private IAdapterFinder<IMapPanel> m_initFinder;

  protected AdapterPartListener<IMapPanel> m_adapterListener;

  /**
   * The main composite.
   */
  private Composite m_composite;

  /**
   * The map panel.
   */
  protected IMapPanel m_panel;

  /**
   * The text field for displaying and typing the scale.
   */
  protected Text m_text;

  private UpdateScaleJob m_updateScaleJob;

  /**
   * The constructor.
   */
  public MapScaleStatusLineItem( )
  {
    /* Init everything. */
    init();
  }

  /**
   * The constructor.
   * 
   * @param The
   *            id of this contribution.
   */
  public MapScaleStatusLineItem( final String id )
  {
    super( id );

    /* Init everything. */
    init();
  }

  /**
   * This function initializes the class.
   */
  private void init( )
  {
    m_closeFinder = new EditorFirstAdapterFinder<IMapPanel>();
    m_initFinder = m_closeFinder;
    m_adapterListener = new AdapterPartListener<IMapPanel>( IMapPanel.class, this, m_initFinder, m_closeFinder );

    m_text = null;

    m_updateScaleJob = new UpdateScaleJob(Messages.getString("org.kalypso.ui.views.map.MapScaleStatusLineItem.0")); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.jface.action.ControlContribution#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createControl( final Composite parent )
  {
    /* The main composite */
    m_composite = new Composite( parent, SWT.NONE );
    final GridLayout gridLayout = new GridLayout( 2, false );
    gridLayout.marginBottom = 0;
    gridLayout.marginHeight = 0;
    m_composite.setLayout( gridLayout );

    /* Create the components. */

    /* Create the label. */
    final Label label = new Label( m_composite, SWT.NONE );
    label.setLayoutData( new GridData( SWT.END, SWT.CENTER, false, true ) );
    label.setText( Messages.getString("org.kalypso.ui.views.map.MapScaleStatusLineItem.1") ); //$NON-NLS-1$

    /* Create the text. */
    m_text = new Text( m_composite, SWT.BORDER );
    final GridData gridData = new GridData( SWT.FILL, SWT.CENTER, true, true );
    gridData.widthHint = 75;
    m_text.setLayoutData( gridData );
    m_text.setText( "" ); //$NON-NLS-1$

    /* Add the selection listener. */
    m_text.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetDefaultSelected( final SelectionEvent e )
      {
        final Text source = (Text) e.getSource();

        try
        {
          /* Get the contained text. */
          final String text = source.getText();

          /* Parse the text. It must be a double. */
          final double scale = Double.parseDouble( text.replace( ",", "." ) ); //$NON-NLS-1$ //$NON-NLS-2$

          /* Set the map scale. */
          MapUtilities.setMapScale( m_panel, scale );
        }
        catch( final NumberFormatException ex )
        {
          /* Tell the user. */
          ErrorDialog.openError( source.getShell(), Messages.getString("org.kalypso.ui.views.map.MapScaleStatusLineItem.5"), Messages.getString("org.kalypso.ui.views.map.MapScaleStatusLineItem.6"), StatusUtilities.statusFromThrowable( ex ) ); //$NON-NLS-1$ //$NON-NLS-2$
        }
      }
    } );

    /* Add a dispose listener. */
    m_composite.addDisposeListener( new DisposeListener()
    {
      public void widgetDisposed( final DisposeEvent e )
      {
        /* Remove the adapter listener from the active page. */
        m_adapterListener.dispose();
      }
    } );

    /* Hook the adapter listener to the active page. */
    final IWorkbenchPage activePage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
    if( activePage != null )
      m_adapterListener.init( activePage );

    return m_composite;
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.partlistener.IAdapterEater#setAdapter(org.eclipse.ui.IWorkbenchPart,
   *      java.lang.Object)
   */
  public void setAdapter( final IWorkbenchPart part, final IMapPanel adapter )
  {
    if( !m_composite.isDisposed() )
      m_composite.setVisible( adapter != null );

    if( m_panel != null )
      m_panel.removeMapPanelListener( this );

    m_panel = adapter;

    if( m_panel != null )
    {
      m_panel.addMapPanelListener( this );

      if( m_text != null )
      {
        double mapScale = m_panel.getCurrentScale();
        if( Double.isNaN( mapScale ) || Double.isInfinite( mapScale ) )
          mapScale = 0;

        final BigDecimal bigScale = new BigDecimal( mapScale, new MathContext( 3, RoundingMode.HALF_UP ) );
        final String scaleString = bigScale.toPlainString();

        m_text.setText( scaleString );
      }
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.listeners.IMapPanelListener#onExtentChanged(org.kalypso.ogc.gml.map.IMapPanel,
   *      org.kalypsodeegree.model.geometry.GM_Envelope, org.kalypsodeegree.model.geometry.GM_Envelope)
   */
  public void onExtentChanged( final IMapPanel source, final GM_Envelope oldExtent, final GM_Envelope newExtent )
  {
    if( m_updateScaleJob != null )
      m_updateScaleJob.cancel();

    if( m_text != null && !m_text.isDisposed() )
    {
      final double mapScale = source.getCurrentScale();
      m_updateScaleJob.setMapScale( mapScale );
      m_updateScaleJob.schedule();
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.listeners.IMapPanelListener#onMapModelChanged(org.kalypso.ogc.gml.map.MapPanel,
   *      org.kalypso.ogc.gml.mapmodel.IMapModell, org.kalypso.ogc.gml.mapmodel.IMapModell)
   */
  public void onMapModelChanged( final IMapPanel source, final IMapModell oldModel, final IMapModell newModel )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.map.listeners.IMapPanelListener#onMessageChanged(org.kalypso.ogc.gml.map.MapPanel,
   *      java.lang.String)
   */
  public void onMessageChanged( final IMapPanel source, final String message )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.map.listeners.IMapPanelListener#onStatusChanged(org.kalypso.ogc.gml.map.IMapPanel)
   */
  public void onStatusChanged( final IMapPanel source )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.map.listeners.IMapPanelListener#onMouseMoveEvent(org.kalypso.ogc.gml.map.IMapPanel,
   *      org.kalypsodeegree.model.geometry.GM_Point, int, int)
   */
  public void onMouseMoveEvent( final IMapPanel source, final GM_Point gmPoint, final int mousex, final int mousey )
  {
  }

  /**
   * @see org.eclipse.jface.action.ContributionItem#dispose()
   */
  @Override
  public void dispose( )
  {
    m_adapterListener.dispose();

    super.dispose();
  }
}