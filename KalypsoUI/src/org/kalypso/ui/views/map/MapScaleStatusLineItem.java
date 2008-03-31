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
package org.kalypso.ui.views.map;

import java.awt.Point;
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
import org.kalypso.ogc.gml.map.MapPanel;
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
public class MapScaleStatusLineItem extends WorkbenchWindowControlContribution implements IAdapterEater<MapPanel>, IMapPanelListener
{
  private IAdapterFinder<MapPanel> m_closeFinder;

  private IAdapterFinder<MapPanel> m_initFinder;

  protected AdapterPartListener<MapPanel> m_adapterListener;

  /**
   * The main composite.
   */
  private Composite m_composite;

  /**
   * The map panel.
   */
  protected MapPanel m_panel;

  /**
   * The text field for displaying and typing the scale.
   */
  protected Text m_text;

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
  public MapScaleStatusLineItem( String id )
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
    m_closeFinder = new EditorFirstAdapterFinder<MapPanel>();
    m_initFinder = m_closeFinder;
    m_adapterListener = new AdapterPartListener<MapPanel>( MapPanel.class, this, m_initFinder, m_closeFinder );

    m_text = null;
  }

  /**
   * @see org.eclipse.jface.action.ControlContribution#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createControl( Composite parent )
  {
    /* The main composite */
    m_composite = new Composite( parent, SWT.NONE );
    GridLayout gridLayout = new GridLayout( 2, false );
    gridLayout.marginBottom = 0;
    gridLayout.marginHeight = 0;
    m_composite.setLayout( gridLayout );

    /* Create the components. */

    /* Create the label. */
    Label label = new Label( m_composite, SWT.NONE );
    label.setLayoutData( new GridData( SWT.END, SWT.CENTER, false, true ) );
    label.setText( "Scale 1 : " );

    /* Create the text. */
    m_text = new Text( m_composite, SWT.BORDER );
    GridData gridData = new GridData( SWT.FILL, SWT.CENTER, true, true );
    gridData.widthHint = 75;
    m_text.setLayoutData( gridData );
    m_text.setText( "" );

    /* Add the selection listener. */
    m_text.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetDefaultSelected( SelectionEvent e )
      {
        Text source = (Text) e.getSource();

        try
        {
          /* Get the contained text. */
          String text = source.getText();

          /* Parse the text. It must be a double. */
          double scale = Double.parseDouble( text.replace( ",", "." ) );

          /* Set the map scale. */
          MapUtilities.setMapScale( m_panel, scale );
        }
        catch( NumberFormatException ex )
        {
          /* Tell the user. */
          ErrorDialog.openError( source.getShell(), "Maßstab", "Ungültiger Maßstab angegeben. Achten Sie darauf, dass Sie eine gültige Zahl angegeben haben.", StatusUtilities.statusFromThrowable( ex ) );
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
    IWorkbenchPage activePage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
    if( activePage != null )
      m_adapterListener.init( activePage );

    return m_composite;
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.partlistener.IAdapterEater#setAdapter(org.eclipse.ui.IWorkbenchPart,
   *      java.lang.Object)
   */
  public void setAdapter( IWorkbenchPart part, MapPanel adapter )
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
        double mapScale = MapUtilities.getMapScale( m_panel );
        if( Double.isNaN( mapScale ) || Double.isInfinite( mapScale ) )
          mapScale = 0;

        BigDecimal bigScale = new BigDecimal( mapScale, new MathContext( 3, RoundingMode.HALF_UP ) );
        String scaleString = bigScale.toPlainString();

        m_text.setText( scaleString );
      }
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.listeners.IMapPanelListener#onExtentChanged(org.kalypso.ogc.gml.map.MapPanel,
   *      org.kalypsodeegree.model.geometry.GM_Envelope, org.kalypsodeegree.model.geometry.GM_Envelope)
   */
  public void onExtentChanged( final MapPanel source, GM_Envelope oldExtent, GM_Envelope newExtent )
  {
    if( m_text != null && !m_text.isDisposed() )
    {
      final UIJob job = new UIJob( "Updating scale box ..." )
      {
        @Override
        public IStatus runInUIThread( IProgressMonitor monitor )
        {
          if( m_text != null && !m_text.isDisposed() )
          {
            double mapScale = MapUtilities.getMapScale( source );
            if( Double.isNaN( mapScale ) || Double.isInfinite( mapScale ) )
              mapScale = 0;

            BigDecimal bigScale = new BigDecimal( mapScale, new MathContext( 3, RoundingMode.HALF_UP ) );
            String scaleString = bigScale.toPlainString();

            m_text.setText( scaleString );
          }

          return Status.OK_STATUS;
        }
      };

      job.schedule();
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.listeners.IMapPanelListener#onMapModelChanged(org.kalypso.ogc.gml.map.MapPanel,
   *      org.kalypso.ogc.gml.mapmodel.IMapModell, org.kalypso.ogc.gml.mapmodel.IMapModell)
   */
  public void onMapModelChanged( MapPanel source, IMapModell oldModel, IMapModell newModel )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.map.listeners.IMapPanelListener#onMessageChanged(org.kalypso.ogc.gml.map.MapPanel,
   *      java.lang.String)
   */
  public void onMessageChanged( MapPanel source, String message )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.map.listeners.IMapPanelListener#onMouseMoveEvent(org.kalypso.ogc.gml.map.MapPanel,
   *      org.kalypsodeegree.model.geometry.GM_Point, java.awt.Point)
   */
  public void onMouseMoveEvent( MapPanel source, GM_Point gmPoint, Point mousePosition )
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