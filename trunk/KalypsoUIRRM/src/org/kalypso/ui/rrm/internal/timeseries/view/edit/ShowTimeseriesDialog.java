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
package org.kalypso.ui.rrm.internal.timeseries.view.edit;

import java.net.URL;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.services.IServiceLocator;
import org.kalypso.contribs.eclipse.jface.dialog.EnhancedTrayDialog;
import org.kalypso.contribs.eclipse.ui.forms.ToolkitUtils;
import org.kalypso.ogc.sensor.TIMESERIES_TYPE;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.zml.core.base.IMultipleZmlSourceElement;

/**
 * @author Dirk Kuch
 */
public class ShowTimeseriesDialog extends EnhancedTrayDialog
{
  private static final String DIALOG_SCREEN_SIZE = "show.time.series.dialog.screen.size"; //$NON-NLS-1$

  private static final String DIALOG_SASH_FORM_WEIGHTS = "show.time.series.dialog.weights"; //$NON-NLS-1$

  private final IServiceLocator m_context;

  private RrmTableComposite m_table;

  private TimeseriesChartComposite m_chart;

  private final IMultipleZmlSourceElement m_source;

  public ShowTimeseriesDialog( final Shell shell, final IMultipleZmlSourceElement source, final IServiceLocator context )
  {
    super( shell );

    m_source = source;
    m_context = context;

    setShellStyle( SWT.CLOSE | SWT.MAX | SWT.TITLE | SWT.BORDER | SWT.APPLICATION_MODAL | SWT.RESIZE );
    setHelpAvailable( false );
    setDialogHelpAvailable( false );
  }

  @Override
  protected Control createDialogArea( final Composite parent )
  {
    final FormToolkit toolkit = ToolkitUtils.createToolkit( parent );
    getShell().setText( Messages.getString( "EditTimeseriesDialog_0" ) ); //$NON-NLS-1$

    final Composite base = toolkit.createComposite( parent, SWT.NULL );
    GridLayoutFactory.swtDefaults().applyTo( base );

    final Point screen = getScreenSize( DIALOG_SCREEN_SIZE );

    final GridData data = new GridData( GridData.FILL, GridData.FILL, true, true );
    data.widthHint = screen.x;
    data.heightHint = screen.y;
    base.setLayoutData( data );

    base.addControlListener( new ControlAdapter()
    {
      @Override
      public void controlResized( final ControlEvent e )
      {
        setScreenSize( DIALOG_SCREEN_SIZE, base.getSize() );
      }
    } );

    /* first row */
    final SashForm form = new SashForm( base, SWT.HORIZONTAL );
    form.setLayout( new FillLayout() );
    form.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true, 2, 0 ) );

    final Composite leftPane = toolkit.createComposite( form );
    GridLayoutFactory.swtDefaults().applyTo( leftPane );

    final Composite rightPane = toolkit.createComposite( form );
    GridLayoutFactory.swtDefaults().applyTo( rightPane );

    m_chart = new TimeseriesChartComposite( leftPane, toolkit, m_context, getClass().getResource( "/etc/timeseries/diagram.kod" ) ); //$NON-NLS-1$
    m_chart.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    m_chart.setSelection( m_source );

    m_table = new RrmTableComposite( rightPane, toolkit, getTableTemplate(), m_context );
    m_table.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    m_table.setSelection( m_source );

    m_chart.addControlListener( new ControlAdapter()
    {
      @Override
      public void controlResized( final ControlEvent e )
      {
        setWeights( form.getWeights() );
      }
    } );

    form.setWeights( getWeights() );
    toolkit.adapt( form );

    return base;
  }

  private URL getTableTemplate( )
  {
    final TIMESERIES_TYPE type = TIMESERIES_TYPE.getType( m_source.getType() );
    switch( type )
    {
      case eSumValue:
        return getClass().getResource( "/etc/timeseries/table.sum.kot" ); //$NON-NLS-1$
      default:
        return getClass().getResource( "/etc/timeseries/table.kot" ); //$NON-NLS-1$
    }
  }

  private int[] getWeights( )
  {
    final IDialogSettings settings = KalypsoUIRRMPlugin.getDefault().getDialogSettings();

    final String weights = settings.get( DIALOG_SASH_FORM_WEIGHTS );
    if( weights == null || weights.trim().isEmpty() )
      return new int[] { 64, 40 };

    final String[] parts = weights.split( "," ); //$NON-NLS-1$ //$NON-NLS-1$
    final int[] w = new int[parts.length];

    for( int i = 0; i < parts.length; i++ )
    {
      w[i] = Integer.valueOf( parts[i] );
    }

    return w;
  }

  protected void setWeights( final int[] weights )
  {
    final StringBuffer buffer = new StringBuffer();
    for( final int weight : weights )
    {
      buffer.append( String.format( Messages.getString( "EditTimeseriesDialog_3" ), weight ) ); //$NON-NLS-1$ //$NON-NLS-1$
    }

    final IDialogSettings settings = KalypsoUIRRMPlugin.getDefault().getDialogSettings();
    settings.put( DIALOG_SASH_FORM_WEIGHTS, StringUtils.chop( buffer.toString() ) );
  }

  @Override
  protected void buttonPressed( final int buttonId )
  {
    m_table.deactivate();
    m_chart.deactivate();

    super.buttonPressed( buttonId );
  }
}