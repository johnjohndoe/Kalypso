/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.ui.rrm.internal.timeseries.view.filter;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Hyperlink;
import org.kalypso.contribs.eclipse.swt.layout.Layouts;
import org.kalypso.contribs.eclipse.ui.forms.ToolkitUtils;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * @author Dirk Kuch
 */
public class TimeseriesBrowserSearchViewer extends Composite
{
  private final ParameterTypeFilterControl m_parameterTypeFilterControl;

  private final TextSearchFilterControl m_textSearchControl;

  private String m_parameterType;

  public TimeseriesBrowserSearchViewer( final Composite parent, final FormToolkit toolkit, final TreeViewer viewer )
  {
    super( parent, SWT.NONE );

    GridLayoutFactory.fillDefaults().numColumns( 2 ).equalWidth( true ).extendedMargins( 0, 0, 0, 5 ).applyTo( this );

    final Group groupTextSearch = new Group( this, SWT.NONE );
    ToolkitUtils.adapt( toolkit, groupTextSearch );
    groupTextSearch.setLayout( new FillLayout() );
    groupTextSearch.setText( Messages.getString("TimeseriesBrowserSearchViewer_0") ); //$NON-NLS-1$
    groupTextSearch.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    m_textSearchControl = new TextSearchFilterControl( groupTextSearch, toolkit );
    m_textSearchControl.setViewer( viewer );

    final Group groupParameter = new Group( this, SWT.NONE );
    ToolkitUtils.adapt( toolkit, groupParameter );
    groupParameter.setLayout( new FillLayout() );
    groupParameter.setText( Messages.getString("TimeseriesBrowserSearchViewer_1") ); //$NON-NLS-1$
    groupParameter.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    m_parameterTypeFilterControl = new ParameterTypeFilterControl( groupParameter, toolkit );
    m_parameterTypeFilterControl.setViewer( viewer );

    final Composite control = toolkit.createComposite( this );
    control.setLayout( Layouts.createGridLayout( 2 ) );
    control.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false, 2, 0 ) );

    final Hyperlink expand = toolkit.createHyperlink( control, Messages.getString("TimeseriesBrowserSearchViewer_2"), SWT.NULL ); //$NON-NLS-1$
    expand.setLayoutData( new GridData( GridData.FILL, GridData.FILL, false, false ) );

    expand.addHyperlinkListener( new HyperlinkAdapter()
    {
      @Override
      public void linkActivated( final org.eclipse.ui.forms.events.HyperlinkEvent e )
      {
        viewer.expandAll();
      }
    } );

    final Hyperlink collapse = toolkit.createHyperlink( control, Messages.getString("TimeseriesBrowserSearchViewer_3"), SWT.NULL ); //$NON-NLS-1$
    collapse.setLayoutData( new GridData( GridData.FILL, GridData.FILL, false, false ) );

    collapse.addHyperlinkListener( new HyperlinkAdapter()
    {
      @Override
      public void linkActivated( final org.eclipse.ui.forms.events.HyperlinkEvent e )
      {
        viewer.collapseAll();
      }
    } );
  }

  public void doClean( )
  {
    m_textSearchControl.reset();

    if( StringUtils.isEmpty( m_parameterType ) )
      m_parameterTypeFilterControl.reset();
  }

  public void setParameterType( final String parameterType )
  {
    m_parameterType = parameterType;
    m_parameterTypeFilterControl.setParameterType( parameterType );
  }
}