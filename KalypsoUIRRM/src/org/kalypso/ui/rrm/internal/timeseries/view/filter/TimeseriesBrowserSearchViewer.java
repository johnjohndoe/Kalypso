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
package org.kalypso.ui.rrm.internal.timeseries.view.filter;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.kalypso.contribs.eclipse.jface.action.ActionHyperlink;
import org.kalypso.contribs.eclipse.jface.viewers.tree.CollapseAllTreeItemsAction;
import org.kalypso.contribs.eclipse.jface.viewers.tree.ExpandAllTreeItemsAction;
import org.kalypso.contribs.eclipse.ui.forms.ToolkitUtils;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.results.view.base.IHydrologyResultReference;
import org.kalypso.ui.rrm.internal.results.view.tree.filter.IRrmDiagramFilterControl;

/**
 * @author Dirk Kuch
 */
public class TimeseriesBrowserSearchViewer extends Composite implements IRrmDiagramFilterControl
{
  private final TimeseriesBrowserParameterTypeFilterControl m_parameterTypeFilterControl;

  private final TimeseriesBrowserTextSearchFilterControl m_textSearchControl;

  private String m_parameterType;

  public TimeseriesBrowserSearchViewer( final Composite parent, final FormToolkit toolkit, final TreeViewer viewer )
  {
    super( parent, SWT.NONE );

    GridLayoutFactory.fillDefaults().numColumns( 2 ).equalWidth( true ).extendedMargins( 0, 0, 0, 5 ).applyTo( this );

    final Group groupTextSearch = new Group( this, SWT.NONE );
    ToolkitUtils.adapt( toolkit, groupTextSearch );
    groupTextSearch.setLayout( new FillLayout() );
    groupTextSearch.setText( Messages.getString( "TimeseriesBrowserSearchViewer_0" ) ); //$NON-NLS-1$
    groupTextSearch.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    m_textSearchControl = new TimeseriesBrowserTextSearchFilterControl( groupTextSearch, toolkit );
    m_textSearchControl.setViewer( viewer );

    final Group groupParameter = new Group( this, SWT.NONE );
    ToolkitUtils.adapt( toolkit, groupParameter );
    groupParameter.setLayout( new FillLayout() );
    groupParameter.setText( Messages.getString( "TimeseriesBrowserSearchViewer_1" ) ); //$NON-NLS-1$
    groupParameter.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    m_parameterTypeFilterControl = new TimeseriesBrowserParameterTypeFilterControl( groupParameter, toolkit );
    m_parameterTypeFilterControl.setViewer( viewer );

    final Composite control = toolkit.createComposite( this );
    control.setLayout( GridLayoutFactory.fillDefaults().numColumns( 2 ).create() );
    control.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false, 2, 0 ) );

    final ImageHyperlink lnkExpandAll = ActionHyperlink.createHyperlink( toolkit, control, getStyle(), new ExpandAllTreeItemsAction( viewer ) );
    lnkExpandAll.setText( Messages.getString( "TimeseriesBrowserSearchViewer_2" ) ); //$NON-NLS-1$
    lnkExpandAll.setLayoutData( new GridData( GridData.FILL, GridData.FILL, false, false ) );

    final ImageHyperlink lnkCollapseAll = ActionHyperlink.createHyperlink( toolkit, control, getStyle(), new CollapseAllTreeItemsAction( viewer ) );
    lnkCollapseAll.setText( Messages.getString( "TimeseriesBrowserSearchViewer_3" ) ); //$NON-NLS-1$
    lnkCollapseAll.setLayoutData( new GridData( GridData.FILL, GridData.FILL, false, false ) );
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

  @Override
  public boolean doSelect( final IHydrologyResultReference reference )
  {
    return false;
  }

  @Override
  public boolean doSelect( final String parameterType )
  {
    return m_parameterTypeFilterControl.doSelect( parameterType );
  }

}